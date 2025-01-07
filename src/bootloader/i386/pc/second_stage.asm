; second_stage.asm
org 0xF000

[BITS 16]
start:
    cli
    cld

    xor ax, ax
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov sp, 0x7C00

    movzx dx, dl
    mov [drive_number], dx

    call hello_second_stage

    call reset_disk
    test ah, ah
    jnz disk_error

    mov bx, [root_directory]
    call read_root_directory
    test ah, ah
    jnz disk_error

    mov si, [root_directory]
    mov di, kernel_file_name
    call find_file
    test al, al
    jz kernel_not_found

    push cx ; Save Found Cluster
    push ebx ; Save File Size

    call check_a20_gate
    test al, al
    jnz a20_ready

    call enable_a20_gate_via_bios
    call check_a20_gate
    test al, al
    jnz a20_ready

    call enable_a20_keyboard
    call check_a20_gate
    test al, al
    jnz a20_ready

    call enable_a20_fast
    call check_a20_gate
    test al, al
    jnz a20_error

a20_ready:
    jmp enable_unreal_mode

load_kernel:
    pop ebx ; Restore File Size
    pop ax ; Restore Found Cluster
    mov edi, [kernel_buffer]
    mov dl, [drive_number]
    call load_file
    test ah, ah
    jnz disk_error

jump_to_kernel:
    ; Load the GDT
    lgdt [gdt_description]

    mov bx, 0x0000
    mov es, bx
    ; Buffer offset
    mov bx, 0x5C00
    mov di, bx
    call load_memory_map

    ; Jump to the kernel
    ;enter protected mode (32 bit)
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    jmp dword 0x08:0x100000
    jmp $


;--------------------------------------------
; Disk reset function
; Input: DL = Drive number
; Output: AH = 0 if successful, non-zero otherwise
;--------------------------------------------
reset_disk:
    mov ah, 0x00             ; Reset drive function
    int 0x13                 ; Call BIOS
    ret


;--------------------------------------------
; Read Sectors LBA
; Input: DL = Drive, AX = LBA, CL = Count, BX = Buffer
; Output: AH = Status
;--------------------------------------------
read_sectors_lba:
    push bp                     ; Save base pointer
    mov bp, sp
    sub sp, 7                  ; Allocate space for local variables
                              ; -1(bp) = count
                              ; -2(bp) = drive
                              ; -4(bp) = buffer
                              ; -6(bp) = lba
                              ; -7(bp) = tmp

    push cx                     ; Save registers we'll modify
    push dx
    push si
    push di

    mov byte [bp-1], cl        ; Store count
    mov byte [bp-2], dl        ; Store drive
    mov word [bp-4], bx        ; Store buffer
    mov word [bp-6], ax        ; Store LBA

.read_loop:
    mov cl, [bp-1]             ; Check if we're done
    test cl, cl
    jz .done

    mov ax, [bp-6]             ; Load LBA for conversion
    call lba_to_chs            ; Convert to CHS (outputs in DH, CH, CL)

    mov [bp-7], cl             ; Temporarily store CL (sector)

    ; Calculate sectors to read until track end
    mov al, [bp-7]             ; Get back the sector number
    and al, 3Fh                ; Mask to get just sector bits
    neg al                     ; Convert to negative
    add al, byte [sectors_per_track] ; Add sectors per track + 1
    inc al

    ; Compare with remaining count
    cmp al, [bp-1]
    jbe .no_adjust_count
    mov al, [bp-1]             ; If more than needed, adjust count
.no_adjust_count:

    push ax                    ; Save sectors to read

    mov dl, [bp-2]             ; Setup parameters for CHS read
    mov bx, [bp-4]
    mov cl, [bp-7]             ; Restore original CL (sector)
    call read_sectors_chs

    pop cx                     ; Recover sectors read count in CL
    mov cl, al                 ; Get actual sectors read

    test ah, ah               ; Check for errors
    jnz .exit

    ; Update counters and pointers
    mov al, [bp-1]
    sub al, cl                ; Subtract sectors read
    mov [bp-1], al            ; Update remaining count

    movzx ax, cl              ; Multiply sectors read by 512 for buffer update
    push cx
    mov cx, 512
    mul cx
    pop cx
    add [bp-4], ax            ; Update buffer pointer

    movzx ax, cl              ; Add sectors read to LBA
    add [bp-6], ax

    jmp .read_loop

.done:
    xor ah, ah                ; Return success

.exit:
    pop di                    ; Restore registers
    pop si
    pop dx
    pop cx

    mov sp, bp                ; Cleanup stack frame
    pop bp
    ret


;--------------------------------------------
; Read Sectors CHS
; Input: DL = Drive, DH = Head, CH = Cylinder (Lower 8 bits), CL = Sector and Higher 2-bits of Cylinder, AL = Count, BX = Buffer
; Output: AH = Status AL = Count
;--------------------------------------------
read_sectors_chs:
    mov ah, 0x02             ; BIOS read sectors function
    int 0x13                 ; Call BIOS
    ret                      ; Return to caller

;--------------------------------------------
; LBA to CHS conversion
; Input: AX = LBA, Outputs: DH = Head, CH = Cylinder (Lower 8 bits), CL = Sector and Higher 2-bits of Cylinder
;--------------------------------------------
lba_to_chs:
    push ebp                 ; Save base pointer for potential debugging or stack tracing
    mov ebp, esp             ; Establish a new stack frame
    push ax                  ; Save the LBA value
    push bx                  ; Save BX register as it will be used temporarily

    xor dx, dx                 ; Clear DX
    mov cx, [sectors_per_track]  ; Load sectors per track from configurable data section for flexibility
    div cx                     ; AX / Sectors per track, AX = quotient (track number), DX = remainder
    inc dx                     ; Remainder contains sector number, Sectors start at 1, so add 1
    push dx                    ; Save sector number temporarily for later use

    xor dx, dx
    mov cx, [number_of_heads]    ; Load number of heads from configurable data section for flexibility
    div cx                     ; AX / Number of heads, AX = cylinder number, DX = head number
    mov ch, al                 ; CH = cylinder number

    ; Now handle the upper 2 bits of the cylinder and pack them into CL.
    ; CL holds the sector number in its lower 6 bits. We need to add the upper 2 bits of the cylinder to the high bits of CL.
    pop bx                   ; BL = sector number, restored from stack because it was saved earlier
    mov cl, bl               ; CL = sector number
    and cl, 00111111b        ; Keep only the lower 6 bits for the sector number
    shl ah, 6                ; Move the upper 2 bits of the cylinder into position
    or  cl, ah               ; Combine with CL (upper 2 bits of cylinder + sector number)
    mov dh, dl               ; DH = head number (from the second division)
    pop bx                   ; Restore BX register
    pop ax                   ; Restore AX register
    mov esp, ebp             ; Restore the original stack pointer
    pop ebp                  ; Restore the base pointer
    ret

;--------------------------------------------
; Cluster to Sector conversion
; Input: AX = Cluster number
; Output: AX = Sector number
;--------------------------------------------
cluster_to_sector:
    push bx
    push dx
    sub ax, 2                ; Subtract 2 to account for FAT12 cluster numbering
    mov bx, [sectors_per_cluster] ; Load sectors per cluster
    mul bx                   ; AX = (cluster - 2) * sectors_per_cluster
    add ax, 33               ; Add 33 to account for reserved and FAT area
    pop dx
    pop bx
    ret

;--------------------------------------------
; Read Root Directory
; Input: DL = Drive, BX = Buffer
; Output: AH = Status
;--------------------------------------------
read_root_directory:
    push cx             ; Save only CX

    mov ax, 19          ; LBA of root directory start
    mov cx, 14          ; Number of sectors to read (14 sectors for FAT12 root)
    call read_sectors_lba

    pop cx             ; Restore CX
    ret

;--------------------------------------------
; Find File in Root Directory
; Input: SI = Root Directory Buffer, DI = File Name String (11 chars)
; Output: AL = Status (1 = found, 0 = not found)
;         If found: CX = Starting Cluster, EBX = File Size (32-bit)
;--------------------------------------------
find_file:
    push si
    push di

    mov cx, 224               ; Number of root directory entries

.entry_loop:
    ; Check if we've hit the end of the directory (0x00)
    cmp byte [si], 0x00
    je .not_found

    ; Check if entry is deleted (0xE5)
    cmp byte [si], 0xE5
    je .next_entry

    ; Compare file name (11 characters)
    push si
    push di
    mov bx, 11                ; Counter for filename comparison

.compare_loop:
    mov al, [si]
    cmp al, [di]
    jne .compare_failed

    inc si
    inc di
    dec bx
    jnz .compare_loop

    ; If we get here, we found a match
    pop di
    pop si

    ; Get starting cluster (offset 0x1A)
    mov cx, [si + 0x1A]       ; Load starting cluster into CX

    ; Get file size (offset 0x1C)
    mov ebx, [si + 0x1C]

    mov al, 1                 ; Return success
    jmp .exit

.compare_failed:
    pop di
    pop si

.next_entry:
    add si, 32                ; Move to next directory entry
    loop .entry_loop

.not_found:
    xor al, al                ; Return 0 for not found
    xor cx, cx                ; Clear return values
    xor ebx, ebx

.exit:
    pop di
    pop si
    ret


;--------------------------------------------
; Load File
; Input: DL = Drive, AX = File Cluster, EBX = File Size, EDI = Destination
; Output: AH = Status (0 = success, else error)
;--------------------------------------------
load_file:
    push bp
    mov bp, sp
    sub sp, 11              ; Local variables:
                          ; -2(bp) = sectors to read
                          ; -4(bp) = file size in sectors
                          ; -6(bp) = current LBA
                          ; -10(bp) = destination pointer
                          ; -11(bp) = drive number

    push cx
    push esi
    push edi

    mov [bp-10], edi        ; Save destination pointer
    mov [bp-11], dl         ; Save drive number

    ; Calculate initial LBA from cluster
    call cluster_to_sector
    mov [bp-6], ax        ; Save LBA

    ; Calculate file size in sectors
    mov eax, ebx          ; File size to EAX
    mov ebx, 512          ; Divisor (bytes per sector)
    xor edx, edx          ; Clear high bits for division
    div ebx               ; EAX = sectors, EDX = remainder
    test edx, edx         ; Check if there's a remainder
    jz .no_round_up
    inc eax               ; Add one more sector if there's a remainder
.no_round_up:
    mov [bp-4], ax        ; Store sectors count

.read_loop:
    ; Check if we're done
    cmp word [bp-4], 0
    je .success

    ; Calculate sectors to read this iteration (min(remaining, 32))
    mov ax, [bp-4]
    cmp ax, 32
    jbe .use_remaining
    mov ax, 32
.use_remaining:
    mov [bp-2], ax        ; Save sectors to read

    ; Read sectors
    mov ax, [bp-6]        ; Load LBA
    mov cx, [bp-2]        ; Load sectors count
    mov bx, [disk_buffer]        ; disk_buffer address
    mov dl, [bp-11]       ; Drive number
    call read_sectors_lba
    test ah, ah           ; Check status
    jnz .exit             ; Exit if error

    ; Copy from disk buffer to destination
    movzx esi, word [disk_buffer]        ; Source: disk buffer
    mov edi, [bp-10]        ; Destination
    mov cx, [bp-2]        ; Number of sectors
    mov ax, 512
    mul cx               ; AX = sectors * 512
    mov cx, ax           ; CX = number of bytes to copy
    a32 rep movsb            ; Copy CX bytes from DS:SI to ES:DI

    ; Update pointers and counters
    movzx eax, word [bp-2]  ; Get sectors read
    mov ecx, 512
    mul ecx                 ; EAX = sectors * 512
    add [bp-10], eax         ; Update destination pointer

    mov ax, [bp-2]
    add [bp-6], ax          ; Update LBA
    sub [bp-4], ax          ; Decrease remaining sectors

    jmp .read_loop

.success:
    xor ah, ah              ; Return success status

.exit:
    pop edi
    pop esi
    pop cx
    mov sp, bp
    pop bp
    ret

;--------------------------------------------
; A20 Gate Enable Check
; Output: AL = 0 if A20 is enabled, non-zero otherwise
;--------------------------------------------
check_a20_gate:
    pushf
    push ds
    push es
    push di
    push si

    cli
    xor ax, ax
    mov es, ax
    not ax
    mov ds, ax
    mov di, 0x0500
    mov si, 0x0510
    mov al, byte [es:di]
    push ax
    mov al, byte [ds:si]
    push ax
    mov byte [es:di], 0
    mov byte [ds:si], 0xFF
    cmp byte [es:di], 0xFF
    pop ax
    mov byte [ds:si], al
    pop ax
    mov byte [es:di], al
    mov ax, 0
    je .exit
    mov ax, 1

.exit:
    pop si
    pop di
    pop es
    pop ds
    popf
    ret

;--------------------------------------------
; Enable A20 Gate via BIOS
; Output: AL = 1 if A20 enabled successfully, 0 if failed
;--------------------------------------------
enable_a20_gate_via_bios:
    push bx

    ; Check if supported (INT 15h, AX=2403h)
    mov ax, 0x2403
    int 0x15
    jc .failure
    test ah, ah            ; Check if high byte is zero
    jnz .failure

    ; Query current state (INT 15h, AX=2402h)
    mov ax, 0x2402
    int 0x15
    jc .failure           ; CF=1 means error
    test ah, ah           ; Check if high byte is zero
    jnz .failure
    test al, al           ; If AL=1, A20 is already enabled
    jnz .success

    ; Enable A20 (INT 15h, AX=2401h)
    mov ax, 0x2401
    int 0x15
    jc .failure           ; CF=1 means error
    test ah, ah           ; Check if high byte is zero
    jnz .failure

.success:
    mov al, 1
    jmp .exit

.failure:
    xor al, al

.exit:
    pop bx
    ret

;--------------------------------------------
; A20 Wait for Input Buffer Empty
; Modifies: AL
;--------------------------------------------
a20wait:
    push dx
    mov dx, 0x64
.loop:
    in al, dx
    test al, 2
    jnz .loop
    pop dx
    ret

;--------------------------------------------
; A20 Wait for Output Buffer Full
; Modifies: AL
;--------------------------------------------
a20wait2:
    push dx
    mov dx, 0x64
.loop:
    in al, dx
    test al, 1
    jz .loop
    pop dx
    ret

;--------------------------------------------
; Enable A20 Gate via Keyboard Controller
;--------------------------------------------
enable_a20_keyboard:
    push dx
    push ax

    ; Disable keyboard
    mov dx, 0x64
    mov al, 0xAD
    out dx, al

    call a20wait

    ; Send read command
    mov al, 0xD0
    out dx, al

    call a20wait2

    ; Read response
    mov dx, 0x60
    in al, dx
    push ax         ; Save keyboard controller output

    call a20wait

    ; Send write command
    mov dx, 0x64
    mov al, 0xD1
    out dx, al

    call a20wait

    ; Write modified value
    pop ax          ; Restore keyboard controller output
    or al, 2        ; Set A20 bit
    mov dx, 0x60
    out dx, al

    call a20wait

    ; Re-enable keyboard
    mov dx, 0x64
    mov al, 0xAE
    out dx, al

    call a20wait

    pop ax
    pop dx
    ret

;--------------------------------------------
; Enable A20 Gate via Fast A20 (Port 92h)
;--------------------------------------------
enable_a20_fast:
    push ax                 ; Save registers
    push dx

    mov dx, 0x92           ; System Control Port A
    in al, dx              ; Read current value
    or al, 2               ; Set bit 1 (A20 gate)
    out dx, al             ; Write back

    pop dx                 ; Restore registers
    pop ax
    ret


enable_unreal_mode:
    cli
    push ds
    push es
    lgdt [gdt_description] ; load gdt register

    mov  eax, cr0          ; switch to pmode by
    or al,1                ; set pmode bit
    mov  cr0, eax
    jmp 0x18:.pmode

[BITS 32]
.pmode:
    mov  bx, 0x10          ; select descriptor 2
    mov  ds, bx            ; 10h = 10000b
    mov  es, bx            ; 10h = 10000b

    and al,0xFE            ; back to realmode
    mov  cr0, eax          ; by toggling bit again
    jmp 0:.unreal

[BITS 16]
.unreal:
    pop es
    pop ds
    jmp load_kernel


;--------------------------------------------
; Load Memory Map
; Input: ES:DI points to the buffer to store the memory map
;--------------------------------------------
load_memory_map:
    xor ebx, ebx

    ; Push initial buffer address
    push di

    ; Push initial entries count
    mov eax, 0
    push eax

    .load_memory_map_loop:
    ; Increment pointer in buffer
    add di, 24

    ; Increment entries count
    pop eax
    inc eax
    push eax

    ; Set magic number
    mov edx, 0x534d4150

    ; Set number of bytes to read
    mov ecx, 24

    ; Read memory map
    mov eax, 0xe820
    int 0x15

    ; Disable interrupts (can be enabled by int 0x15)
    cli

    ; Exit if ebx is equal to zero (reading has ended)
    cmp ebx, 0
    jne .load_memory_map_loop

    ; Store entries count at the begin of the buffer
    pop eax
    pop di
    mov [di], eax

    ret

;--------------------------------------------
; String printing routine
; Input: DS:SI points to the null-terminated string
;--------------------------------------------
print_string:
    pusha                    ; Save registers
.print_next_char:
    lodsb                    ; Load next character into AL
    or al, al                ; Check if null terminator
    jz .done
    mov ah, 0x0E             ; BIOS teletype function
    int 0x10                 ; Print character
    jmp .print_next_char     ; Repeat
.done:
    popa                     ; Restore registers
    ret

print_space:
    push si
    mov si, space
    call print_string
    pop si
    ret

hello_second_stage:
    push si
    mov si, hello_second_stage_msg
    call print_string
    pop si
    ret

debug:
    push si
    mov si, debug_msg
    call print_string
    pop si
    ret

disk_error:
    mov si, disk_error_msg
    call print_string
    call print_ah_hex
    jmp halt

kernel_not_found:
    mov si, kernel_not_found_msg
    call print_string
    jmp halt

a20_error:
    mov si, a20_error_msg
    call print_string
    jmp halt

halt:
    hlt
    jmp halt

;--------------------------------------------
; Print AH Register in Hexadecimal
;--------------------------------------------
print_ah_hex:
    pusha                      ; Save all registers

    mov al, ah                 ; Copy AH to AL (we will work with AL for both nibbles)
    call print_hex_byte        ; Print the byte in AL

    popa                       ; Restore all registers
    ret

;--------------------------------------------
; Print Hexadecimal Byte
; Input: AL = byte to print
;--------------------------------------------
print_hex_byte:
    push ax                    ; Save AX

    ; Convert high nibble
    mov ah, al                 ; Copy AL to AH
    shr ah, 4                  ; Shift right to isolate the high nibble
    call print_hex_nibble      ; Print the high nibble

    ; Convert low nibble
    mov ah, al                 ; Restore AL to AH
    and ah, 0x0F               ; Mask to isolate the low nibble
    call print_hex_nibble      ; Print the low nibble

    pop ax                     ; Restore AX
    ret

;--------------------------------------------
; Print Hexadecimal Nibble
; Input: AH = nibble to print (0-15)
;--------------------------------------------
print_hex_nibble:
    add ah, '0'                ; Convert to ASCII (0-9)
    cmp ah, '9'                ; Check if it's greater than '9'
    jbe .output                ; If yes, it's already valid
    add ah, 7                  ; Convert to A-F for values 10-15

.output:
    push ax
    mov al, ah                 ; Move ASCII value to AL
    mov ah, 0x0E               ; BIOS teletype function
    int 0x10                   ; Print character
    pop ax
    ret


drive_number db 0x00
root_directory dw 0x7E00
kernel_buffer dd 0x100000
disk_buffer dw 0x7E00
hello_second_stage_msg db "Hello second stage!", 10, 13, 0
kernel_file_name db "KERNEL  BIN", 0
disk_error_msg db "Disk error!", 10, 13, 0
a20_error_msg db "A20 Gate is not available.", 0
kernel_not_found_msg db "KERNEL.BIN not found!", 0
debug_msg db "Debug", 10, 13, 0
space db " ", 0

sectors_per_track dw 18      ; Number of sectors per track
number_of_heads   dw 2       ; Number of heads
sectors_per_cluster dw 1     ; Number of sectors per cluster

gdt_description:
    dw gdt_end - gdt_begin - 1
    dd gdt_begin

gdt_begin:
; Null segment, reserved by CPU
gdt_null:
    dd 0x00000000
    dd 0x00000000

; Code segment 32 bit (0x08)
gdt_code_32:
    ; Segment limit (4 GiB)
    dw 0xFFFF

    ; Segment base address (16 bits)
    dw 0x0000

    ; Segment base address (8 bits)
    db 0x00

    ; 1 - present bit (1 for all valid sectors)
    ; 00 - privilege (ring level), 00 is the higest
    ; 1 - reserved
    ; 1 - excebutable bit, code can be excecuted here
    ; 0 - conforming bit, only kernel can execute code
    ; 1 - segment can be read
    ; 0 - access bit, default is zero
    db 10011010b

    ; 1 - granularity (0 = 1B block, 1 = 4 KiB block)
    ; 1 - size bit (0 = 16b protected mode, 1 = 32b protected mode)
    ; 00 - reserved
    ; 1111 - segment base address (4 bits)
    db 11001111b

    ; Segment base address (8 bits)
    db 0x00

; Data segment 32 bit (0x10)
gdt_data_32:
    ; Segment limit (4 GiB)
    dw 0xFFFF

    ; Segment base address (16 bits)
    dw 0x0000

    ; Segment base address (8 bits)
    db 0x00

    ; 1 - present bit (1 for all valid sectors)
    ; 00 - privilege (ring level), 00 is the higest
    ; 1 - reserved
    ; 0 - excebutable bit, code can't be excecuted here (because it's just data)
    ; 0 - conforming bit, only kernel can execute code
    ; 1 - segment can be read
    ; 0 - access bit, default is zero
    db 10010010b

    ; 1 - granularity (0 = 1B block, 1 = 4 KiB block)
    ; 1 - size bit (0 = 16b protected mode, 1 = 32b protected mode)
    ; 00 - reserved
    ; 1111 - segment base address (4 bits)
    db 11001111b

    ; Segment base address (8 bits)
    db 0x00
    ; code segment 16 bit (0x18)
gdt_code_16:
    ; Segment limit (4 GiB)
    dw 0xFFFF

    ; Segment base address (16 bits)
    dw 0x0000

    ; Segment base address (8 bits)
    db 0x00

    ; 1 - present bit (1 for all valid sectors)
    ; 00 - privilege (ring level), 00 is the higest
    ; 1 - reserved
    ; 1 - excebutable bit, code can be excecuted here
    ; 0 - conforming bit, only kernel can execute code
    ; 1 - segment can be read
    ; 0 - access bit, default is zero
    db 10011010b

    ; 1 - granularity (0 = 1B block, 1 = 4 KiB block)
    ; 1 - size bit (0 = 16b protected mode, 1 = 32b protected mode)
    ; 00 - reserved
    ; 1111 - segment base address (4 bits)
    db 00000001b

    ; Segment base address (8 bits)
    db 0x00
gdt_end:

dw 0xAA55

; verify that the second stage bootlader size not exceed 4KB
%if ($ - $$) >= 4096
    %error "Second stage bootloader exceeds 4KB!"
%endif