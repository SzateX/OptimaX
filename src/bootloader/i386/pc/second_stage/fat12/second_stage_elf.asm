; second_stage_elf.asm
%include "elf.asm"
%include "multiboot.asm"

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
    mov bp, sp
    sub sp, 13 ; Local variables:
                ; -1(bp) = drive number
                ; -3(bp) = kernel start cluster
                ; -7(bp) = kernel file size
                ; -9(bp) = elf entry point segment
                ; -13(bp) = elf wntry point

    movzx dx, dl
    mov [bp-1], dl ; Save drive number in local variable

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

    mov [bp-3], cx ; Save Found Cluster
    mov [bp-7], ebx ; Save File Size

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
.load_elf_header:
    mov ebx, 8192 ; Load first 8KiB of a file.
    mov ax, [bp-3]  ; Load Found Cluster
    mov edi, [elf_header_buffer] ; Load ELF header buffer address
    mov dl, [bp-1]
    call load_file
    test ah, ah
    jnz disk_error

.check_multiboot_signature:
; Look in 8KiB buffer for Multiboot signature (0x1BADB002) aligned to 4 bytes
    mov edi, [elf_header_buffer]
    mov ecx, 2048 ; 8KiB / 4 bytes
    mov eax, 0x1BADB002 ; Multiboot signature
    repne scasd
    jne kernel_not_found

    sub edi, 4 ; Adjust EDI to point to the signature
    mov eax, [edi] ; Load the signature into EAX
    add eax, [edi + 4] ; Load the flags into EBX
    add eax, [edi + 8] ; Load the checksum into ECX
    cmp eax, 0
    jne kernel_not_found ; If the sum is not zero, it's not a valid Multiboot header

.identify_elf:
    ; Check if the file is a valid ELF file
    mov esi, [elf_header_buffer]
    cmp dword [esi + Elf32_Ehdr.e_ident + Elf32_Ident.ei_mag], 0x464C457F ; Check for ELF magic number
    jne kernel_not_found

    ; Check if it's a 32-bit ELF file
    cmp byte [esi + Elf32_Ehdr.e_ident + Elf32_Ident.ei_class], 1 ; EI_CLASS
    jne kernel_not_found

    ; Check if it's little-endian
    cmp byte [esi + Elf32_Ehdr.e_ident + Elf32_Ident.ei_data], 1 ; EI_DATA
    jne kernel_not_found

    ; Check if it's version 1
    cmp byte [esi + Elf32_Ehdr.e_ident + Elf32_Ident.ei_version], 1 ; EI_VERSION
    jne kernel_not_found

    ; Check if it's an executable file
    cmp word [esi + Elf32_Ehdr.e_type], 2 ; ET_EXEC
    jne kernel_not_found

.load_program_headers:
    mov esi, [elf_header_buffer] ; Load ELF header buffer address

    ; Load program headers
    mov ecx, [esi + Elf32_Ehdr.e_phoff] ; e_phoff
    movzx eax, word [esi + Elf32_Ehdr.e_phnum] ; e_phnum
    movzx ebx, word [esi + Elf32_Ehdr.e_phentsize] ; e_phentsize
    mul ebx ; Calculate size of all program headers
    mov ebx, eax

    mov dl, [bp-1] ; Drive number
    mov ax, [bp-3] ; Load Found Cluster
    mov edi, [elf_header_buffer] ; Load ELF header buffer address
    add edi, Elf32_Ehdr_size ; Move to the start of program headers
    call load_file_offset

    test ah, ah
    jnz disk_error

    call debug

.load_loadable_segments:
    mov edx, [elf_header_buffer]

    mov esi, edx
    add esi, Elf32_Ehdr_size
    movzx ecx, word [edx + Elf32_Ehdr.e_phnum]
    movzx ebx, word [edx + Elf32_Ehdr.e_phentsize]
.load_next_program_header:
    test ecx, ecx
    jz .load_segments_done

    push ecx
    push ebx
    push esi
    push edx
    push edi

    cmp dword [esi + Elf32_Phdr.p_type], 1  ; PT_LOAD
    jne .skip_segment

    mov ax, [bp-3] ; Load Found Cluster
    mov ebx, [esi + Elf32_Phdr.p_filesz] ; p_filesz
    mov ecx, [esi + Elf32_Phdr.p_offset] ; p_offset
    mov edi, [esi + Elf32_Phdr.p_paddr] ; p_paddr
    mov dl, [bp-1]
    cmp edi, 0
    jz .skip_segment ; Skip if p_paddr is zero

    call load_file_offset

    test ah, ah
    jnz disk_error

    mov ecx, [esi + Elf32_Phdr.p_memsz] ; p_memsz
    mov ebx, [esi + Elf32_Phdr.p_filesz] ; p_filesz
    cmp ecx, ebx
    jz .skip_segment ; If p_memsz == p_filesz, skip zeroing
    ; Zero out the memory if p_memsz > p_filesz
    xor eax, eax
    sub ecx, ebx ; Calculate size to zero
    mov edi, [esi + Elf32_Phdr.p_paddr] ; p_paddr
    add edi, ebx ; Move edi to the end of the loaded segment
    a32 rep stosb ; Zero out the memory

.skip_segment:
    pop edi
    pop edx
    pop esi
    pop ebx
    pop ecx
    add esi, ebx
    dec ecx
    jmp .load_next_program_header

.load_segments_done:
    mov eax, [edx + Elf32_Ehdr.e_entry]
    test eax, eax
    jz kernel_not_found

    mov [bp-13], eax ; Save entry point

    call debug

jump_to_kernel:
    ; Load the GDT
    lgdt [gdt_description]

    mov bx, 0x0000
    mov es, bx
    ; Buffer offset
    mov bx, [memory_map_buffer]
    mov di, bx
    call load_memory_map

    ; Jump to the kernel
    ; enter protected mode (32 bit)
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    jmp 0x08:protected_mode

[BITS 32]
protected_mode:
    mov eax, 0x2BADB002; ; Multiboot magic number

    jmp dword [bp-13]
    jmp $

[BITS 16]
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
    push ecx
    xor ecx, ecx          ; Clear ECX for loading byte offset
    call load_file_offset
.exit:
    pop ecx
    mov sp, bp
    pop bp
    ret

;--------------------------------------------
; Load File Offset
; Input: DL = Drive, AX = File first cluster, EBX = Bytes to load, ECX = Loading byte offset,  EDI = Destination
; Output: AH = Status (0 = success, else error)
;--------------------------------------------
load_file_offset:
    push bp
    mov bp, sp
    sub sp, 14              ; Local variables:
                          ; -2(bp) = sectors to read in one iteration
                          ; -4(bp) = file size in sectors
                          ; -6(bp) = current LBA
                          ; -10(bp) = destination pointer
                          ; -12(bp) = drive number
                          ; -14(bp) = bytes to skip

    push esi
    push edi
    push edx

    mov dword [bp-10], edi        ; Save destination pointer
    mov [bp-12], dl         ; Save drive number

    ; Calculate initial LBA from cluster
    call cluster_to_sector
    mov [bp-6], ax        ; Save LBA

    push ebx

    ; Calculate offset in sectors and bytes to skip
    mov eax, ecx          ; Load offset in EAX
    mov ebx, 512          ; Bytes per sector
    xor edx, edx          ; Clear high bits for division
    div ebx               ; EAX = sectors, EDX = remainder
    mov [bp-14], dx       ; Store bytes to skip
    add [bp-6], ax        ; Update LBA with sectors offset

    pop ebx

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
    mov dl, [bp-12]       ; Drive number
    call read_sectors_lba
    test ah, ah           ; Check status
    jnz .exit             ; Exit if error

    ; Copy from disk buffer to destination
    movzx esi, word [disk_buffer]        ; Source: disk buffer
    add esi, [bp-14]      ; Adjust source pointer by bytes to skip
    mov edi, [bp-10]        ; Destination
    movzx ecx, word [bp-2]        ; Number of sectors
    mov eax, 512
    mul ecx               ; AX = sectors * 512
    mov ecx, eax           ; CX = number of bytes to copy
    a32 rep movsb            ; Copy CX bytes from DS:SI to ES:DI

    ; Update pointers and counters
    movzx eax, word [bp-2]  ; Get sectors read
    mov ecx, 512
    mul ecx                 ; EAX = sectors * 512
    add [bp-10], eax         ; Update destination pointer

    mov ax, [bp-2]
    add [bp-6], ax          ; Update LBA
    sub [bp-4], ax          ; Decrease remaining sectors
    mov word [bp-14], 0          ; Reset bytes to skip for next iteration

    jmp .read_loop

.success:
    xor ah, ah              ; Return success status

.exit:
    pop edx
    pop edi
    pop esi

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
    xor eax, eax
    push eax

    add di, 4 ; Reserve space for entries count

    .load_memory_map_loop:
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

    mov [di-4], ecx ; Store size of the entry

    ; Exit if ebx is equal to zero (reading has ended)
    cmp ebx, 0
    ; Increment pointer in buffer
    add di, 24
    jne .load_memory_map_loop

    ; Store entries count at the begin of the buffer
    pop eax
    pop di
    mov [multiboot_info + Multiboot_Boot_Info.mmap_length], eax
    movzx eax, di
    mov [multiboot_info + Multiboot_Boot_Info.mmap_addr], eax
    ret


;--------------------------------------------
; Simple bubble sort based on the base address
; Input: ESI = Address of memory map, ECX = Length of memory map
; Each entry is 24 bytes
;--------------------------------------------
sort_memory_map:
    push bp
    mov bp, sp
    sub sp, 8 ; Local variable:
              ; -4(bp) = n
              ; -8(bp) = newn

    pushad

    ; n = length
    mov [bp-4], ecx

.sort_memory_map_loop:
    ; newn = 0
    mov dword [bp-8], 0
    ; for i = 1 to n-1
    mov ecx, 1
.inner_loop:
    cmp ecx, [bp-4]             ; i < n ?
    jae .end_inner              ; if i >= n, exit loop

    mov eax, ecx
    imul eax, eax, 24 ; eax = i * 24 (size of each entry)
    add eax, esi ; eax = address of entry i
    mov edi, eax
    sub edi, 24 ; edi = address of entry i-1
    ; if entry[i-1].base_addr > entry[i].base_addr
    mov ebx, [edi + 8] ; Load base_addr_high of entry i-1
    cmp ebx, [eax + 8] ; Compare with base_addr_high of entry i
    ja .swap_entries
    jb .no_swap
    mov ebx, [edi + 4] ; Load base_addr of entry i-1
    cmp ebx, [eax + 4] ; Compare with base_addr of entry i

    jbe .no_swap
    ; Swap entries
.swap_entries:
    push ecx
    mov ecx, 6
    ; Swap 6 dwords (24 bytes)
.swap_loop:
    mov ebx, [edi]
    mov edx, [eax]
    mov [edi], edx
    mov [eax], ebx
    add edi, 4
    add eax, 4
    loop .swap_loop
    pop ecx
    mov [bp-8], ecx ; newn = i
.no_swap:
    inc ecx
    jmp .inner_loop

.end_inner:
    ; n = newn
    mov eax, [bp-8]
    mov [bp-4], eax
    ; until n <= 1
    cmp dword [bp-4], 1
    jnle .sort_memory_map_loop
.exit:
    popad
    mov sp, bp
    pop bp
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


root_directory dw 0x7E00
kernel_buffer dd 0x100000
elf_header_buffer dd 0x500
multiboot_info dd (0x500 + Elf32_Ehdr_size)
memory_map_buffer dd (0x500 + Elf32_Ehdr_size + 116)
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
