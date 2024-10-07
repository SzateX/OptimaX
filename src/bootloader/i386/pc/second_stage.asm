; It does not work, dont' know why?
; second_stage.asm
BITS 16
org 0xF000

jmp short start
nop

; BIOS Parameter Block (BPB)
OEMLabel         db 'OptimaX '     ; 8 bytes OEM Name
BytesPerSector   dw 512            ; Bytes per sector
SectorsPerCluster db 1             ; Sectors per cluster
ReservedSectors  dw 1              ; Number of reserved sectors
NumberOfFATs     db 2              ; Number of FATs
RootDirEntries   dw 224            ; Number of root directory entries
TotalSectors     dw 2880           ; Total sectors (for floppies)
MediaDescriptor  db 0xF0           ; Media descriptor
SectorsPerFAT    dw 9              ; Sectors per FAT
SectorsPerTrack  dw 18             ; Sectors per track
NumberOfHeads    dw 2              ; Number of heads
HiddenSectors    dd 0              ; Hidden sectors
TotalSectorsBig  dd 0              ; (not used for floppies)

DriveNumber      db 0              ; Interrupt 13 drive number
Reserved         db 0              ; Reserved
Signature        db 0x29           ; Extended boot signature
VolumeID         dd 0x12345678     ; Volume ID
VolumeLabel      db 'OPTIMAX PAR'  ; 11 bytes Volume Label
FileSystemType   db 'FAT12   '     ; 8 bytes File system type

; Bootloader code starts here
start:
    cli
    cld

    ; Store the drive number (provided by BIOS in DL)
    mov [BOOT_DRIVE], dl

    ; Initialize the stack
    xor ax, ax
    mov ss, ax
    mov sp, 0x7C00

    ; Set up segment registers
    mov ds, ax
    mov es, ax

    mov si, hello_second_stage_msg
    call print_string

    ; Read the root directory into memory at 0x7E00
    mov bx, 0x7E00        ; ES:BX points to buffer at 0x0000:0x7E00
    xor dx, dx
    mov gs, dx
    call read_root_dir

    ; Search for KERNEL.BIN in the root directory
    mov si, 0x7E00        ; Start of root directory entries
    mov di, 0x7E00 + 224 * 32  ; End of root directory (224 entries * 32 bytes per entry)
    call find_file

    jc file_not_found     ; If CF is set, file was not found

    ; Check If Line A20 Enabled
    mov si, check_a20_gate
    call print_string
    call check_a20
    or ax, ax
    jnz A20Ready
    mov si, enable_a20
    call print_string
    call enable_A20_BIOS

    A20Ready:
    mov si, a20_enabled_str
    call print_string
    ; Entering unreal mode
    cli                    ; no interrupts
    push ds                ; save real mode

    lgdt [gdtinfo]         ; load gdt register

    mov  eax, cr0          ; switch to pmode by
    or al,1                ; set pmode bit
    mov  cr0, eax
    jmp 0x8:pmode

    pmode:
    mov  bx, 0x10          ; select descriptor 2
    mov  ds, bx            ; 10h = 10000b

    and al,0xFE            ; back to realmode
    mov  cr0, eax          ; by toggling bit again
    jmp 0x0:unreal

    unreal:
    pop ds                 ; get back old segment

    mov si, jumped_to_unreal
    call print_string

    ;Calculate the first data sector of the file
    mov ax, [FOUND_CLUSTER]
    call cluster_to_sector
    mov [FILE_SECTOR], ax

    ;Calculate the number of sectors to read
    mov ax, [FILE_SIZE]   ; File size in bytes
    xor dx, dx
    div word [BytesPerSector]  ; Divide by bytes per sector
    cmp dx, 0             ; Check if there's any remainder
    je no_extra_sector    ; If remainder is zero, no extra sector is needed
    inc ax
no_extra_sector:
    mov [SECTORS_TO_READ], ax
    push edi
    push esi
    mov edi, 0x100000
read_file_loop:
    cmp word [SECTORS_TO_READ], 32
    jle less_than_32_sectors
    mov ax, [FILE_SECTOR]
    mov cx, [SECTORS_TO_READ]
    mov bx, 0x7E00
    mov dx, 1
    mov gs, dx
    call read_file
    mov esi, 0x7E00
    mov ecx, 512 * 32
    cld
    a32 o32 rep movsb ; copy 32 sectors
    sub word [SECTORS_TO_READ], 32
    add word [FILE_SECTOR], 32
    jmp read_file_loop

less_than_32_sectors:
    mov ax, [FILE_SECTOR]
    mov cx, [SECTORS_TO_READ]
    mov bx, 0x7E00
    call read_file
    mov esi, 0x7E00
    mov cx, [SECTORS_TO_READ]
    shr cx, 9
    cld
    a32 o32 rep movsb ; copy 32 sectors

    pop esi
    pop edi

    mov si, loading_finished
    call print_string

jump_to_kernel:
    hlt

    ; Jump to the loaded second-stage bootloader
    ; jmp 0x0000:0xF000

disk_error:
    mov si, disk_error_msg
    call print_string
    jmp halt

file_not_found:
    mov si, file_not_found_msg
    call print_string
    jmp halt

halt:
    cli
    hlt
    jmp halt

; Subroutine to read the root directory (14 sectors starting from sector 19)
; Input: BX = buffer address to load the root directory
read_root_dir:
    mov ax, 19            ; Starting sector (LBA) of root directory
    mov cx, 14            ; Number of sectors to read
    call read_sectors
    jc disk_error
    ret

; Subroutine to find the file in the root directory
; SI: start of root directory
; DI: end of root directory
find_file:
find_next_entry:
    cmp si, di
    jae not_found
    cmp byte [si], 0x00   ; Check for unused entry
    je not_found
    cmp byte [si], 0xE5   ; Check for deleted entry
    je skip_entry
    ; Compare filename (8 bytes) and extension (3 bytes)
    push si               ; Save SI
    push di               ; Save DI
    mov di, si
    mov si, file_name     ; Filename to search for
    mov cx, 11
    repe cmpsb
    pop di                ; Restore DI
    pop si                ; Restore SI
    je file_found
skip_entry:
    add si, 32            ; Move to next directory entry
    jmp find_next_entry   ; Unconditional jump
not_found:
    stc                   ; Set CF to indicate file not found
    ret
file_found:
    ; File found
    ; Get starting cluster (offset 26) and file size (offset 28)
    mov bx, si
    mov ax, [bx + 26]     ; Starting cluster
    mov [FOUND_CLUSTER], ax
    mov ax, [bx + 28]     ; File size in bytes
    mov [FILE_SIZE], ax
    clc                   ; Clear CF to indicate success
    ret

; Subroutine to convert cluster number to sector number
; Input: AX = cluster number
; Output: AX = sector number
cluster_to_sector:
    ; Data area starts at sector 33 (standard for 1.44MB FAT12 floppy)
    sub ax, 2             ; Cluster numbering starts at 2
    mul word [SectorsPerCluster]
    mov bx, ax
    mov ax, 33            ; First data sector
    add ax, bx            ; Sector number = first data sector + (cluster - 2) * sectors per cluster
    ret

; Subroutine to read the file into memory
; Input: BX = buffer address to load the file
; Input CX = Number of sectors to read
; Input AX = Starting sector (LBA) of the file
read_file:
    call read_sectors
    ret

; Subroutine to read sectors into memory
; AX = starting LBA sector
; CX = number of sectors to read
; BX = memory buffer address
; GS = hlt flag
read_sectors:
    push bx
read_sectors_loop:
    push cx
    mov cx, 1                 ; Read one sector at a time
    push ax
    call lba_to_chs
    mov ah, 0x02              ; BIOS Read Sector function
    mov al, 1                 ; Number of sectors to read
    mov dl, [BOOT_DRIVE]      ; Drive number
    int 0x13
    jc disk_error

    ;push dx
    ;        mov dx, gs
    ;        cmp dx, 1
    ;        je halt
    ;        pop dx

    pop ax
    add ax, 1                 ; Next LBA sector
    add bx, 512               ; Move to next memory address
    pop cx
    loop read_sectors_loop
    pop bx
    ret

; Subroutine to convert LBA to CHS
; Input: AX = LBA sector number
; Output: DH = head, CH = cylinder, CL = sector
lba_to_chs:
    ; Disk geometry for 1.44MB floppy disk:
    ; 80 cylinders (tracks), 2 heads, 18 sectors per track
    ; Total sectors: 80 * 2 * 18 = 2880 sectors
    push ebp
    mov ebp, esp
    push bx

    xor dx, dx                 ; Clear DX
    mov cx, [SectorsPerTrack]  ; Sectors per track
    div cx                     ; AX / Sectors per track, AX = quotient (track number), DX = remainder
    inc dx                     ; Remainder contains sector number, Sectors start at 1, so add 1
    push dx                    ; Save sector number

    xor dx, dx
    mov cx, [NumberOfHeads]    ; Number of heads
    div cx                     ; AX / Numer of heads, AX = cylinder number, DX = head number
    mov ch, al                 ; CH = cylinder number

    ; Now handle the upper 2 bits of the cylinder and pack them into CL.
    ; CL holds the sector number in its lower 6 bits. We need to add the upper 2 bits of the cylinder to the high bits of CL.
    pop bx                   ; BL = sector number
    mov cl, bl               ; CL = sector number
    and cl, 00111111b        ; Keep only the lower 6 bits for the sector number
    shl ah, 6                ; Move the upper 2 bits of the cylinder into position
    or  cl, ah               ; Combine with CL (upper 2 bits of cylinder + sector number)
    mov dh, dl               ; DH = head number (from the second division)
    pop bx
    mov esp, ebp
    pop ebp
    ret

check_a20:
    pushf
    push ds
    push es
    push di
    push si
    cli
    xor ax, ax ; ax = 0
    mov es, ax
    not ax ; ax = 0xFFFF
    mov ds, ax
    mov di, 0x0500
    mov si, 0x0510
    mov al, byte [es:di]
    push ax
    mov al, byte [ds:si]
    push ax
    mov byte [es:di], 0x00
    mov byte [ds:si], 0xFF
    cmp byte [es:di], 0xFF
    pop ax
    mov byte [ds:si], al
    pop ax
    mov byte [es:di], al
    mov ax, 0
    je check_a20__exit
    mov ax, 1
check_a20__exit:
    pop si
    pop di
    pop es
    pop ds
    popf
    ret

a20wait:
    in      al,0x64
    test    al,2
    jnz     a20wait
    ret

a20wait2:
    in      al,0x64
    test    al,1
    jz      a20wait2
    ret

halt_a20:
    mov si, a20_error
    call print_string
    cli
    hlt

enable_A20_BIOS:
    mov     ax,2403h                ;--- A20-Gate Support ---
    int     15h
    cmp     ah,0
    jnz     a20_bios_failed                  ;INT 15h is not supported
    mov     ax,2402h                ;--- A20-Gate Status ---
    int     15h
    jb      a20_bios_failed             ;couldn't get status
    cmp     ah,0
    jnz     a20_bios_failed              ;couldn't get status
    cmp     al,1
    jz      a20_enabled           ;A20 is already activated
    mov     ax,2401h                ;--- A20-Gate Activate ---
    int     15h
    jb      a20_bios_failed              ;couldn't activate the gate
    cmp     ah,0
    jnz     a20_bios_failed              ;couldn't activate the gate
; Check if Enabled via BIOS
a20_check_after_bios:
    call check_a20
    or ax, ax
    jnz a20_enabled
; Not Enabled via BIOS. Try by Keyboard Controller
a20_bios_failed:
    call enable_A20_keyboard
; Check if Enabled via Keyboard Controller
a20_keyboard_check:
    call check_a20
    or ax, ax
    jnz a20_enabled
; Not Enabled via Keyboard Controller, Try Fast A20
a20_keyboard_failed:
    in al, 0x92
    or al, 2
    out 0x92, al
; Check if enabled via Fast A20
a20_fast_a20_check:
    call check_a20
    or ax, ax
    jnz a20_enabled
; Not Enabled via A20. No idea what to do here
    mov si, a20_error
    call print_string
    cli
    hlt
; Enabled Line A20
a20_enabled:
    ret

enable_A20_keyboard:
    cli
    call    a20wait
    mov     al,0xAD
    out     0x64,al
    call    a20wait
    mov     al,0xD0
    out     0x64,al
    call    a20wait2
    in      al,0x60
    push    eax
    call    a20wait
    mov     al,0xD1
    out     0x64,al
    call    a20wait
    pop     eax
    or      al,2
    out     0x60,al
    call    a20wait
    mov     al,0xAE
    out     0x64,al
    call    a20wait
    sti
    ret

; Subroutine to print a null-terminated string pointed by SI
; Input: SI = pointer to the string
print_string:
    mov ah, 0x0E          ; BIOS Teletype output function
print_next_char:
    lodsb
    cmp al, 0
    je print_done
    int 0x10
    jmp print_next_char
print_done:
    ret

; Data
BOOT_DRIVE      db 0x00
FOUND_CLUSTER   dw 0x0000
FILE_SIZE       dw 0x0000
FILE_SECTOR     dw 0x0000
SECTORS_TO_READ dw 0x0000

file_name       db 'KERNEL  BIN'  ; Filename in 8.3 format (11 bytes)
disk_error_msg  db 'Disk Error!', 0
file_not_found_msg db 'KERNEL.BIN not found!', 0
hello_second_stage_msg db 'Hello second stage', 0xD, 0XA, 0
check_a20_gate db 'Checking A20 Gate...', 0xD, 0XA, 0
enable_a20 db 'Enabling A20 Gate...', 0xD, 0XA, 0
a20_enabled_str db 'A20 Gate is enabled', 0xD, 0XA, 0
jumped_to_unreal db 'Jumped to unreal mode', 0xD, 0XA, 0
loading_finished db 'Loading finished', 0xD, 0XA, 0
a20_error db 'A20 Gate is not available. Critical ERROR...', 0

gdtinfo:
   dw gdt_end - gdt - 1   ;last byte in table
   dd gdt                 ;start of table

gdt:        dd 0,0        ; entry 0 is always unused
codedesc:   db 0xff, 0xff, 0, 0, 0, 10011010b, 00000000b, 0
flatdesc:   db 0xff, 0xff, 0, 0, 0, 10010010b, 11001111b, 0
gdt_end:

dw 0xAA55

; verify that the second stage bootlader size not exceed 4KB
%if ($ - $$) >= 4096
    %error "Second stage bootloader exceeds 4KB!"
%endif