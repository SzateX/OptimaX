; boot.asm
BITS 16
org 0x7C00

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

    ; Read the root directory into memory at 0x7E00
    mov bx, 0x7E00        ; ES:BX points to buffer at 0x0000:0x7E00
    call read_root_dir

    ; Search for SECONDST.BIN in the root directory
    mov si, 0x7E00        ; Start of root directory entries
    mov di, 0x7E00 + 224 * 32  ; End of root directory (224 entries * 32 bytes per entry)
    call find_file

    jc file_not_found     ; If CF is set, file was not found

    ; Calculate the first data sector of the file
    mov ax, [FOUND_CLUSTER]
    call cluster_to_sector
    mov [FILE_SECTOR], ax

    ; Calculate the number of sectors to read
    mov ax, [FILE_SIZE]   ; File size in bytes
    xor dx, dx
    div word [BytesPerSector]  ; Divide by bytes per sector
    cmp dx, 0             ; Check if there's any remainder
    je no_extra_sector    ; If remainder is zero, no extra sector is needed
    inc ax
no_extra_sector:
    mov [SECTORS_TO_READ], ax

    ; Load the second-stage bootloader into memory at 0x0000:F000
    mov ax, 0x0000        ; Segment 0x0000
    mov es, ax
    mov bx, 0xF000        ; Offset F000h
    call read_file

    ; Jump to the loaded second-stage bootloader
    jmp 0x0000:0xF000

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
read_file:
    mov ax, [FILE_SECTOR]      ; Starting sector (LBA) of the file
    mov cx, [SECTORS_TO_READ]  ; Number of sectors to read
    mov [0xEFF0], ax
    mov [0xEFF2], cx
    call read_sectors
    ret

; Subroutine to read sectors into memory
; AX = starting LBA sector
; CX = number of sectors to read
; BX = memory buffer address
read_sectors:
    push bx
read_sectors_loop:
    push cx
    mov cx, 1                 ; Read one sector at a time
    call lba_to_chs
    push ax
    mov ah, 0x02              ; BIOS Read Sector function
    mov al, 1                 ; Number of sectors to read
    mov dl, [BOOT_DRIVE]      ; Drive number
    int 0x13
    jc disk_error
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

    xor dx, dx                 ; Clear DX
    mov cx, [SectorsPerTrack]  ; Sectors per track
    div cx                     ; AX / Sectors per track, AX = quotient (track number), DX = remainder
    inc dl                     ; Remainder contains sector number, Sectors start at 1, so add 1
    push dx                    ; Save sector number

    xor dx, dx
    mov cx, [NumberOfHeads]    ; Number of heads
    div cx                     ; AX / Numer of heads, AX = cylinder number, DX = head number
    mov ch, al                 ; CH = cylinder number

    ; Now handle the upper 2 bits of the cylinder and pack them into CL.
    ; CL holds the sector number in its lower 6 bits. We need to add the upper 2 bits of the cylinder to the high bits of CL.
    pop cx                   ; CL = sector number
    and cl, 00111111b        ; Keep only the lower 6 bits for the sector number
    shl ah, 6                ; Move the upper 2 bits of the cylinder into position
    or  cl, ah               ; Combine with CL (upper 2 bits of cylinder + sector number)
    mov dh, dl               ; DH = head number (from the second division)
    mov esp, ebp
    pop ebp
    ret

; Subroutine to print a null-terminated string pointed by SI
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

file_name       db 'SECONDSTBIN'  ; Filename in 8.3 format (11 bytes)
disk_error_msg  db 'Disk Error!', 0
file_not_found_msg db 'SECONDST.BIN not found!', 0


; Pad the rest of the boot sector with zeros
times 510-($-$$) db 0

; Boot sector signature (0x55AA)
dw 0xAA55

; Verify that the boot signature is at the correct position
signature_offset equ 510

%if ($ - $$) != (signature_offset + 2)
    %error "Boot signature is not at the correct position!"
%endif