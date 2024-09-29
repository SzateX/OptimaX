BITS 16
ORG 0xF000

start:
    ; Set up segment registers
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0xFFFE
    sti

    ; Clear the screen (optional)
    mov ah, 0x00
    mov al, 0x03
    int 0x10

    ; Print "Hello World"
    mov si, msg
    call print_string

    ; Halt the system
    cli
    hlt
    jmp $

; Subroutine to print a null-terminated string pointed by SI
print_string:
    mov ah, 0x0E          ; BIOS Teletype output function
.print_next_char:
    lodsb
    cmp al, 0
    je .done
    int 0x10
    jmp .print_next_char
.done:
    ret

msg db 'Kocham Edytke', 0