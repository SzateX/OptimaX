ENTRY(_start)

SECTIONS
{
    /DISCARD/ :
    {
        *(.comment)
        *(.note.gnu.build-id)
    }

    /* Put the multiboot header at the start of the *file* */
    .multiboot 0 : AT(0)
    {
        KEEP(*(.multiboot))
    }

    /*
      Now we instruct the linker that the next section (text) should
      load at 0x00100000 in memory. Even though the multiboot header is
      at file offset 0, the CPU will jump to 1MB when the bootloader
      loads it.
    */
    . = 0x00100000;

    .text ALIGN(4): AT(ADDR(.text))
    {
        KEEP(*(.text))
    }

    .rodata ALIGN(4): AT(ADDR(.rodata))
    {
        *(.rodata)
        *(.rodata*)
    }

    .data ALIGN(4): AT(ADDR(.data))
    {
        *(.data)
    }

    .bss ALIGN(4): AT(ADDR(.bss))
    {
        *(COMMON)
        *(.bss)
    }
}
