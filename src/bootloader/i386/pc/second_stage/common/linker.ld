OUTPUT_FORMAT("binary")
ENTRY(_start)
SECTIONS
{
  /* Set the start address to 0x0000F000 */
  . = 0x0000F000;

  .text :
  {
    *(.text)
  }

  .rodata :
  {
    *(.rodata)
  }

  .data :
  {
    *(.data)
  }

  .bss :
  {
    *(.bss)
    *(COMMON)
  }
}