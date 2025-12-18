;multiboot.asm

struc Multiboot_Boot_Info
  .flags resd 1
  .mem_lower resd 1
  .mem_upper resd 1
  .boot_device resd 1
  .cmdline resd 1
  .mods_count resd 1
  .mods_addr resd 1
  .syms resd 3 ; (elf) struct Multiboot_ELF_Symbols
  .mmap_length resd 1
  .mmap_addr resd 1
  .drives_length resd 1
  .drives_addr resd 1
  .config_table resd 1
  .boot_loader_name resd 1
  .apm_table resd 1
  .vbe_control_info resd 1
  .vbe_mode_info resd 1
  .vbe_mode resd 1
  .vbe_interface_seg resd 1
  .vbe_interface_off resd 1
  .vbe_interface_len resd 1
  .framebuffer_addr resd 1
  .framebuffer_pitch resd 1
  .framebuffer_width resd 1
  .framebuffer_height resd 1
  .framebuffer_bpp resd 1
  .framebuffer_type resd 1
  .framebuffer_color_info resb 6
endstruc

struc Multiboot_Memory_Map_Entry
  .base_addr_low resd 1
  .base_addr_high resd 1
  .length_low resd 1
  .length_high resd 1
  .type resd 1
endstruc