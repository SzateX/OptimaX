;elf.asm

struc Elf32_Ehdr
  .e_ident resb 16 ; 0
  .e_type resw 1 ; 16 (0x10)
  .e_machine resw 1 ; 18 (0x12)
  .e_version resd 1 ; 20 (0x14)
  .e_entry resd 1 ; 24 (0x18)
  .e_phoff resd 1 ; 28 (0x1C)
  .e_shoff resd 1 ; 32 (0x20)
  .e_flags resd 1 ; 36 (0x24)
  .e_ehsize resw 1 ; 40 (0x28)
  .e_phentsize resw 1 ; 42 (0x2A)
  .e_phnum resw 1 ; 44 (0x2C)
  .e_shentsize resw 1 ; 46 (0x2E)
  .e_shnum resw 1 ; 48 (0x30)
  .e_shstrndx resw 1 ; 50 (0x32)
endstruc

struc Elf32_Ident
  .ei_mag resb 4
  .ei_class resb 1
  .ei_data resb 1
  .ei_version resb 1
  .ei_osabi resb 1
  .ei_abiversion resb 1
  .ei_pad resb 7
endstruc

struc Elf32_Phdr
  .p_type resd 1 ; 0 (0x00)
  .p_offset resd 1 ; 4 (0x04)
  .p_vaddr resd 1 ; 8 (0x08)
  .p_paddr resd 1 ; 12 (0x0C)
  .p_filesz resd 1 ; 16 (0x10)
  .p_memsz resd 1 ; 20 (0x14)
  .p_flags resd 1 ; 24 (0x18)
  .p_align resd 1 ; 28 (0x1C)
endstruc

struc Elf32_Shdr
  .sh_name resd 1
  .sh_type resd 1
  .sh_flags resd 1
  .sh_addr resd 1
  .sh_offset resd 1
  .sh_size resd 1
  .sh_link resd 1
  .sh_info resd 1
  .sh_addralign resd 1
  .sh_entsize resd 1
endstruc

struc Elf32_Sym
  .st_name resd 1
  .st_value resd 1
  .st_size resd 1
  .st_info resb 1
  .st_other resb 1
  .st_shndx resw 1
endstruc

struc Elf32_Rel
  .r_offset resd 1
  .r_info resd 1
endstruc

struc Elf32_Rela
  .r_offset resd 1
  .r_info resd 1
  .r_addend resd 1
endstruc

struc Elf32_Dyn
  .d_tag resd 1
  .d_val resd 1
endstruc

struc Elf32_Verdef
  .vd_version resd 1
  .vd_flags resd 1
  .vd_ndx resd 1
  .vd_cnt resd 1
  .vd_hash resd 1
  .vd_aux resd 1
  .vd_next resd 1
endstruc

struc Elf32_Verdaux
  .vda_name resd 1
  .vda_next resd 1
endstruc

struc Elf32_Verneed
  .vn_version resd 1
  .vn_cnt resd 1
  .vn_file resd 1
  .vn_aux resd 1
  .vn_next resd 1
endstruc

struc Elf32_Vernaux
  .vna_hash resd 1
  .vna_flags resd 1
  .vna_other resd 1
  .vna_name resd 1
  .vna_next resd 1
endstruc

struc Elf32_Versym
  .vs_val resd 1
endstruc

