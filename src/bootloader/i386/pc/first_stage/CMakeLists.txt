# src/bootloader/i386/pc/first_stage/CMakeLists.txt

cmake_minimum_required(VERSION 3.20)
project(bootloader_first_stage ASM_NASM)

enable_language(ASM_NASM)

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_ASM_NASM_COMPILE_OBJECT "<CMAKE_ASM_NASM_COMPILER> <INCLUDES> <FLAGS> -o <OBJECT> <SOURCE>")
set(CMAKE_ASM_NASM_FLAGS "-f bin")
set(CMAKE_ASM_NASM_FLAGS_ELF32 "-f elf32")

if(BOOT_DISK_FILE_SYSTEM MATCHES "fat12")
    set(ASM fat12/boot.asm)
else()
    message(FATAL_ERROR "Wrong file system: ${BOOT_DISK_FILE_SYSTEM}")
endif()

set_source_files_properties(${ASM} PROPERTIES LANGUAGE ASM_NASM)

add_custom_command(
        OUTPUT ${BOOTLOADER_OUTPUT_DIR}/boot.bin
        COMMAND ${CMAKE_COMMAND} -E make_directory ${BOOTLOADER_OUTPUT_DIR}
        COMMAND ${CMAKE_ASM_NASM_COMPILER} ${CMAKE_ASM_NASM_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${ASM} -o ${BOOTLOADER_OUTPUT_DIR}/boot.bin
        DEPENDS ${ASM}
        COMMENT "Assembling Stage 1 Bootloader"
)

add_custom_target(bootloader_stage1 DEPENDS ${BOOTLOADER_OUTPUT_DIR}/boot.bin)