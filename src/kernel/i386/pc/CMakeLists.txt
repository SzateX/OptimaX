# src/kernel/arch/i386/pc/CMakeLists.txt

cmake_minimum_required(VERSION 3.20)
project(kernel C ASM_NASM)

set(CMAKE_VERBOSE_MAKEFILE ON)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_COMPILER "clang")

enable_language(ASM_NASM)

## Setup cross-compilation
set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR i386)

set(CMAKE_ASM_NASM_COMPILE_OBJECT "<CMAKE_ASM_NASM_COMPILER> <INCLUDES> <FLAGS> -o <OBJECT> <SOURCE>")
set(CMAKE_ASM_NASM_OBJECT_FORMAT "elf32")
set(CMAKE_ASM_NASM_FLAGS "-f elf32")
set(CMAKE_ASM_NASM_FLAGS_ELF32 "-f elf32")

set(FLAGS_COMMON "-fuse-ld=lld -target i386-pc-none-elf -march=i386 -ffreestanding -nostdlib -fno-pic -fno-pie -static")
set(CMAKE_C_FLAGS_DEBUG "${FLAGS_COMMON} -g -O0 -ggdb")
set(CMAKE_C_FLAGS_RELEASE "${FLAGS_COMMON}")

set(LINKER_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/linker.ld)
set(KERNEL_STUB ${CMAKE_CURRENT_SOURCE_DIR}/startup.asm)
set(SOURCE_FILES ${KERNEL_STUB} ${CMAKE_CURRENT_SOURCE_DIR}/kernel.c)

# Set linker flags to use the linker script and produce binary output
set(LINKER_FLAGS "-nostdlib -Wl,-z,notext -T${LINKER_SCRIPT}")
set(CMAKE_EXE_LINKER_FLAGS "${LINKER_FLAGS}")

set_source_files_properties(${KERNEL_STUB} PROPERTIES LANGUAGE ASM_NASM)

add_executable(kernel ${SOURCE_FILES})
set_target_properties(kernel PROPERTIES LINK_DEPENDS "${LINKER_SCRIPT}" OUTPUT_NAME "kernel.bin")