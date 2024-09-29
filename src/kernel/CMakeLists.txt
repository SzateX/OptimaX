cmake_minimum_required(VERSION 3.20)
project(kernel C)

ENABLE_LANGUAGE(C ASM)

set(CMAKE_VERBOSE_MAKEFILE ON)

set(CMAKE_C_COMPILER "clang")

## Setup cross-compilation
set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR i386)

set(FLAGS_COMMON "-fuse-ld=lld -target i386-pc-none-elf -march=i386 -ffreestanding -nostdlib")
set(CMAKE_C_FLAGS "${FLAGS_COMMON}")

set(CMAKE_C_STANDARD 11)

set(SOURCE_FILES kernel.c)

add_executable (kernel.elf ${SOURCE_FILES})
set_target_properties(kernel.elf PROPERTIES LINK_DEPENDS ${CMAKE_SOURCE_DIR}/linker.ld)