cmake_minimum_required(VERSION 3.20)
project(bootloader ASM_NASM C)

enable_language(ASM_NASM C)

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_ASM_NASM_COMPILE_OBJECT "<CMAKE_ASM_NASM_COMPILER> <INCLUDES> <FLAGS> -o <OBJECT> <SOURCE>")
set(CMAKE_ASM_NASM_FLAGS "-f bin")
set(CMAKE_ASM_NASM_FLAGS_ELF32 "-f elf32")

if(BUILD_ARCH MATCHES "^(i.86|x86_64)$")
    if(BUILD_PLATFORM MATCHES "pc")
        set(ASM i386/pc/boot.asm)
        set(SECOND_STAGE i386/pc/second_stage.asm)
        set(LINKER_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/i386/pc/linker.ld)
        set(SECOND_STAGE_STUB i386/pc/second_stage_stub.asm)
        set(SOURCE_FILES ${SECOND_STAGE_STUB} i386/pc/second_stage.c)
    else()
        message(FATAL_ERROR "Wrong platform: ${BUILD_PLATFORM}")
    endif()
else()
    message(FATAL_ERROR "Wrong architecture: ${BUILD_ARCH}")
endif()

set_source_files_properties(${ASM} PROPERTIES LANGUAGE ASM_NASM)

add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/boot.bin
        COMMAND ${CMAKE_ASM_NASM_COMPILER} ${CMAKE_ASM_NASM_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${ASM} -o ${CMAKE_CURRENT_BINARY_DIR}/boot.bin
        DEPENDS ${ASM}
        COMMENT "Assembling Stage 1 Bootloader"
)

# Define a custom target that depends on boot.bin
add_custom_target(bootloader_stage1 DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/boot.bin)

set_source_files_properties(${SECOND_STAGE} PROPERTIES LANGUAGE ASM_NASM)
set_source_files_properties(${SECOND_STAGE_STUB} PROPERTIES LANGUAGE ASM_NASM COMPILE_FLAGS "-f elf32")

add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/secondst.bin
        COMMAND ${CMAKE_ASM_NASM_COMPILER} ${CMAKE_ASM_NASM_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${SECOND_STAGE} -o ${CMAKE_CURRENT_BINARY_DIR}/secondst.bin
        DEPENDS ${SECOND_STAGE}
        COMMENT "Assembling Stage 2 Bootloader"
)

add_custom_target(bootloader_stage2 DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/secondst.bin)

set(CMAKE_C_COMPILER "clang")

## Setup cross-compilation
set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR i386)

set(COMPILER_FLAGS "-target i386-pc-none-elf -march=i386 -ffreestanding -Oz -m16 -fomit-frame-pointer -fno-stack-protector -fno-pie")
set(CMAKE_C_FLAGS "${COMPILER_FLAGS}")

# Set linker flags to use the linker script and produce binary output
set(LINKER_FLAGS "-nostdlib -T${LINKER_SCRIPT} -Wl")
set(CMAKE_EXE_LINKER_FLAGS "${LINKER_FLAGS}")


# Create executable named thirdst.bin
add_executable(thirdst.bin ${SOURCE_FILES})
set_target_properties(thirdst.bin PROPERTIES LINK_DEPENDS "${LINKER_SCRIPT}")

add_custom_target(bootloader_bin DEPENDS bootloader_stage1 bootloader_stage2 thirdst.bin)