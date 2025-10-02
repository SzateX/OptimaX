# src/bootloader/i386/pc/second_stage/CMakeLists.txt

cmake_minimum_required(VERSION 3.20)
project(bootloader_second_stage ASM_NASM)

enable_language(ASM_NASM)

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_ASM_NASM_COMPILE_OBJECT "<CMAKE_ASM_NASM_COMPILER> <INCLUDES> <FLAGS> -o <OBJECT> <SOURCE>")
set(CMAKE_ASM_NASM_FLAGS "-f bin")
set(CMAKE_ASM_NASM_FLAGS_ELF32 "-f elf32")

set(CMAKE_C_COMPILER "clang")

## Setup cross-compilation
set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR i386)

set(COMPILER_FLAGS "-target i386-pc-none-elf -march=i386 -ffreestanding -Oz -m16 -fomit-frame-pointer -fno-stack-protector -fno-pie -fno-unroll-loops")
set(CMAKE_C_FLAGS "${COMPILER_FLAGS}")
set(CMAKE_C_FLAGS_DEBUG "${COMPILER_FLAGS} -DDEBUG")
set(CMAKE_C_FLAGS_RELEASE "${COMPILER_FLAGS}")

if(BOOT_DISK_FILE_SYSTEM MATCHES "fat12")
    set(SECOND_STAGE fat12/second_stage_elf.asm)
    set(STAGE2_INC "${CMAKE_CURRENT_SOURCE_DIR}/fat12")
    set_source_files_properties(${SECOND_STAGE} PROPERTIES LANGUAGE ASM_NASM)

    add_custom_command(
            OUTPUT ${BOOTLOADER_OUTPUT_DIR}/secondst.bin
            COMMAND ${CMAKE_COMMAND} -E make_directory ${BOOTLOADER_OUTPUT_DIR}
            COMMAND ${CMAKE_ASM_NASM_COMPILER} ${CMAKE_ASM_NASM_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${SECOND_STAGE} -I "${STAGE2_INC}" -o ${BOOTLOADER_OUTPUT_DIR}/secondst.bin
            DEPENDS ${SECOND_STAGE}
            COMMENT "Assembling Stage 2 Bootloader"
    )

    add_custom_target(bootloader_stage2 DEPENDS ${BOOTLOADER_OUTPUT_DIR}/secondst.bin)
else()
    set(LINKER_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/common/linker.ld)
    set(SECOND_STAGE_STUB ${CMAKE_CURRENT_SOURCE_DIR}/common/second_stage_stub.asm)
    set(SOURCE_FILES ${SECOND_STAGE_STUB} ${CMAKE_CURRENT_SOURCE_DIR}/common/second_stage.c)

    # Set linker flags to use the linker script and produce binary output
    set(LINKER_FLAGS "-nostdlib -T${LINKER_SCRIPT}")
    set(CMAKE_EXE_LINKER_FLAGS "${LINKER_FLAGS}")

    set_source_files_properties(${SECOND_STAGE_STUB} PROPERTIES LANGUAGE ASM_NASM COMPILE_FLAGS "-f elf32")

    add_executable(secondst.bin ${SOURCE_FILES})
    set_target_properties(secondst.bin PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${BOOTLOADER_OUTPUT_DIR} LINK_DEPENDS "${LINKER_SCRIPT}")

    add_custom_target(bootloader_stage2 DEPENDS ${BOOTLOADER_OUTPUT_DIR}/secondst.bin)
endif()