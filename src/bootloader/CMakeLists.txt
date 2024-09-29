cmake_minimum_required(VERSION 3.20)
project(bootloader ASM_NASM)

enable_language(ASM_NASM)

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_ASM_NASM_COMPILE_OBJECT "<CMAKE_ASM_NASM_COMPILER> <INCLUDES> <FLAGS> -o <OBJECT> <SOURCE>")
set(CMAKE_ASM_NASM_FLAGS "-f bin")

if(BUILD_ARCH MATCHES "^(i.86|x86_64)$")
    if(BUILD_PLATFORM MATCHES "pc")
        set(ASM i386/pc/boot.asm)
        set(SECOND_STAGE i386/pc/second_stage.asm)
    else()
        message(FATAL_ERROR "Wrong platform: ${BUILD_PLATFORM}")
    endif()
else()
    message(FATAL_ERROR "Wrong architecture: ${BUILD_ARCH}")
endif()

set_source_files_properties(${ASM} PROPERTIES LANGUAGE ASM_NASM)
set_source_files_properties(${SECOND_STAGE} PROPERTIES LANGUAGE ASM_NASM)

add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/boot.bin
        COMMAND ${CMAKE_ASM_NASM_COMPILER} ${CMAKE_ASM_NASM_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${ASM} -o ${CMAKE_CURRENT_BINARY_DIR}/boot.bin
        DEPENDS ${ASM}
        COMMENT "Assembling Stage 1 Bootloader"
)

# Define a custom target that depends on boot.bin
add_custom_target(bootloader_stage1 DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/boot.bin)


add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/secondst.bin
        COMMAND ${CMAKE_ASM_NASM_COMPILER} ${CMAKE_ASM_NASM_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${SECOND_STAGE} -o ${CMAKE_CURRENT_BINARY_DIR}/secondst.bin
        DEPENDS ${SECOND_STAGE}
        COMMENT "Assembling Stage 2 Bootloader"
)

add_custom_target(bootloader_stage2 DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/secondst.bin)

add_custom_target(bootloader_bin DEPENDS bootloader_stage1 bootloader_stage2)