# src/bootloader/CMakeLists.txt

cmake_minimum_required(VERSION 3.20)
project(bootloader ASM_NASM C)

if(BUILD_ARCH MATCHES "^(i.86|x86_64)$")
    if(BUILD_PLATFORM MATCHES "pc")
        add_subdirectory(i386/pc/first_stage)
        add_subdirectory(i386/pc/second_stage)
    else()
        message(FATAL_ERROR "Wrong platform: ${BUILD_PLATFORM}")
    endif()
else()
    message(FATAL_ERROR "Wrong architecture: ${BUILD_ARCH}")
endif()


add_custom_target(bootloader DEPENDS bootloader_stage1 bootloader_stage2)