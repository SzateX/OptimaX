# src/kernel/CMakeLists.txt

cmake_minimum_required(VERSION 3.20)
project(kernel ASM_NASM C)

set(CMAKE_VERBOSE_MAKEFILE ON)

if (BUILD_ARCH STREQUAL "i386")
    if (BUILD_PLATFORM MATCHES "pc")
        add_subdirectory(i386/pc)
    else()
        message(FATAL_ERROR "Wrong platform: ${BUILD_PLATFORM}")
    endif()
else()
    message(FATAL_ERROR "Wrong architecture: ${BUILD_ARCH}")
endif()