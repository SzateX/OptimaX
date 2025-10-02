# CmakeLists.txt

cmake_minimum_required(VERSION 3.20)
project(OptimaX C)

set(CMAKE_VERBOSE_MAKEFILE ON)

set(BUILD_ARCH "i386")
set(BUILD_PLATFORM "pc")
set(BOOT_DISK_FILE_SYSTEM "fat12")

# bootloader type = own (custom bootloader) | grub (GRUB bootloader)
set(bootloader_type "own")

set(CMAKE_C_COMPILER "/usr/bin/clang")

# Paths
set(FLOPPY_IMG ${CMAKE_BINARY_DIR}/floppy.img)

add_subdirectory(src/kernel)

if (bootloader_type STREQUAL "own")
    set(BOOTLOADER_OUTPUT_DIR ${CMAKE_BINARY_DIR}/bootloader_output)

    add_subdirectory(src/bootloader)

    # Paths of the bootloader binaries
    set(BOOTLOADER_BIN ${BOOTLOADER_OUTPUT_DIR}/boot.bin)
    set(STAGE2_BIN ${BOOTLOADER_OUTPUT_DIR}/secondst.bin)


    # Custom command to create the floppy image
    add_custom_command(
            OUTPUT ${FLOPPY_IMG}
            COMMAND ${CMAKE_COMMAND} -E echo "Creating floppy image with bootloader..."
            # Validate bootloader (size and signature)
            COMMAND ${CMAKE_COMMAND} -DBOOTLOADER_BIN=${BOOTLOADER_BIN} -P ${CMAKE_SOURCE_DIR}/verify_bootloader.cmake
            # Create empty floppy image
            COMMAND dd if=/dev/zero of=${FLOPPY_IMG} bs=1024 count=1440
            # Format the image with FAT12 filesystem
            COMMAND mkfs.msdos -F 12 -I ${FLOPPY_IMG}
            # Insert bootloader into the boot sector
            COMMAND dd if=${BOOTLOADER_BIN} of=${FLOPPY_IMG} bs=512 count=1 conv=notrunc
            # Copy Stage 2 bootloader into the root directory
            COMMAND mcopy -i ${FLOPPY_IMG} ${STAGE2_BIN} ::/secondst.bin
            # Copy the kernel into the root directory
            # COMMAND ls -la .
            COMMAND mcopy -i ${FLOPPY_IMG} $<TARGET_FILE:kernel> ::/KERNEL.BIN
            COMMAND mcopy -i ${FLOPPY_IMG} -s ../file_system/DATA ::/DATA
            COMMAND mcopy -i ${FLOPPY_IMG} -s ../file_system/ENV ::/ENV
            DEPENDS ${BOOTLOADER_BIN} ${STAGE2_BIN}
            COMMENT "Creating floppy image with bootloader"
    )

    # Custom target to build the floppy image
    add_custom_target(
            floppy_image ALL
            DEPENDS ${FLOPPY_IMG}
    )

    # Add a dependency to ensure bootloader is built before floppy_image
    add_dependencies(floppy_image bootloader kernel)

elseif (bootloader_type STREQUAL "grub")
    # Point to your script in the source tree (update path if needed).
    set(CREATE_GRUB_FLOPPY_SCRIPT "${CMAKE_SOURCE_DIR}/scripts/create_grub_floppy.sh")

    add_custom_command(
            OUTPUT ${FLOPPY_IMG}
            COMMAND /bin/bash ${CREATE_GRUB_FLOPPY_SCRIPT} ${FLOPPY_IMG} $<TARGET_FILE:kernel>
            DEPENDS kernel
            COMMENT "Creating floppy image with GRUB bootloader"
    )

    # Custom target to build the floppy image
    add_custom_target(
            floppy_image ALL
            DEPENDS ${FLOPPY_IMG}
    )

    add_dependencies(floppy_image kernel)
else ()
    message(FATAL_ERROR "Bootloader type '${bootloader_type}' is not supported.")
endif ()