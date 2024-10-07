cmake_minimum_required(VERSION 3.20)
project(OptimaX C)

set(BUILD_ARCH "i386")
set(BUILD_PLATFORM "pc")
set(CMAKE_C_COMPILER "/usr/bin/clang")

add_subdirectory(src/bootloader)

# Paths
set(BOOTLOADER_BIN ${CMAKE_BINARY_DIR}/src/bootloader/boot.bin)
set(STAGE2_BIN ${CMAKE_BINARY_DIR}/src/bootloader/thirdst.bin)
#set(STAGE2_BIN ${CMAKE_BINARY_DIR}/src/bootloader/secondst.bin)
set(FLOPPY_IMG ${CMAKE_BINARY_DIR}/floppy.img)
set(KERNEL_BIN ../file_system/KERNEL.BIN)

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
        COMMAND ls -la .
        COMMAND mcopy -i ${FLOPPY_IMG} ${KERNEL_BIN} ::/KERNEL.BIN
        DEPENDS ${BOOTLOADER_BIN} ${STAGE2_BIN}
        COMMENT "Creating floppy image with bootloader"
)

# Custom target to build the floppy image
add_custom_target(
        floppy_image ALL
        DEPENDS ${FLOPPY_IMG}
)

# Add a dependency to ensure bootloader_bin is built before floppy_image
add_dependencies(floppy_image bootloader_bin)