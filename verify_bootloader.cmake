# Get the bootloader binary path
set(BOOTLOADER_BIN "${BOOTLOADER_BIN}")

# Define the maximum allowed bootloader size (512 bytes)
set(MAX_BOOTLOADER_SIZE 512)

# Get the size of the bootloader binary
file(SIZE "${BOOTLOADER_BIN}" BOOTLOADER_SIZE)

# Check if the total size exceeds the maximum allowed size
if(${BOOTLOADER_SIZE} GREATER ${MAX_BOOTLOADER_SIZE})
    message(FATAL_ERROR "Bootloader is too big (${BOOTLOADER_SIZE} bytes). It must be ${MAX_BOOTLOADER_SIZE} bytes or less.")
elseif(${BOOTLOADER_SIZE} LESS ${MAX_BOOTLOADER_SIZE})
    message(WARNING "Bootloader is less than ${MAX_BOOTLOADER_SIZE} bytes. It will be padded with zeros.")
else()
    message(STATUS "Bootloader size is exactly ${MAX_BOOTLOADER_SIZE} bytes.")
endif()

# Read the last two bytes of the bootloader binary
file(READ "${BOOTLOADER_BIN}" BOOTLOADER_CONTENT HEX)

# Get the length of the content
string(LENGTH "${BOOTLOADER_CONTENT}" CONTENT_LENGTH)

# Calculate the position of the last two bytes
math(EXPR SIGNATURE_START "${CONTENT_LENGTH} - 4")  # Each byte is represented by two hex digits

# Extract the last two bytes (boot signature)
string(SUBSTRING "${BOOTLOADER_CONTENT}" ${SIGNATURE_START} 4 BOOT_SIGNATURE)

# Expected boot signature is 55AA (0xAA55 in little-endian format)
set(EXPECTED_SIGNATURE "55aa")

# Compare the boot signature
if(NOT "${BOOT_SIGNATURE}" STREQUAL "${EXPECTED_SIGNATURE}")
    message(FATAL_ERROR "Bootloader does not end with 0x${EXPECTED_SIGNATURE} boot signature. Found 0x${BOOT_SIGNATURE} instead.")
else()
    message(STATUS "Bootloader ends with correct boot signature (0xAA55).")
endif()
