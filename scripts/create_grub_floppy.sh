#!/usr/bin/env bash
#
# create_grub_floppy.sh
#   Usage: create_grub_floppy.sh FLOPPY_IMG KERNEL_BIN
#
#   Creates a floppy image named FLOPPY_IMG containing a GRUB bootloader
#   and optionally copies KERNEL_BIN (or other files) to it.

set -e

FLOPPY_IMG="$1"
KERNEL_BIN="$2"

# Derive some other paths
OUTDIR="$(dirname "$FLOPPY_IMG")"
GRUB_IMG="${OUTDIR}/$(basename "$FLOPPY_IMG" .img)-grub.img"

echo "Creating GRUB image at: $GRUB_IMG"

# 1) Build the grub image
grub-mkimage \
  -p /grub \
  -C auto \
  -O i386-pc \
  -o "$GRUB_IMG" \
  biosdisk part_msdos fat multiboot configfile ls cat help

# 2) Determine how many 512-byte blocks the GRUB image occupies
SIZE=$(ls --block-size=512 -s "$GRUB_IMG" | sed 's/\s.*$//')

# 3) Create the floppy 1.44MB
dd if=/dev/zero of="$FLOPPY_IMG" bs=1024 count=1440

# 4) Write stage1 GRUB boot sector
dd if=/usr/lib/grub/i386-pc/boot.img of="$FLOPPY_IMG" conv=notrunc

# 5) Write the actual GRUB core image right after sector 0
dd if="$GRUB_IMG" of="$FLOPPY_IMG" conv=notrunc seek=1

# 6) Format the rest of the floppy as FAT and create a /grub directory
mformat -i "$FLOPPY_IMG" -kR $((SIZE + 2))
mmd -i "$FLOPPY_IMG" grub

echo "Kernel binary: $KERNEL_BIN"

if grub-file --is-x86-multiboot "$KERNEL_BIN"; then
  echo multiboot confirmed
else
  echo the file is not multiboot
fi

# 7) Optionally copy the kernel or other files
if [ -n "$KERNEL_BIN" ] && [ -f "$KERNEL_BIN" ]; then
  echo "Copying kernel: $KERNEL_BIN"
  mcopy -i "$FLOPPY_IMG" "$KERNEL_BIN" ::/KERNEL.BIN
  mcopy -i "$FLOPPY_IMG" ../grub_config/grub.cfg ::/grub/grub.cfg
fi

echo "Floppy image created successfully at: $FLOPPY_IMG"
