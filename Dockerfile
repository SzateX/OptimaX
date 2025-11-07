FROM debian:bookworm-slim

ENV DEBIAN_FRONTEND=noninteractive
ENV LIBGUESTFS_BACKEND=direct

RUN apt-get update && \
    apt-get install --no-install-recommends -y \
      clang lld llvm gcc g++ gdb make nasm \
      qemu-system-x86 qemu-system-gui qemu-utils \
      mtools xorriso grub-pc-bin grub-common cmake \
      python3 python3-pip python3-guestfs libguestfs-tools \
      ca-certificates git curl pkg-config && \
    rm -rf /var/lib/apt/lists/*


# If you also target UEFI, uncomment the next line for GRUB EFI tools
# RUN apt-get update && apt-get install --no-install-recommends -y grub-efi-amd64-bin && rm -rf /var/lib/apt/lists/*
