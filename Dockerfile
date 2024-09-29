FROM debian:bullseye-backports
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    	clang lld qemu qemu-utils qemu-system-x86 qemu-system-gui qemu-system-i386 gcc gdb  make nasm \
          libguestfs-tools \
          qemu-utils \
          linux-image-generic \
          mtools \
          python3 python3-pip python3-guestfs && pip3 install cmake