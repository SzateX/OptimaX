import os
import re
import struct
from pathlib import Path

import pytest
from unicorn import Uc, UC_ARCH_X86, UC_MODE_16
from unicorn.x86_const import (
    UC_X86_REG_AX,
    UC_X86_REG_BX,
    UC_X86_REG_CH,
    UC_X86_REG_CL,
    UC_X86_REG_CS,
    UC_X86_REG_CX,
    UC_X86_REG_DH,
    UC_X86_REG_DS,
    UC_X86_REG_ES,
    UC_X86_REG_IP,
    UC_X86_REG_SI,
    UC_X86_REG_DI,
    UC_X86_REG_SS,
    UC_X86_REG_SP,
    UC_X86_REG_EBX,
)


ORG = 0xF000  # second stage is assembled with org 0xF000
RET_STOP = 0xFFFE  # sentinel return address to stop emulation
MEM_SIZE = 0x200000


def _candidate_dirs():
    env_dir = os.environ.get("BOOTLOADER_BUILD_DIR")
    if env_dir:
        yield Path(env_dir)
    for name in (
        "cmake-build-debug-docker",
        "cmake-build-release-docker",
        "build",
        ".",
    ):
        yield Path(name)


def find_artifact(filename: str) -> Path | None:
    for base in _candidate_dirs():
        candidate = base / "bootloader_output" / filename
        if candidate.exists():
            return candidate
    return None


def load_symbols(lst_path: Path) -> dict[str, int]:
    symbols: dict[str, int] = {}
    addr_pattern = re.compile(r"^\s*\d+\s+([0-9A-Fa-f]{8})")
    label_pattern = re.compile(r"\b([A-Za-z_\.][\w\.]*)\s*:")
    current_addr: int | None = None
    pending_labels: list[str] = []

    for line in lst_path.read_text().splitlines():
        addr_match = addr_pattern.match(line)
        if addr_match:
            current_addr = int(addr_match.group(1), 16)
            if pending_labels:
                for lbl in pending_labels:
                    symbols[lbl] = current_addr
                pending_labels.clear()

        label_match = label_pattern.search(line)
        if label_match and current_addr is not None:
            pending_labels.append(label_match.group(1))

    if not symbols:
        raise RuntimeError(f"No symbols parsed from {lst_path}")
    return symbols


@pytest.fixture(scope="session")
def stage2_image():
    bin_path = find_artifact("secondst.bin")
    lst_path = find_artifact("secondst.lst")
    if not bin_path or not lst_path:
        pytest.skip("second stage artifacts not found; build the project first")
    symbols = load_symbols(lst_path)
    binary = bin_path.read_bytes()
    return {"bin": binary, "symbols": symbols}


def resolve_symbol(symbols: dict[str, int], name: str) -> int:
    addr = symbols.get(name)
    if addr is None:
        raise KeyError(f"Symbol '{name}' not found in listing")
    # Ensure we have a linear address that matches where we load the image.
    if addr < ORG:
        addr += ORG
    return addr


def run_function(image, name: str, *, ram_writes=(), regs=None, max_steps=20000) -> Uc:
    symbols = image["symbols"]
    binary = image["bin"]
    start_addr = resolve_symbol(symbols, name)

    uc = Uc(UC_ARCH_X86, UC_MODE_16)
    uc.mem_map(0, MEM_SIZE)
    uc.mem_write(ORG, binary)

    # Seed RAM regions.
    for addr, data in ram_writes:
        uc.mem_write(addr, data)

    # Default segments/registers.
    uc.reg_write(UC_X86_REG_CS, 0)
    uc.reg_write(UC_X86_REG_DS, 0)
    uc.reg_write(UC_X86_REG_ES, 0)
    uc.reg_write(UC_X86_REG_SS, 0)
    uc.reg_write(UC_X86_REG_SP, 0x8000)

    if regs:
        for reg, val in regs.items():
            uc.reg_write(reg, val)

    # Push sentinel return address so RET stops at RET_STOP.
    sp = uc.reg_read(UC_X86_REG_SP)
    uc.mem_write(sp, struct.pack("<H", RET_STOP))

    uc.emu_start(start_addr, RET_STOP, count=max_steps)
    return uc


def test_lba_to_chs_basic(stage2_image):
    uc = run_function(stage2_image, "lba_to_chs", regs={UC_X86_REG_AX: 0})
    assert uc.reg_read(UC_X86_REG_CH) == 0  # cylinder
    assert uc.reg_read(UC_X86_REG_DH) == 0  # head
    assert uc.reg_read(UC_X86_REG_CL) & 0x3F == 1  # sector


def test_lba_to_chs_next_head(stage2_image):
    uc = run_function(stage2_image, "lba_to_chs", regs={UC_X86_REG_AX: 18})
    assert uc.reg_read(UC_X86_REG_CH) == 0
    assert uc.reg_read(UC_X86_REG_DH) == 1  # head 1
    assert uc.reg_read(UC_X86_REG_CL) & 0x3F == 1  # sector 1


def test_cluster_to_sector(stage2_image):
    uc = run_function(stage2_image, "cluster_to_sector", regs={UC_X86_REG_AX: 2})
    assert uc.reg_read(UC_X86_REG_AX) == 33  # data area starts at sector 33


def test_find_file(stage2_image):
    root_addr = 0x2000
    name_addr = 0x1800
    # Build root directory with a single valid entry.
    root = bytearray(224 * 32)
    filename = b"KERNEL  BIN"
    root[0:11] = filename
    struct.pack_into("<H", root, 0x1A, 5)  # starting cluster
    struct.pack_into("<I", root, 0x1C, 0x600)  # file size

    ram_writes = [
        (root_addr, bytes(root)),
        (name_addr, filename),
    ]
    uc = run_function(
        stage2_image,
        "find_file",
        ram_writes=ram_writes,
        regs={
            UC_X86_REG_SI: root_addr,
            UC_X86_REG_DI: name_addr,
        },
    )
    assert uc.reg_read(UC_X86_REG_AX) & 0xFF == 1  # AL = success
    assert uc.reg_read(UC_X86_REG_CX) == 5  # starting cluster
    assert uc.reg_read(UC_X86_REG_EBX) == 0x600  # file size


def test_sort_memory_map(stage2_image):
    mmap_addr = 0x3000
    entry_size = 24
    # Three entries out of order by base address.
    entries = [
        (0x300000, 0x1000, 1),  # base, length, type
        (0x100000, 0x1000, 1),
        (0x200000, 0x1000, 1),
    ]
    mmap = bytearray(entry_size * len(entries))
    for idx, (base, length, typ) in enumerate(entries):
        off = idx * entry_size
        struct.pack_into("<I", mmap, off + 0x0, base & 0xFFFFFFFF)
        struct.pack_into("<I", mmap, off + 0x4, base >> 32)
        struct.pack_into("<I", mmap, off + 0x8, length & 0xFFFFFFFF)
        struct.pack_into("<I", mmap, off + 0xC, length >> 32)
        struct.pack_into("<I", mmap, off + 0x10, typ)

    uc = run_function(
        stage2_image,
        "sort_memory_map",
        ram_writes=[(mmap_addr, bytes(mmap))],
        regs={
            UC_X86_REG_ES: 0,
            UC_X86_REG_DS: 0,
            UC_X86_REG_ES: 0,
            UC_X86_REG_SI: mmap_addr,
            UC_X86_REG_CX: len(entries),
        },
    )

    sorted_blob = uc.mem_read(mmap_addr, len(mmap))
    sorted_entries = []
    for idx in range(len(entries)):
        off = idx * entry_size
        base_low = struct.unpack_from("<I", sorted_blob, off + 0x0)[0]
        base_high = struct.unpack_from("<I", sorted_blob, off + 0x4)[0]
        sorted_entries.append((base_high << 32) | base_low)

    assert sorted_entries == sorted(sorted_entries)


def test_combine_overlapping_priority(stage2_image):
    mmap_addr = 0x4000
    entry_size = 24
    # Sorted input: usable region overlapped by reserved (higher priority).
    entries = [
        (0x00000000, 0x2000, 1),  # usable
        (0x00001000, 0x3000, 2),  # reserved overlaps upper half
    ]
    mmap = bytearray(entry_size * len(entries))
    for idx, (base, length, typ) in enumerate(entries):
        off = idx * entry_size
        struct.pack_into("<I", mmap, off + 0x0, base & 0xFFFFFFFF)
        struct.pack_into("<I", mmap, off + 0x4, base >> 32)
        struct.pack_into("<I", mmap, off + 0x8, length & 0xFFFFFFFF)
        struct.pack_into("<I", mmap, off + 0xC, length >> 32)
        struct.pack_into("<I", mmap, off + 0x10, typ)

    uc = run_function(
        stage2_image,
        "combine_entries_in_memory_map",
        ram_writes=[(mmap_addr, bytes(mmap))],
        regs={
            UC_X86_REG_ES: 0,
            UC_X86_REG_DS: 0,
            UC_X86_REG_SI: mmap_addr,
            UC_X86_REG_CX: len(entries),
        },
    )

    combined_blob = uc.mem_read(mmap_addr, entry_size)
    base_low = struct.unpack_from("<I", combined_blob, 0x0)[0]
    length_low = struct.unpack_from("<I", combined_blob, 0x8)[0]
    typ = struct.unpack_from("<I", combined_blob, 0x10)[0]

    # Expect merged range anchored at 0 with reserved type (2) winning the overlap.
    assert base_low == 0x0
    assert length_low >= 0x3000  # should at least cover the higher-priority region
    assert typ == 2
