#include <stdint.h>

extern void jump_to_kernel(void);
void print_string(const char* str);
void lba_to_chs(uint32_t lba, uint8_t* head, uint8_t* cylinder, uint8_t* sector);
uint8_t reset_disk(uint8_t drive);
uint8_t read_sectors_chs(uint8_t drive, uint8_t head, uint8_t cylinder, uint8_t sector, uint8_t count, uint8_t* buffer);
uint8_t read_sectors_lba(uint8_t drive, uint32_t lba, uint8_t count, uint8_t* buffer);
uint16_t cluster_to_sector(uint16_t cluster);
uint8_t read_root_directory(uint8_t drive, uint8_t* root_directory_buffer);
uint8_t find_file(const uint8_t* root_directory_buffer, const char* file_name_str, uint16_t* found_file_cluster, uint32_t* file_size);
uint8_t load_file(uint8_t drive, uint16_t file_cluster, uint32_t file_size, uint8_t* destination);
uint8_t check_a20_gate();
uint8_t enable_a20_gate_via_bios();
static inline void io_out8(uint16_t port, uint8_t value);
static inline uint8_t io_in8(uint16_t port);
void enable_A20_keyboard();
static inline void a20wait();
static inline void a20wait2();
static inline void enable_a20_fast_a20();
void enable_unreal_mode();

struct GDTPointer {
    uint16_t limit;
    uint32_t base;
} __attribute__((packed));

const uint16_t sectors_per_track = 18;
const uint16_t number_of_heads = 2;
const uint8_t sectors_per_cluster = 1;
const uint16_t bytes_per_sector = 512;
uint8_t* root_directory = (uint8_t*) 0x7E00;
uint8_t* disk_buffer = (uint8_t*) 0x7E00;
uint8_t* kernel_buffer = (uint8_t*) 0x100000;

__attribute__((aligned(8))) uint64_t gdt[] = {
        0x0000000000000000, // Null descriptor
        0x00009A000000FFFF, // Code segment descriptor
        0x00CF92000000FFFF  // Data segment descriptor
};

__attribute__((aligned(8))) uint64_t gdt_protected[] = {
        0x0000000000000000, // Null descriptor
        0x00CF9A000000FFFF, // Code segment descriptor (32-bit)
        0x00CF92000000FFFF,  // Data segment descriptor (32-bit)
        0x00019A000000FFFF, // Code segment descriptor (16-bit)
        0x000192000000FFFF  // Data segment descriptor (16-bit)

};

struct GDTPointer gdt_info = {
        .limit = sizeof(gdt) - 1,
        .base = (uint32_t)&gdt
};

struct GDTPointer gdt_protected_info = {
        .limit = sizeof(gdt_protected) - 1,
        .base  = (uint32_t)&gdt_protected
};

const char* disk_error_msg = "Disk error!\r\n";
const char* hello_second_stage_msg = "Hello second stage!\r\n";
const char* kernel_not_found_msg = "KERNEL.BIN not found!\r\n";
const char* check_a20_gate_msg = "Checking A20 gate...\r\n";
const char* enable_a20_gate_msg = "Enabling A20 gate...\r\n";
const char* a20_error_msg = "A20 Gate is not available. Critical ERROR...\r\n";
const char* a20_gate_enabled_msg = "A20 gate enabled!\r\n";
const char* jump_to_unreal_mode_msg = "Jumping to Unreal Mode...\r\n";
const char* jumped_to_unreal_mode_msg = "Jumped to Unreal Mode!\r\n";
const char* loading_kernel_msg = "Loading KERNEL.BIN...\r\n";
const char* loading_done_msg = "Loading done!\r\n";
const char* error_resetting_disk = "Error while resetting disk!\r\n";
const char* jumping_to_kernel_msg = "Jumping to KERNEL.BIN...\r\n";

const char* file_name = "KERNEL  BIN";

__attribute__((noreturn)) void c_loader(){
    // Get the drive number from DL register
    uint8_t drive_number;
    __asm__ __volatile__("mov %%dl, %0" : "=r"(drive_number));

    uint8_t status = reset_disk(drive_number);
    if (status){
        print_string(error_resetting_disk);
        goto halt;
    }

    print_string(hello_second_stage_msg);
    status = read_root_directory(drive_number, root_directory);
    if (status){
        print_string(disk_error_msg);
        goto halt;
    }
    uint16_t kernel_cluster;
    uint32_t kernel_size;
    if (!find_file(root_directory, file_name, &kernel_cluster, &kernel_size)){
        print_string(kernel_not_found_msg);
        goto halt;
    }

    print_string(check_a20_gate_msg);
    if(!check_a20_gate()){
        print_string(enable_a20_gate_msg);
        uint8_t a20_gate_enabled = enable_a20_gate_via_bios();
        if (!check_a20_gate() || !a20_gate_enabled){
            enable_A20_keyboard();
            if (!check_a20_gate()){
                enable_a20_fast_a20();
                if (!check_a20_gate()){
                    print_string(a20_error_msg);
                    goto halt;
                }
            }
        }
    }
    print_string(a20_gate_enabled_msg);

    print_string(jump_to_unreal_mode_msg);
    enable_unreal_mode();
    print_string(jumped_to_unreal_mode_msg);

    print_string(loading_kernel_msg);
    status = load_file(drive_number, kernel_cluster, kernel_size, kernel_buffer);
    if (status){
        print_string(disk_error_msg);
        goto halt;
    }
    print_string(loading_done_msg);

    print_string(jumping_to_kernel_msg);

    __asm__ __volatile__(
            "jmp jump_to_kernel"
    );

    //It doesn't work because it generates jump 0x08:0x0000 instead 0x08:0x100000. Maybe -m16 flag does wrong generating?
    /*__asm__ __volatile__ (
            "cli                            \n\t"

            "lgdt (%0)                      \n\t"

            "mov %%cr0, %%eax               \n\t"
            "or $0x1, %%eax                 \n\t"
            "mov %%eax, %%cr0               \n\t"
            "ljmp 0x8, 0x100000             \n\t"
            :
            : "r" (&gdt_protected_info)
            : "eax", "memory"
            );*/

    halt:
    for (;;)
        __asm__ __volatile__("hlt");
}

void print_string(const char* str){
    for (int i = 0; str[i] != '\0'; i++){
        register char c asm("al") = str[i];
        __asm__ __volatile__(
                "mov $0x0E, %%ah\n"
                "mov $0x00, %%bh\n"
                "mov $0x07, %%bl\n"
                "int $0x10\n"
                :
                : "al"(c)
        );
    }
}

void lba_to_chs(uint32_t lba, uint8_t* head, uint8_t* cylinder, uint8_t* sector){
    *sector = (lba % sectors_per_track) + 1;
    *head = (lba / sectors_per_track) % number_of_heads;
    *cylinder = (lba / sectors_per_track) / number_of_heads;
}

uint8_t reset_disk(uint8_t drive){
    uint16_t dx = drive;
    uint8_t status;
    __asm__ __volatile__(
            "movw %1, %%dx\n"
            "xor %%ax, %%ax\n"
            "int $0x13\n"
            "movb %%ah, %0\n"
            : "=r"(status)
            : "r"(dx)
            : "cc", "ax"
    );
    return status;
}

uint8_t read_sectors_chs(uint8_t drive, uint8_t head, uint8_t cylinder, uint8_t sector, uint8_t count, uint8_t* buffer){
    register uint8_t status;
    register uint16_t ax = (uint16_t) 0x0200 | count;
    register uint16_t dx = ((uint16_t) head << 8) | drive;
    register uint16_t cx = ((uint16_t) cylinder << 8) | sector | (uint16_t)((cylinder & 0x300) >> 2);
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wpointer-to-int-cast"
    register uint16_t si = (uint16_t) buffer;
    #pragma clang diagnostic pop
    __asm__ __volatile__(
            "mov %1, %%ax\n"
            "mov %2, %%dx\n"
            "mov %3, %%cx\n"
            "movw %4, %%si\n"
            "int $0x13\n"
            "mov %%ah, %0\n"
            : "=r"(status)
            : "a"(ax), "d"(dx), "c"(cx), "S"(si)
    );
    return status;
}

uint8_t read_sectors_lba(uint8_t drive, uint32_t lba, uint8_t count, uint8_t* buffer){
    uint8_t head, cylinder, sector;
    uint8_t sectors_to_read;

    while(count > 0){
        lba_to_chs(lba, &head, &cylinder, &sector);
        sectors_to_read = sectors_per_track - sector + 1;

        if (sectors_to_read > count)
            sectors_to_read = count;

        uint8_t status = read_sectors_chs(drive, head, cylinder, sector, sectors_to_read, buffer);
        if (status)
            return status;
        count -= sectors_to_read;
        lba += sectors_to_read;
        buffer += sectors_to_read * 512;
    }
    return 0;
}

uint16_t cluster_to_sector(uint16_t cluster){
    return (cluster - 2) * sectors_per_cluster + 33;
}

uint8_t read_root_directory(uint8_t drive, uint8_t* root_directory_buffer){
    return read_sectors_lba(drive, 19, 14, root_directory_buffer);
}

uint8_t find_file(const uint8_t* root_directory_buffer, const char* file_name_str, uint16_t* found_file_cluster, uint32_t* file_size){
    for (int i = 0; i < 224; i += 32){
        if (root_directory_buffer[i] == 0x00)
            return 0;

        if (root_directory_buffer[i] != 0xE5){
            for (int j = 0; j < 11; j++){
                if (root_directory_buffer[i + j] != file_name_str[j])
                    break;

                if (j == 10){
                    *found_file_cluster = *((uint16_t*) (root_directory_buffer + i + 0x1A));
                    *file_size = *((uint32_t*) (root_directory_buffer + i + 0x1C));
                    return 1;
                }
            }
        }
    }

    return 0;
}

uint8_t load_file(uint8_t drive, uint16_t file_cluster, uint32_t file_size, uint8_t* destination){
    uint32_t lba = cluster_to_sector(file_cluster);
    uint32_t file_size_in_sectors;
    if (file_size % bytes_per_sector == 0)
        file_size_in_sectors = file_size / bytes_per_sector;
    else
        file_size_in_sectors = file_size / bytes_per_sector + 1;

    while(file_size_in_sectors > 0){
        uint8_t actual_sectors_to_read = file_size_in_sectors >= 32 ? 32 : file_size_in_sectors;
        uint8_t status = read_sectors_lba(drive, lba, actual_sectors_to_read, disk_buffer);
        if(status != 0){
            return status;
        }
        for(int i = 0; i < actual_sectors_to_read * bytes_per_sector; i++)
            destination[i] = disk_buffer[i];
        lba += actual_sectors_to_read;
        destination += actual_sectors_to_read * bytes_per_sector;
        file_size_in_sectors -= actual_sectors_to_read;
    }
    return 0;
}

uint8_t check_a20_gate() {
    uint8_t a20_gate;
    __asm__ __volatile__(
            "pushf\n"
            "push %%ds\n"
            "push %%es\n"
            "push %%di\n"
            "push %%si\n"
            "cli\n"
            "xor %%ax, %%ax\n" //ax = 0
            "mov %%ax, %%es\n"
            "not %%ax\n" //ax = 0xFFFF
            "mov %%ax, %%ds\n"
            "mov $0x0500, %%di\n"
            "mov $0x0510, %%si\n"
            "movb %%es:(%%di), %%al\n"
            "push %%ax\n"
            "movb %%ds:(%%si), %%al\n"
            "push %%ax\n"
            "movb $0x00, %%es:(%%di)\n"
            "movb $0xFF, %%ds:(%%si)\n"
            "cmpb $0xFF, %%es:(%%di)\n"
            "pop %%ax\n"
            "movb %%al, %%ds:(%%si)\n"
            "pop %%ax\n"
            "movb %%al, %%es:(%%di)\n"
            "mov $0, %%ax\n"
            "je 1f\n"
            "mov $1, %%ax\n"
            ""
            "1:\n"
            "pop %%si\n"
            "pop %%di\n"
            "pop %%es\n"
            "pop %%ds\n"
            "popf"
            :"=a"(a20_gate)
    );
    return a20_gate;
}

uint16_t int_15h(uint16_t function, uint8_t* cflag){
    uint16_t ax;
    __asm__ volatile (
            "int $0x15\n"
            "setc %1\n"
            : "=a"(ax), "=b"(*cflag)
            : "a"(function)
            : "cc"
            );
    return ax;
}

uint8_t enable_a20_gate_via_bios(){
    uint8_t cflag;
    uint16_t result = int_15h(0x2403, &cflag);
    if(result & 0xFF00)
        return 0;

    result = int_15h(0x2402, &cflag);
    if(cflag)
        return 0;
    if(result & 0xFF00)
        return 0;
    if(result & 0xFF)
        return 1;

    result = int_15h(0x2401, &cflag);
    if(cflag)
        return 0;
    if(result & 0xFF00)
        return 0;

    return 1;
}

static inline void io_out8(uint16_t port, uint8_t value) {
    __asm__ volatile (
            "outb %0, %1"
            :
            : "a"(value), "d"(port)
            );
}

static inline uint8_t io_in8(uint16_t port) {
    uint8_t value;
    __asm__ volatile (
            "inb %1, %0"
            : "=a"(value)
            : "d"(port)
            );
    return value;
}

void enable_A20_keyboard() {
    io_out8(0x64, 0xAD);  // Disable the keyboard
    a20wait();
    io_out8(0x64, 0xD0);  // Send read command
    a20wait2();
    uint8_t response = io_in8(0x60);  // Read response
    a20wait();
    io_out8(0x64, 0xD1);  // Send write command
    a20wait();
    io_out8(0x60, response | 2);  // Write back with A20 enable bit
    a20wait();
    io_out8(0x64, 0xAE);  // Re-enable the keyboard
    a20wait();
}

static inline void a20wait() {
    while(io_in8(0x64) & 2);
}

static inline void a20wait2() {
    while(!(io_in8(0x64) & 1));
}

static inline void enable_a20_fast_a20() {
    uint8_t port_b = io_in8(0x92);
    port_b |= 2;
    io_out8(0x92, port_b);
}

void enable_unreal_mode() {
    __asm__ __volatile__ (
            "cli                            \n\t"
            "push %%ds                      \n\t"

            "lgdt (%0)                      \n\t"

            "mov %%cr0, %%eax               \n\t"
            "orb $0x1, %%al                 \n\t"
            "mov %%eax, %%cr0               \n\t"
            "ljmp $0x8, $pmode_label%=        \n\t"

            "pmode_label%=:                   \n\t"
            "movw $0x10, %%bx               \n\t"
            "movw %%bx, %%ds                \n\t"

            "andb $0xFE, %%al               \n\t"
            "mov %%eax, %%cr0               \n\t"
            "ljmp $0x0, $unreal_label%=       \n\t"

            "unreal_label%=:                  \n\t"
            "pop %%ds                       \n\t"
            :
            : "r" (&gdt_info)
            : "eax", "ebx", "memory"
            );
}