//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! bus.zig - System bus module. 
//!

//  Physical Memory Map (from n64.readthedocs.io)
//    Address  Range    |   Name   | Description
//  00000000h-003FFFFFh |  RDRAM   | Built-in
//  00400000h-007FFFFFh |  RDRAM   | Expansion Pak
//  03F00000h-03FFFFFFh | RDRAM IO | RDRAM MMIO
//  04000000h-04000FFFh | SP DMEM  | RSP Data Memory
//  04001000h-04001FFFh | SP IMEM  | RSP Instruction Memory
//  04040000h-040FFFFFh |  SP IO   | RSP DMA, PC, ...
//  04100000h-041FFFFFh |  DP COM  | RDP Command
//  04200000h-042FFFFFh | DP SPAN  | ???
//  04300000h-043FFFFFh |    MI    | MIPS Interface
//  04400000h-044FFFFFh |    VI    | Video Interface
//  04500000h-045FFFFFh |    AI    | Audio Interface 
//  04600000h-046FFFFFh |    PI    | Peripheral Interface
//  04700000h-047FFFFFh |    RI    | RDRAM Interface
//  04800000h-048FFFFFh |    SI    | Serial Interface
//  05000000h-05FFFFFFh | CART2 A1 | N64DD Control
//  06000000h-07FFFFFFh | CART1 A1 | N64DD IPL
//  08000000h-0FFFFFFFh | CART2 A2 | SRAM
//  10000000h-1FBFFFFFh | CART1 A2 | ROM
//  1FC00000h-1FC007BFh | PIF ROM  |
//  1FC007C0h-1FC007FFh | PIF RAM  |
//  1FD00000h-7FFFFFFFh | CART1 A3 |

const std = @import("std");

const MemoryRegion = enum(u64) {
    RDRAM0 = 0x000, RDRAM1 = 0x001, RDRAM2 = 0x002, RDRAM3 = 0x003,
    RDRAM4 = 0x004, RDRAM5 = 0x005, RDRAM6 = 0x006, RDRAM7 = 0x007,

    RDRAMIO = 0x03F,

    RSP = 0x040,
};

var ram: []u8 = undefined; // RDRAM
var rom: []u8 = undefined; // Cartridge ROM

// RSP memory
var spDMEM: []u8 = undefined;

/// Initialize bus module
pub fn init(alloc: std.mem.Allocator, romPath: []const u8, isFastBoot: bool) anyerror!void {
    ram = try alloc.alloc(u8, 0x40_0000);

    spDMEM = try alloc.alloc(u8, 0x1000);
    
    // Load ROM file
    const romFile = try std.fs.cwd().openFile(romPath, .{.read = true},);
    defer romFile.close();

    rom = try romFile.reader().readAllAlloc(alloc, 0x40_0000);

    if (isFastBoot) {
        // Copy first 1000h bytes of ROM to SP DMEM
        @memcpy(@ptrCast([*]u8, spDMEM), @ptrCast([*]u8, rom), 0x1000);
    }
}

/// Deinitialize bus module
pub fn deinit(alloc: std.mem.Allocator) void {
    alloc.free(ram);
    alloc.free(rom);

    alloc.free(spDMEM);
}

pub fn read32(pAddr: u64) u32 {
    const pAddr_ = pAddr & 0xFFFFFFFC;

    var data: u32 = undefined;

    switch (pAddr_ >> 20) {
        @enumToInt(MemoryRegion.RSP) => {
            switch ((pAddr_ >> 12) & 0xFF) {
                0x00 => {
                    @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &spDMEM[pAddr_ & 0xFFF]), 4);
                    data = @byteSwap(u32, data);
                },
                else => {
                    unreachable;
                }
            }
        },
        else => {
            std.log.warn("[Bus] Unhandled read32 @ pAddr {X}h.", .{pAddr_});

            unreachable;
        }
    }

    return data;
}
