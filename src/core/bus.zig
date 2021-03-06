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

const ai = @import("ai.zig");
const mi = @import("mi.zig");
const si = @import("si.zig");
const vi = @import("vi.zig");

const pif = @import("pif.zig");
const rsp = @import("rsp.zig");

const MemoryRegion = enum(u64) {
    RDRAM0 = 0x000, RDRAM1 = 0x001, RDRAM2 = 0x002, RDRAM3 = 0x003,
    RDRAM4 = 0x004, RDRAM5 = 0x005, RDRAM6 = 0x006, RDRAM7 = 0x007,

    RDRAMIO = 0x03F,
    RSP     = 0x040,
    MI      = 0x043,
    VI      = 0x044,
    AI      = 0x045,
    PI      = 0x046,
    RI      = 0x047,
    SI      = 0x048,

    CartS = 0x100,
    CartE = 0x1FB,

    PIF = 0x1FC,
};

// TODO: write own module for PI stuff
const PIReg = enum(u64) {
    DRAMAddr = 0x00,
    CartAddr = 0x04,
    ReadLen  = 0x08,
    WriteLen = 0x0C,
    PIStatus = 0x10,
};

const PIStatus = packed struct {
    dmaBusy : bool = false,
    ioBusy  : bool = false,
    dmaError: bool = false,
    dmaIRQ  : bool = false,
    _pad0   :  u12 = 0,
    _pad1   :  u16 = 0,
};

const PIRegs = struct {
    dramAddr: u32 = undefined, cartAddr: u32 = undefined, writeLen: u32 = undefined,
    piStatus: PIStatus = PIStatus{},
};

var piRegs = PIRegs{};

pub var ram: []u8 = undefined; // RDRAM
var rom: []u8 = undefined; // Cartridge ROM

/// Initialize bus module
pub fn init(alloc: std.mem.Allocator, romPath: []const u8, isFastBoot: bool) anyerror!void {
    ram = try alloc.alloc(u8, 0x80_0000);
    
    // Load ROM file
    const romFile = try std.fs.cwd().openFile(romPath, .{.read = true},);
    defer romFile.close();

    rom = try romFile.reader().readAllAlloc(alloc, 0x80_0000);

    if (isFastBoot) {
        // Copy first 1000h bytes of ROM to SP DMEM
        @memcpy(@ptrCast([*]u8, &rsp.spDMEM), @ptrCast([*]u8, rom), 0x1000);
    }
}

/// Deinitialize bus module
pub fn deinit(alloc: std.mem.Allocator) void {
    alloc.free(ram);
    alloc.free(rom);
}

pub fn read8(pAddr: u64) u8 {
    var data: u8 = undefined;

    switch (pAddr >> 20) {
        @enumToInt(MemoryRegion.RDRAM0) ... @enumToInt(MemoryRegion.RDRAM7) => {
            data = ram[pAddr & 0x7F_FFFF];
        },
        @enumToInt(MemoryRegion.RSP) => {
            data = rsp.read8(pAddr);
        },
        else => {
            std.log.warn("[Bus] Unhandled read8 @ pAddr {X}h.", .{pAddr});

            unreachable;
        }
    }

    return data;
}

pub fn read16(pAddr: u64) u16 {
    if ((pAddr & 1) != 0) @panic("unaligned read16");

    var data: u16 = undefined;

    switch (pAddr >> 20) {
        @enumToInt(MemoryRegion.RDRAM0) ... @enumToInt(MemoryRegion.RDRAM7) => {
            @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &ram[pAddr & 0x7F_FFFF]), 2);
            data = @byteSwap(u16, data);
        },
        @enumToInt(MemoryRegion.RSP) => {
            data = rsp.read16(pAddr);
        },
        else => {
            std.log.warn("[Bus] Unhandled read16 @ pAddr {X}h.", .{pAddr});

            unreachable;
        }
    }

    return data;
}

pub fn read32(pAddr: u64) u32 {
    if ((pAddr & 3) != 0) @panic("unaligned read32");

    var data: u32 = undefined;

    switch (pAddr >> 20) {
        @enumToInt(MemoryRegion.RDRAM0) ... @enumToInt(MemoryRegion.RDRAM7) => {
            @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &ram[pAddr & 0x7F_FFFF]), 4);
            data = @byteSwap(u32, data);
        },
        @enumToInt(MemoryRegion.RDRAMIO) => {
            switch (pAddr & 0xFF_FFFF) {
                else => {
                    std.log.warn("[Bus] Unhandled read32 @ pAddr {X}h (RDRAM I/O).", .{pAddr});

                    data = 0;
                }
            }
        },
        @enumToInt(MemoryRegion.RSP) => {
            data = rsp.read32(pAddr);
        },
        @enumToInt(MemoryRegion.MI) => {
            data = mi.read32(pAddr);
        },
        @enumToInt(MemoryRegion.VI) => {
            data = vi.read32(pAddr);
        },
        @enumToInt(MemoryRegion.AI) => {
            data = ai.read32(pAddr);
        },
        @enumToInt(MemoryRegion.PI) => {
            switch (pAddr & 0xF_FFFF) {
                @enumToInt(PIReg.PIStatus) => {
                    std.log.info("[Bus] Read32 @ pAddr {X}h (PI Status).", .{pAddr});

                    data = @bitCast(u32, piRegs.piStatus);
                },
                else => {
                    std.log.warn("[Bus] Unhandled read32 @ pAddr {X}h (Peripheral Interface).", .{pAddr});
                }
            }
        },
        @enumToInt(MemoryRegion.RI) => {
            switch (pAddr & 0xF_FFFF) {
                else => {
                    std.log.warn("[Bus] Unhandled read32 @ pAddr {X}h (RDRAM Interface).", .{pAddr});

                    data = 0;
                }
            }
        },
        @enumToInt(MemoryRegion.SI) => {
            data = si.read32(pAddr);
        },
        @enumToInt(MemoryRegion.CartS) ... @enumToInt(MemoryRegion.CartE) => {
            @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &rom[pAddr - 0x1000_0000]), 4);
            data = @byteSwap(u32, data);
        },
        @enumToInt(MemoryRegion.PIF) => {
            if (pAddr >= 0x1FC0_07C0 and pAddr < 0x1FC0_0800) {
                std.log.info("[Bus] Read32 @ pAddr {X}h (PIF RAM).", .{pAddr});

                @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &pif.pifRAM[pAddr - 0x1FC0_07C0]), 4);
                data = @byteSwap(u32, data);
            } else {
                std.log.warn("[Bus] Unhandled write32 @ pAddr {X}h (PIF), data: {X}h.", .{pAddr, data});

                unreachable;
            }
        },
        else => {
            std.log.warn("[Bus] Unhandled read32 @ pAddr {X}h.", .{pAddr});

            unreachable;
        }
    }

    return data;
}

pub fn read64(pAddr: u64) u64 {
    if ((pAddr & 7) != 0) @panic("unaligned read164");

    var data: u64 = undefined;

    switch (pAddr >> 20) {
        @enumToInt(MemoryRegion.RDRAM0) ... @enumToInt(MemoryRegion.RDRAM7) => {
            @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &ram[pAddr & 0x7F_FFFF]), 8);
            data = @byteSwap(u64, data);
        },
        @enumToInt(MemoryRegion.MI) => {
            if (pAddr == 0x4300028) return data;
            std.log.warn("[MI] Unhandled read64 @ pAddr {X}h.", .{pAddr});

            @panic("mi: unhandled read64");
        },
        else => {
            std.log.warn("[Bus] Unhandled read64 @ pAddr {X}h.", .{pAddr});

            unreachable;
        }
    }

    return data;
}

pub fn write8(pAddr: u64, data: u8) void {
    switch (pAddr >> 20) {
        @enumToInt(MemoryRegion.RDRAM0) ... @enumToInt(MemoryRegion.RDRAM7) => {
            ram[pAddr & 0x7F_FFFF] = data;
        },
        @enumToInt(MemoryRegion.RSP) => {
            rsp.write8(pAddr, data);
        },
        else => {
            std.log.warn("[Bus] Unhandled write8 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

            unreachable;
        }
    }
}

pub fn write16(pAddr: u64, data: u16) void {
    if ((pAddr & 1) != 0) @panic("unaligned write16");

    switch (pAddr >> 20) {
        @enumToInt(MemoryRegion.RDRAM0) ... @enumToInt(MemoryRegion.RDRAM7) => {
            const data_ = @byteSwap(u16, data);

            @memcpy(@ptrCast([*]u8, &ram[pAddr & 0x7F_FFFF]), @ptrCast([*]const u8, &data_), 2);
        },
        else => {
            std.log.warn("[Bus] Unhandled write16 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

            unreachable;
        }
    }
}

var siDRAMAddr: u32 = 0;

pub fn write32(pAddr: u64, data: u32) void {
    if ((pAddr & 3) != 0) @panic("unaligned write32");

    switch (pAddr >> 20) {
        @enumToInt(MemoryRegion.RDRAM0) ... @enumToInt(MemoryRegion.RDRAM7) => {
            const data_ = @byteSwap(u32, data);

            @memcpy(@ptrCast([*]u8, &ram[pAddr & 0x7F_FFFF]), @ptrCast([*]const u8, &data_), 4);
        },
        @enumToInt(MemoryRegion.RDRAMIO) => {
            switch (pAddr & 0xFF_FFFF) {
                else => {
                    std.log.warn("[Bus] Unhandled write32 @ pAddr {X}h (RDRAM I/O), data: {X}h.", .{pAddr, data});
                }
            }
        },
        @enumToInt(MemoryRegion.RSP) => {
            rsp.write32(pAddr, data);
        },
        @enumToInt(MemoryRegion.MI) => {
            mi.write32(pAddr, data);
        },
        @enumToInt(MemoryRegion.VI) => {
            vi.write32(pAddr, data);
        },
        @enumToInt(MemoryRegion.AI) => {
            ai.write32(pAddr, data);
        },
        @enumToInt(MemoryRegion.PI) => {
            switch (pAddr & 0xF_FFFF) {
                @enumToInt(PIReg.DRAMAddr) => {
                    std.log.info("[Bus] Write32 @ pAddr {X}h (PI DRAM Address), data: {X}h.", .{pAddr, data});

                    piRegs.dramAddr = data & 0xFF_FFFF;
                },
                @enumToInt(PIReg.CartAddr) => {
                    std.log.info("[Bus] Write32 @ pAddr {X}h (PI Cart Address), data: {X}h.", .{pAddr, data});

                    piRegs.cartAddr = data;
                },
                @enumToInt(PIReg.ReadLen) => {
                    std.log.info("[Bus] Write32 @ pAddr {X}h (PI Read Length), data: {X}h.", .{pAddr, data});

                    piRegs.piStatus.dmaIRQ = true;

                    mi.setPending(mi.InterruptSource.PI);

                    @panic("blah");
                },
                @enumToInt(PIReg.WriteLen) => {
                    std.log.info("[Bus] Write32 @ pAddr {X}h (PI Write Length), data: {X}h.", .{pAddr, data});

                    piRegs.writeLen = data & 0xFF_FFFF;

                    // Start PI DMA (Cartridge => RDRAM)
                    @memcpy(@ptrCast([*]u8, &ram[piRegs.dramAddr]), @ptrCast([*]u8, &rom[piRegs.cartAddr & 0xFFF_FFFF]), piRegs.writeLen +% 1);

                    piRegs.piStatus.dmaIRQ = true;

                    mi.setPending(mi.InterruptSource.PI);
                },
                @enumToInt(PIReg.PIStatus) => {
                    std.log.info("[Bus] Write32 @ pAddr {X}h (PI Status), data: {X}h.", .{pAddr, data});

                    if ((data & 2) != 0) {
                        piRegs.piStatus.dmaIRQ = false;

                        mi.clearPending(mi.InterruptSource.PI);
                    }
                },
                else => {
                    std.log.warn("[Bus] Unhandled write32 @ pAddr {X}h (Peripheral Interface), data: {X}h.", .{pAddr, data});
                }
            }
        },
        @enumToInt(MemoryRegion.RI) => {
            switch (pAddr & 0xF_FFFF) {
                else => {
                    std.log.warn("[Bus] Unhandled write32 @ pAddr {X}h (RDRAM Interface), data: {X}h.", .{pAddr, data});
                }
            }
        },
        @enumToInt(MemoryRegion.SI) => {
            si.write32(pAddr, data);
        },
        @enumToInt(MemoryRegion.PIF) => {
            if (pAddr >= 0x1FC0_07C0 and pAddr < 0x1FC0_0800) {
                std.log.info("[Bus] Write32 @ pAddr {X}h (PIF RAM), data: {X}h.", .{pAddr, data});

                const data_ = @byteSwap(u32, data);

                @memcpy(@ptrCast([*]u8, &pif.pifRAM[pAddr - 0x1FC0_07C0]), @ptrCast([*]const u8, &data_), 4);
            } else {
                std.log.warn("[Bus] Unhandled write32 @ pAddr {X}h (PIF), data: {X}h.", .{pAddr, data});

                unreachable;
            }
        },
        else => {
            std.log.warn("[Bus] Unhandled write32 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

            unreachable;
        }
    }
}

pub fn write64(pAddr: u64, data: u64) void {
    if ((pAddr & 7) != 0) @panic("unaligned write64");

    switch (pAddr >> 20) {
        @enumToInt(MemoryRegion.RDRAM0) ... @enumToInt(MemoryRegion.RDRAM7) => {
            const data_ = @byteSwap(u64, data);

            @memcpy(@ptrCast([*]u8, &ram[pAddr & 0x7F_FFFF]), @ptrCast([*]const u8, &data_), 8);
        },
        else => {
            std.log.warn("[Bus] Unhandled write64 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

            unreachable;
        }
    }
}
