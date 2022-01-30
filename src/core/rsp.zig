//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! rsp.zig - Reality Signal Processor interpreter module. 
//!

const std = @import("std");

const info = std.log.info;
const warn = std.log.warn;

const bus = @import("bus.zig");

const mi = @import("mi.zig");

const InterruptSource = mi.InterruptSource;

const RSPMemory = enum(u64) {
    DMEM = 0x00,
    IMEM = 0x01,
    IO   = 0x40,
    PC   = 0x80,
};

const RSPReg = enum(u64) {
    RSPAddr     = 0x00,
    RSPDRAMAddr = 0x04,
    RSPDMALenRD = 0x08,
    RSPDMALenWR = 0x0C,
    RSPStatus   = 0x10,
};

const RSPAddr = packed struct {
    addr  : u12  = 0,
    isIMEM: bool = false,
};

const RSPDMALen = packed struct {
    length: u12 = 0,
    count : u8  = 0,
    skip  : u12 = 0,
};

const RSPStatus = packed struct {
    h  : bool = true,
    b  : bool = false,
    db : bool = false,
    df : bool = false,
    iof: bool = false,
    ss : bool = false,
    ib : bool = false,
    s0 : bool = false,
    s1 : bool = false,
    s2 : bool = false,
    s3 : bool = false,
    s4 : bool = false,
    s5 : bool = false,
    s6 : bool = false,
    s7 : bool = false,
};

const RSPRegs = struct {
    rspAddr  : RSPAddr   = RSPAddr{},
    rspDMALen: RSPDMALen = RSPDMALen{},
    rspStatus: RSPStatus = RSPStatus{},

    rspDRAMAddr: u24 = 0,

    pc: u12 = undefined,
};

// RSP memory
pub var spDMEM: [0x1000]u8 = undefined;
pub var spIMEM: [0x1000]u8 = undefined;

var rspRegs = RSPRegs{};

var rspIRQ = false;

pub fn read8(pAddr: u64) u8 {
    var data: u8 = undefined;

    switch ((pAddr >> 12) & 0xFF) {
        @enumToInt(RSPMemory.DMEM) => {
            data = spDMEM[pAddr & 0xFFF];
        },
        @enumToInt(RSPMemory.IMEM) => {
            data = spIMEM[pAddr & 0xFFF];
        },
        else => {
            warn("[RSP] Unhandled read8 @ pAddr {X}h.", .{pAddr});

            @panic("unhandled RSP read");
        }
    }

    return data;
}

pub fn read32(pAddr: u64) u32 {
    var data: u32 = undefined;

    switch ((pAddr >> 12) & 0xFF) {
        @enumToInt(RSPMemory.DMEM) => {
            @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &spDMEM[pAddr & 0xFFF]), 4);
            data = @byteSwap(u32, data);
        },
        @enumToInt(RSPMemory.IMEM) => {
            @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &spIMEM[pAddr & 0xFFF]), 4);
            data = @byteSwap(u32, data);
        },
        @enumToInt(RSPMemory.IO) => {
            switch (pAddr & 0xFF) {
                @enumToInt(RSPReg.RSPStatus) => {
                    info("[RSP] Read32 @ pAddr {X}h (RSP Status).", .{pAddr});

                    data = @intCast(u32, @bitCast(u15, rspRegs.rspStatus));
                },
                else => {
                    warn("[RSP] Unhandled read32 @ pAddr {X}h.", .{pAddr});

                    @panic("unhandled RSP read");
                }
            }
        },
        @enumToInt(RSPMemory.PC) => {
            info("[RSP] Read32 @ pAddr {X}h (RSP PC).", .{pAddr});

            data = @intCast(u32, rspRegs.pc);
        },
        else => {
            warn("[RSP] Unhandled read32 @ pAddr {X}h.", .{pAddr});

            @panic("unhandled RSP read");
        }
    }

    return data;
}

pub fn write8(pAddr: u64, data: u8) void {
    switch ((pAddr >> 12) & 0xFF) {
        @enumToInt(RSPMemory.DMEM) => {
            spDMEM[pAddr & 0xFFF] = data;
        },
        @enumToInt(RSPMemory.IMEM) => {
            spIMEM[pAddr & 0xFFF] = data;
        },
        else => {
            warn("[RSP] Unhandled write8 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

            @panic("unhandled RSP write");
        }
    }
}

pub fn write32(pAddr: u64, data: u32) void {
    switch ((pAddr >> 12) & 0xFF) {
        @enumToInt(RSPMemory.DMEM) => {
            const data_ = @byteSwap(u32, data);
            @memcpy(@ptrCast([*]u8, &spDMEM[pAddr & 0xFFF]), @ptrCast([*]const u8, &data_), 4);
        },
        @enumToInt(RSPMemory.IMEM) => {
            const data_ = @byteSwap(u32, data);
            @memcpy(@ptrCast([*]u8, &spIMEM[pAddr & 0xFFF]), @ptrCast([*]const u8, &data_), 4);
        },
        @enumToInt(RSPMemory.IO) => {
            switch (pAddr & 0xFF) {
                @enumToInt(RSPReg.RSPAddr) => {
                    info("[RSP] Write32 @ pAddr {X}h (RSP DMEM/IMEM Address), data: {X}h.", .{pAddr, data});

                    rspRegs.rspAddr = @bitCast(RSPAddr, @truncate(u13, data));
                },
                @enumToInt(RSPReg.RSPDRAMAddr) => {
                    info("[RSP] Write32 @ pAddr {X}h (RSP DRAM Address), data: {X}h.", .{pAddr, data});

                    rspRegs.rspDRAMAddr = @truncate(u24, data);
                },
                @enumToInt(RSPReg.RSPDMALenRD) => {
                    info("[RSP] Write32 @ pAddr {X}h (RSP DMA Length RD), data: {X}h.", .{pAddr, data});

                    rspRegs.rspDMALen = @bitCast(RSPDMALen, data);

                    doDMAToRSP();
                },
                @enumToInt(RSPReg.RSPStatus) => {
                    info("[RSP] Write32 @ pAddr {X}h (RSP Status), data: {X}h.", .{pAddr, data});

                    if ((data & (1 <<  0)) != 0) rspRegs.rspStatus.h  = false;
                    if ((data & (1 <<  1)) != 0) rspRegs.rspStatus.h  = true;
                    if ((data & (1 <<  2)) != 0) rspRegs.rspStatus.b  = false;
                    if ((data & (1 <<  3)) != 0) rspIRQ = false;
                    if ((data & (1 <<  4)) != 0) rspIRQ = true;
                    if ((data & (1 <<  5)) != 0) rspRegs.rspStatus.ss = false;
                    if ((data & (1 <<  6)) != 0) rspRegs.rspStatus.ss = true;
                    if ((data & (1 <<  7)) != 0) rspRegs.rspStatus.ib = false;
                    if ((data & (1 <<  8)) != 0) rspRegs.rspStatus.ib = true;
                    if ((data & (1 <<  9)) != 0) rspRegs.rspStatus.s0 = false;
                    if ((data & (1 << 10)) != 0) rspRegs.rspStatus.s0 = true;
                    if ((data & (1 << 11)) != 0) rspRegs.rspStatus.s1 = false;
                    if ((data & (1 << 12)) != 0) rspRegs.rspStatus.s1 = true;
                    if ((data & (1 << 13)) != 0) rspRegs.rspStatus.s2 = false;
                    if ((data & (1 << 14)) != 0) rspRegs.rspStatus.s2 = true;
                    if ((data & (1 << 15)) != 0) rspRegs.rspStatus.s3 = false;
                    if ((data & (1 << 16)) != 0) rspRegs.rspStatus.s3 = true;
                    if ((data & (1 << 17)) != 0) rspRegs.rspStatus.s4 = false;
                    if ((data & (1 << 18)) != 0) rspRegs.rspStatus.s4 = true;
                    if ((data & (1 << 19)) != 0) rspRegs.rspStatus.s5 = false;
                    if ((data & (1 << 20)) != 0) rspRegs.rspStatus.s5 = true;
                    if ((data & (1 << 21)) != 0) rspRegs.rspStatus.s6 = false;
                    if ((data & (1 << 22)) != 0) rspRegs.rspStatus.s6 = true;
                    if ((data & (1 << 23)) != 0) rspRegs.rspStatus.s7 = false;
                    if ((data & (1 << 24)) != 0) rspRegs.rspStatus.s7 = true;

                    if (rspIRQ) {
                        mi.setPending(InterruptSource.SP);
                    } else {
                        mi.clearPending(InterruptSource.SP);
                    }

                    if (!rspRegs.rspStatus.h) {
                        @panic("rsp is running");
                    }
                },
                else => {
                    warn("[RSP] Unhandled write32 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

                    @panic("unhandled RSP write");
                }
            }
        },
        @enumToInt(RSPMemory.PC) => {
            info("[RSP] Write32 @ pAddr {X}h (RSP Status), data: {X}h.", .{pAddr, data});

            rspRegs.pc = @truncate(u12, data);
        },
        else => {
            warn("[RSP] Unhandled write32 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

            @panic("unhandled RSP write");
        }
    }
}

fn doDMAToRSP() void {
    const ramAddr = @intCast(u64, rspRegs.rspDRAMAddr);
    const rspAddr = @intCast(u64, rspRegs.rspAddr.addr);

    const length = @intCast(u64, rspRegs.rspDMALen.length) + 1;
    const count  = @intCast(u64, rspRegs.rspDMALen.count ) + 1;
    const skip   = @intCast(u64, rspRegs.rspDMALen.skip);

    var mem: ?*[0x1000]u8 = null;

    if (rspRegs.rspAddr.isIMEM) {
        info("[RSP] RAM->IMEM DMA, DRAM pAddr: {X}h, IMEM pAddr: {X}h, length: {}.", .{ramAddr, rspAddr, length});

        mem = &spIMEM;
    } else {
        info("[RSP] RAM->DMEM DMA, DRAM pAddr: {X}h, DMEM pAddr: {X}h, length: {}.", .{ramAddr, rspAddr, length});

        mem = &spDMEM;
    }

    var count_: u64 = 0;
    while (count_ < count) : (count_ += 1) {
        var length_: u64 = 0;
        while (length_ < length) : (length_ += 1) {
            mem.?[count_ * length + length_] = bus.ram[count_ * skip * length + length_];
        }
    }

    rspIRQ = true;

    mi.setPending(InterruptSource.SP);
}
