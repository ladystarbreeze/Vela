//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! mi.zig - MIPS Interface module. 
//!

const std = @import("std");

const cop0 = @import("cop0.zig");

const MIReg = enum(u64) {
    MIVersion   = 0x4,
    MIInterrupt = 0x8,
    MIMask      = 0xC,
};

pub const InterruptSource = enum(u6) {
    SP = 1 << 0,
    SI = 1 << 1,
    AI = 1 << 2,
    VI = 1 << 3,
    PI = 1 << 4,
    DP = 1 << 5,
};

var miVersion: u32 = 0x0202_0102;

var miInterrupt: u6  = 0;
var miMask     : u6  = 0;

pub fn read32(pAddr: u64) u32 {
    var data: u32 = undefined;

    switch (pAddr & 0xFF) {
        @enumToInt(MIReg.MIVersion) => {
            std.log.info("[MI] Read32 @ pAddr {X}h (MI Version).", .{pAddr});

            data = miVersion;
        },
        @enumToInt(MIReg.MIInterrupt) => {
            std.log.info("[MI] Read32 @ pAddr {X}h (MI Interrupt).", .{pAddr});

            data = @intCast(u32, miInterrupt);
        },
        @enumToInt(MIReg.MIMask) => {
            std.log.info("[MI] Read32 @ pAddr {X}h (MI Mask).", .{pAddr});

            data = @intCast(u32, miMask);
        },
        else => {
            std.log.warn("[MI] Unhandled read32 @ pAddr {X}h.", .{pAddr});
        }
    }

    return data;
}

pub fn write32(pAddr: u64, data: u32) void {
    switch (pAddr & 0xFF) {
        @enumToInt(MIReg.MIMask) => {
            std.log.info("[MI] Write32 @ pAddr {X}h (MI Mask), data: {X}h.", .{pAddr, data});

            if ((data & (1 << 0)) != 0) miMask &= ~@enumToInt(InterruptSource.SP);
            if ((data & (1 << 1)) != 0) miMask |=  @enumToInt(InterruptSource.SP);
            if ((data & (1 << 2)) != 0) miMask &= ~@enumToInt(InterruptSource.SI);
            if ((data & (1 << 3)) != 0) miMask |=  @enumToInt(InterruptSource.SI);
            if ((data & (1 << 6)) != 0) miMask &= ~@enumToInt(InterruptSource.VI);
            if ((data & (1 << 7)) != 0) miMask |=  @enumToInt(InterruptSource.VI);
            if ((data & (1 << 8)) != 0) miMask &= ~@enumToInt(InterruptSource.PI);
            if ((data & (1 << 9)) != 0) miMask |=  @enumToInt(InterruptSource.PI);

            checkForInterrupts();
        },
        else => {
            std.log.warn("[MI] Unhandled write32 @ pAddr {X}h, data: {X}h.", .{pAddr, data});
        }
    }
}

fn checkForInterrupts() void {
    if ((miInterrupt & miMask) != 0) {
        std.log.info("[MI] INTR: {X}h, MASK: {X}h", .{miInterrupt, miMask});

        cop0.setPending(2);
    } else {
        cop0.clearPending(2);
    }
}

pub fn setPending(i: InterruptSource) void {
    miInterrupt |= @enumToInt(i);

    checkForInterrupts();
}

pub fn clearPending(i: InterruptSource) void {
    miInterrupt &= ~@enumToInt(i);

    checkForInterrupts();
}
