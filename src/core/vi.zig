//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! vi.zig - Video Interface module. 
//!

const std = @import("std");

const mi = @import("mi.zig");

const changeScreen = @import("n64.zig").changeScreen;

const InterruptSource = mi.InterruptSource;

const setPending   = mi.setPending;
const clearPending = mi.clearPending;

const VIReg = enum(u64) {
    VIControl  = 0x00,
    VIOrigin   = 0x04,
    VIWidth    = 0x08,
    VIIntr     = 0x0C,
    VICurrentV = 0x10,
};

const FBMode = enum(u2) {
    Blank    = 0,
    Reserved = 1,
    RGBA5553 = 2,
    RGBA8888 = 3,
};

const VIControl = packed struct {
    fbMode : u2  = @enumToInt(FBMode.Blank),
    gammaDitherEnable: bool = false,
    gammaEnable: bool = false,
    divotEnable: bool = false,
    _pad0  : u1  = 0,
    serrate: bool = false,
    _pad1  : u1  = 0,
    aaMode : u2  = 0,
    _pad2  : u6  = 0,
    _pad3  : u16 = 0,
};

pub var viControl = VIControl{};
var viOrigin  : u32 = 0;
var viWidth   : u32 = 0;
var viIntr    : u10 = 0;
var viCurrentV: u10 = 0;

pub fn init() void {

}

pub fn getOrigin() usize {
    return @intCast(usize, viOrigin);
}

pub fn read32(pAddr: u64) u32 {
    var data: u32 = undefined;

    switch (pAddr & 0xFF) {
        @enumToInt(VIReg.VICurrentV) => {
            //std.log.info("[VI] Read32 @ pAddr {X}h (VI Current V).", .{pAddr});

            data = @intCast(u32, viCurrentV);
        },
        else => {
            std.log.warn("[VI] Unhandled read32 @ pAddr {X}h.", .{pAddr});
        }
    }

    return data;
}

pub fn write32(pAddr: u64, data: u32) void {
    switch (pAddr & 0xFF) {
        @enumToInt(VIReg.VIControl) => {
            std.log.info("[VI] Write32 @ pAddr {X}h (VI Control), data: {X}h.", .{pAddr, data});

            viControl = @bitCast(VIControl, data);
        },
        @enumToInt(VIReg.VIOrigin) => {
            std.log.info("[VI] Write32 @ pAddr {X}h (VI Origin), data: {X}h.", .{pAddr, data});

            viOrigin = data & 0xFF_FFFF;
        },
        @enumToInt(VIReg.VIWidth) => {
            std.log.info("[VI] Write32 @ pAddr {X}h (VI Width), data: {X}h.", .{pAddr, data});

            viWidth = data;

            changeScreen(@bitCast(c_int, viWidth), viControl.fbMode);
        },
        @enumToInt(VIReg.VIIntr) => {
            std.log.info("[VI] Write32 @ pAddr {X}h (VI Interrupt), data: {X}h.", .{pAddr, data});

            viIntr = @truncate(u10, data);
        },
        @enumToInt(VIReg.VICurrentV) => {
            std.log.info("[VI] Write32 @ pAddr {X}h (VI Current V), data: {X}h.", .{pAddr, data});

            mi.clearPending(InterruptSource.VI);
        },
        else => {
            std.log.warn("[VI] Unhandled write32 @ pAddr {X}h, data: {X}h.", .{pAddr, data});
        }
    }
}

pub fn incCurrentV() void {
    viCurrentV +%= 2;

    if (viCurrentV == viIntr) {
        std.log.info("[VI] VI interrupt pending.", .{});

        mi.setPending(InterruptSource.VI);
    }
}
