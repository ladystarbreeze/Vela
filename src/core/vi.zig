//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! vi.zig - Video Interface module. 
//!

const std = @import("std");

const changeScreen = @import("n64.zig").changeScreen;

const VIReg = enum(u64) {
    VIControl  = 0x00,
    VIOrigin   = 0x04,
    VIWidth    = 0x08,
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
            std.log.info("[Bus] Read32 @ pAddr {X}h (VI Current V).", .{pAddr});

            data = @intCast(u32, viCurrentV);

            viCurrentV +%= 2;
        },
        else => {
            std.log.warn("[Bus] Unhandled read32 @ pAddr {X}h (Video Interface).", .{pAddr});
        }
    }

    return data;
}

pub fn write32(pAddr: u64, data: u32) void {
    switch (pAddr & 0xFF) {
        @enumToInt(VIReg.VIControl) => {
            std.log.info("[Bus] Write32 @ pAddr {X}h (VI Control), data: {X}h.", .{pAddr, data});

            viControl = @bitCast(VIControl, data);
        },
        @enumToInt(VIReg.VIOrigin) => {
            std.log.info("[Bus] Write32 @ pAddr {X}h (VI Origin), data: {X}h.", .{pAddr, data});

            viOrigin = data & 0xFF_FFFF;
        },
        @enumToInt(VIReg.VIWidth) => {
            std.log.info("[Bus] Write32 @ pAddr {X}h (VI Width), data: {X}h.", .{pAddr, data});

            viWidth = data;

            changeScreen(@bitCast(c_int, viWidth), viControl.fbMode);
        },
        else => {
            std.log.warn("[Bus] Unhandled write32 @ pAddr {X}h (Video Interface), data: {X}h.", .{pAddr, data});
        }
    }
}
