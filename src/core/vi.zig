//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! vi.zig - Video Interface module. 
//!

const std = @import("std");

const VIReg = enum(u64) {
    VIControl = 0x00,
    VIOrigin  = 0x04,
    VIWidth   = 0x08,
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

var viControl = VIControl{};
var viOrigin : u32 = undefined;

pub fn init() void {

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
        else => {
            std.log.warn("[Bus] Unhandled write32 @ pAddr {X}h (Video Interface), data: {X}h.", .{pAddr, data});
        }
    }
}
