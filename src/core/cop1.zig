//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! cop1.zig - Floating-point Unit module. 
//!

const std = @import("std");

const info = std.log.info;
const warn = std.log.warn;

const cop0 = @import("cop0.zig");

const FPUCtrlReg = enum(u32) {
    FCR31 = 31,
};

const RoundingMode = enum(u2) {
    RoundNearest = 0,
    RoundZero    = 1,
    RoundInfP    = 2,
    RoundInfN    = 3,
};

const FPUFlags = enum(u5) {
    I = 0b00001,
    U = 0b00010,
    O = 0b00100,
    Z = 0b01000,
    V = 0b10000,
};

const FCR31 = packed struct {
    rm     : u2   = @enumToInt(RoundingMode.RoundNearest),
    flags  : u5   = 0,
    enables: u5   = 0,
    cause  : u5   = 0,
    causeE : bool = false,
    _pad0  : u5   = 0,
    c      : bool = false,
    fs     : bool = false,
    _pad1  : u7   = 0,
};

var fprs: [32]f64 = undefined;

var fcr31: FCR31 = FCR31{};

pub fn getCtrl32(idx: u32) u32 {
    var data: u32 = undefined;
    switch (idx) {
        @enumToInt(FPUCtrlReg.FCR31) => {
            data = @bitCast(u32, fcr31);
        },
        else => {
            warn("[FPU] Read from unhandled FPU Control register {}.", .{idx});

            @panic("unhandled FPU Control register");
        }
    }

    info("[FPU] Read {s}.", .{@tagName(@intToEnum(FPUCtrlReg, idx))});
    
    return data;
}

pub fn setCtrl32(idx: u32, data: u32) void {
    switch (idx) {
        @enumToInt(FPUCtrlReg.FCR31) => {
            fcr31 = @bitCast(FCR31, data);
        },
        else => {
            warn("[FPU] Write to unhandled FPU Control register {}, data: {X}h.", .{idx, data});

            @panic("unhandled FPU Control register");
        }
    }

    info("[FPU] Write {s}, data: {X}h.", .{@tagName(@intToEnum(FPUCtrlReg, idx)), data});
}

pub fn setFGR32(idx: u32, data: u32) void {
    if (cop0.status.fr) {
        fprs[idx] = @bitCast(f64, (@bitCast(u64, fprs[idx]) & 0xFFFFFFFF_00000000) | @intCast(u64, data));
    } else {
        fprs[idx] = @bitCast(f64, @intCast(u64, data));
    }
}

pub fn setFGR64(idx: u32, data: u64) void {
    if (cop0.status.fr) {
        fprs[idx] = @bitCast(f64, data);
    } else {
        fprs[idx + 0] = @bitCast(f64, data & 0xFFFFFFFF);
        fprs[idx + 1] = @bitCast(f64, data >> 32);
    }
}
