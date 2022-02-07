//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! cop1.zig - Floating-point Unit module. 
//!

const std = @import("std");

const info = std.log.info;
const warn = std.log.warn;

const math = std.math;

const cop0 = @import("cop0.zig");

const FPUCtrlReg = enum(u32) {
    FCR31 = 31,
};

pub const Cond = enum(u4) {
    F    = 0x0,
    UN   = 0x1,
    EQ   = 0x2,
    UEQ  = 0x3,
    OLT  = 0x4,
    ULT  = 0x5,
    OLE  = 0x6,
    ULE  = 0x7,
    SF   = 0x8,
    NGLE = 0x9,
    SEQ  = 0xA,
    NGL  = 0xB,
    LT   = 0xC,
    NGE  = 0xD,
    LE   = 0xE,
    NGT  = 0xF,
};

pub const Fmt = enum {
    S, D, W, L
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

var isDisasm = false;

var fprs: [32]u64 = undefined;

var fcr31: FCR31 = FCR31{};

pub var coc1 = false;

fn getFd(instr: u32) u32 {
    return (instr >> 6) & 0x1F;
}

fn getFs(instr: u32) u32 {
    return (instr >> 11) & 0x1F;
}

fn getFt(instr: u32) u32 {
    return (instr >> 16) & 0x1F;
}

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

pub fn getFGR32(idx: u32) u32 {
    if (cop0.status.fr) {
        return @truncate(u32, fprs[idx]);
    } else {
        if ((idx & 1) != 0) {
            return @truncate(u32, fprs[idx & 0x1E] >> 32);
        } else {
            return @truncate(u32, fprs[idx]);
        }
    }
}

pub fn getFGR64(idx: u32) u64 {
    var idx_ = idx;

    if (!cop0.status.fr) {
        idx_ &= 0x1E;
    }

    return fprs[idx_];
}

pub fn setFGR32(idx: u32, data: u32) void {
    if (cop0.status.fr) {
        fprs[idx] = (fprs[idx] & 0xFFFFFFFF_00000000) | @intCast(u64, data);
    } else {
        if ((idx & 1) != 0) {
            fprs[idx & 0x1E] = (fprs[idx] & 0xFFFFFFFF) | (@intCast(u64, data) << 32);
        } else {
            fprs[idx] = (fprs[idx] & 0xFFFFFFFF_00000000) | @intCast(u64, data);
        }
    }
}

pub fn setFGR64(idx: u32, data: u64) void {
    var idx_ = idx;

    if (!cop0.status.fr) {
        idx_ &= 0x1E;
    }

    fprs[idx_] = data;
}

/// ADD - ADD
pub fn fADD(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);
    const ft = getFt(instr);

    switch (fmt) {
        Fmt.S => setFGR32(fd, @bitCast(u32, @intToFloat(f32, getFGR32(fs)) + @intToFloat(f32, getFGR32(ft)))),
        Fmt.D => setFGR64(fd, @bitCast(u64, @intToFloat(f64, getFGR64(fs)) + @intToFloat(f64, getFGR64(ft)))),
        else => {
            @panic("add: unhandled fmt");
        }
    }

    if (isDisasm) info("[FPU] ADD.{s} ${}, ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, ft, fd, getFGR64(fd)});
}

/// C - Compare
pub fn fC(instr: u32, cond: u4, fmt: comptime Fmt) void {
    const fs = getFs(instr);
    const ft = getFt(instr);

    var cond_: u4 = 0;

    switch (fmt) {
        Fmt.S => {
            const s = @intToFloat(f32, getFGR32(fs));
            const t = @intToFloat(f32, getFGR32(ft));

            if (math.isNan(s) or math.isNan(t)) {
                if ((cond & 8) != 0) @panic("c.cond: invalid operation");

                cond_ = 1;
            } else {
                if (s <  t) cond_ |= 2;
                if (s == t) cond_ |= 4;
            }
        },
        Fmt.D => {
            const s = @intToFloat(f64, getFGR64(fs));
            const t = @intToFloat(f64, getFGR64(ft));

            if (math.isNan(s) or math.isNan(t)) {
                if ((cond & 8) != 0) @panic("c.cond: invalid operation");

                cond_ = 1;
            } else {
                if (s <  t) cond_ |= 2;
                if (s == t) cond_ |= 4;
            }
        },
        else => {
            @panic("c.cond: unhandled fmt");
        }
    }

    fcr31.c = (cond & cond_) != 0;

    coc1 = fcr31.c;

    if (isDisasm) info("[FPU] C.{s}.{s} ${}, ${}", .{@tagName(@intToEnum(Cond, cond)), @tagName(fmt), fs, ft});
}

/// CVT.D - ConVerT to Double
pub fn fCVT_D(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);

    var data: f64 = undefined;

    switch (fmt) {
        Fmt.S => data = @floatCast(f64, @bitCast(f32, getFGR32(fs))),
        Fmt.W => data = @intToFloat(f64, getFGR32(fs)),
        else => {
            @panic("cvt.d: unhandled fmt");
        }
    }

    setFGR64(fd, @bitCast(u64, data));

    if (isDisasm) info("[FPU] CVT.D.{s} ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, fd, @bitCast(u64, data)});
}

/// CVT.S - ConVerT to Single
pub fn fCVT_S(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);

    var data: f32 = undefined;

    switch (fmt) {
        Fmt.D => data = @floatCast(f32, @bitCast(f64, getFGR64(fs))),
        Fmt.W => data = @intToFloat(f32, getFGR32(fs)),
        else => {
            @panic("cvt.s: unhandled fmt");
        }
    }

    setFGR32(fd, @bitCast(u32, data));

    if (isDisasm) info("[FPU] CVT.S.{s} ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, fd, @bitCast(u32, data)});
}

/// CVT.W - ConVerT to Word
pub fn fCVT_W(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);

    var data: u32 = undefined;

    switch (fmt) {
        // Fmt.S => data = @truncate(u32, @floatToInt(u64, @floatCast(f64, @bitCast(f32, getFGR32(fs))))),
        Fmt.S => data = 0,
        Fmt.D => data = @truncate(u32, @floatToInt(u64, @bitCast(f64, getFGR64(fs)))),
        else => {
            @panic("cvt.w: unhandled fmt");
        }
    }

    setFGR32(fd, data);

    if (isDisasm) info("[FPU] CVT.W.{s} ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, fd, @bitCast(u32, data)});
}

/// DIV - DIVide
pub fn fDIV(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);
    const ft = getFt(instr);

    switch (fmt) {
        Fmt.S => setFGR32(fd, @bitCast(u32, @intToFloat(f32, getFGR32(fs)) / @intToFloat(f32, getFGR32(ft)))),
        Fmt.D => setFGR64(fd, @bitCast(u64, @intToFloat(f64, getFGR64(fs)) / @intToFloat(f64, getFGR64(ft)))),
        else => {
            @panic("div: unhandled fmt");
        }
    }

    if (isDisasm) info("[FPU] DIV.{s} ${}, ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, ft, fd, getFGR64(fd)});
}

/// MOV - MOVe
pub fn fMOV(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);

    var data: f32 = undefined;

    switch (fmt) {
        Fmt.S => setFGR32(fd, @bitCast(u32, @intToFloat(f32, getFGR32(fs)))),
        Fmt.D => setFGR64(fd, @bitCast(u64, @intToFloat(f64, getFGR64(fs)))),
        else => {
            @panic("mov: unhandled fmt");
        }
    }

    if (isDisasm) info("[FPU] MOV.{s} ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, fd, @bitCast(u32, data)});
}

/// MUL - MULtiply
pub fn fMUL(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);
    const ft = getFt(instr);

    switch (fmt) {
        Fmt.S => setFGR32(fd, @bitCast(u32, @intToFloat(f32, getFGR32(fs)) * @intToFloat(f32, getFGR32(ft)))),
        Fmt.D => setFGR64(fd, @bitCast(u64, @intToFloat(f64, getFGR64(fs)) * @intToFloat(f64, getFGR64(ft)))),
        else => {
            @panic("mul: unhandled fmt");
        }
    }

    if (isDisasm) info("[FPU] MUL.{s} ${}, ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, ft, fd, getFGR64(fd)});
}

/// NEG - NEGate
pub fn fNEG(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);

    var data: f32 = undefined;

    switch (fmt) {
        Fmt.S => setFGR32(fd, @bitCast(u32, -@intToFloat(f32, getFGR32(fs)))),
        else => {
            @panic("neg: unhandled fmt");
        }
    }

    if (isDisasm) info("[FPU] NEG.{s} ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, fd, @bitCast(u32, data)});
}

/// SQRT - SQuare RooT
pub fn fSQRT(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);

    var data: f32 = undefined;

    switch (fmt) {
        Fmt.S => setFGR32(fd, @bitCast(u32, @sqrt(@intToFloat(f32, getFGR32(fs))))),
        else => {
            @panic("mov unhandled fmt");
        }
    }

    if (isDisasm) info("[FPU] MOV.{s} ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, fd, @bitCast(u32, data)});
}

/// SUB - SUB
pub fn fSUB(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);
    const ft = getFt(instr);

    switch (fmt) {
        Fmt.S => setFGR32(fd, @bitCast(u32, @intToFloat(f32, getFGR32(fs)) - @intToFloat(f32, getFGR32(ft)))),
        Fmt.D => setFGR64(fd, @bitCast(u64, @intToFloat(f64, getFGR64(fs)) - @intToFloat(f64, getFGR64(ft)))),
        else => {
            @panic("add: unhandled fmt");
        }
    }

    if (isDisasm) info("[FPU] SUB.{s} ${}, ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, ft, fd, getFGR64(fd)});
}

/// TRUNC.W - TRUNCate to Word
pub fn fTRUNC_W(instr: u32, fmt: comptime Fmt) void {
    const fd = getFd(instr);
    const fs = getFs(instr);

    var data: u32 = undefined;

    switch (fmt) {
        Fmt.S => data = @bitCast(u32, @trunc(@bitCast(f32, getFGR32(fs)))),
        Fmt.D => data = @truncate(u32, @floatToInt(u64, @trunc(@bitCast(f64, getFGR64(fs))))),
        else => {
            @panic("trunc.w: unhandled fmt");
        }
    }

    setFGR32(fd, data);

    if (isDisasm) info("[FPU] TRUNC.W.{s} ${}, ${}; ${} = {X}h", .{@tagName(fmt), fd, fs, fd, @bitCast(u32, data)});
}
