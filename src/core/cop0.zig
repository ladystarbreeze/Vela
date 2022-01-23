//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! cop0.zig - System Coprocessor module. 
//!

const std = @import("std");

const raiseException = @import("cpu.zig").raiseException;

/// COP0 register aliases
const COP0Reg = enum(u32) {
    Index       =  0,
    Random      =  1,
    EntryLo0    =  2,
    EntryLo1    =  3,
    Context     =  4,
    PageMask    =  5,
    Wired       =  6,
    R7          =  7,
    BadVAddr    =  8,
    Count       =  9,
    EntryHi     = 10,
    Compare     = 11,
    Status      = 12,
    Cause       = 13,
    EPC         = 14,
    PRId        = 15,
    Config      = 16,
    LLAddr      = 17,
    WatchLo     = 18,
    WatchHi     = 19,
    XContext    = 20,
    R21         = 21,
    R22         = 22,
    R23         = 23,
    R24         = 24,
    R25         = 25,
    ParityError = 26,
    CacheError  = 27,
    TagLo       = 28,
    TagHi       = 29,
    ErrorEPC    = 30,
    R31         = 31,
};

pub const ExceptionCode = enum(u5) {
    Interrupt              = 0,
    TLBModification        = 1,
    TLBMissLoad            = 2,
    TLBMissStore           = 3,
    AddressErrorLoad       = 4,
    AddressErrorStore      = 5,
    InstructionBusError    = 6,
    DataBusError           = 7,
    SystemCall             = 8,
    Breakpoint             = 9,
    ReservedInstruction    = 10,
    CoprocessorUnusable    = 11,
    Overflow               = 12,
    Trap                   = 13,
    FloatingPointException = 15,
    Watch                  = 23,
};

const Cause = packed struct {
    _pad0  : u2   = 0,
    excCode: u5   = 0,
    _pad1  : u1   = 0,
    ip     : u8   = 0,
    _pad2  : u12  = 0,
    ce     : u2   = 0,
    _pad3  : u1   = 0,
    bd     : bool = false,
};

const Status = packed struct {
    ie : bool = false,
    exl: bool = false,
    erl: bool = false,
    ksu: u2   = 0,
    ux : bool = false,
    sx : bool = false,
    kx : bool = false,
    im : u8   = 0,
    ds : u9   = 0,
    re : bool = false,
    fr : bool = false,
    rp : bool = false,
    cu : u4   = 0,
};

pub var cause: Cause = Cause{};
const causeMask: u32 = 0x0000_0300;

pub var status: Status = Status{};
const statusMask: u32 = 0xFE00FFFF;

var count  : u32 = 0;
var compare: u32 = 0;

pub var epc: u32 = 0xFFFFFFFF;
pub var errorEPC: u32 = 0xFFFFFFFF;

pub fn init() void {
    set32(@enumToInt(COP0Reg.Random), 0x0000001F);
    //set32(@enumToInt(COP0Reg.Status), 0x70400004);
    set32(@enumToInt(COP0Reg.PRId  ), 0x00000B00);
    set32(@enumToInt(COP0Reg.Config), 0x0006E463);

    status = @bitCast(Status, @intCast(u32, 0x70400000));
    cause  = @bitCast(Cause , @intCast(u32, 0xB000007C));
}

pub fn get32(idx: u32) u32 {
    var data: u32 = undefined;

    switch (idx) {
        @enumToInt(COP0Reg.Count) => {
            data = count;
        },
        @enumToInt(COP0Reg.Compare) => {
            data = compare;
        },
        @enumToInt(COP0Reg.Status) => {
            data = @bitCast(u32, status);
        },
        @enumToInt(COP0Reg.Cause) => {
            data = @bitCast(u32, cause);
        },
        @enumToInt(COP0Reg.EPC) => {
            data = epc;
        },
        @enumToInt(COP0Reg.ErrorEPC) => {
            data = errorEPC;
        },
        else => {
            data = 0;
        }
    }

    //std.log.info("[COP0] Read {s}, data: {X}h", .{@tagName(@intToEnum(COP0Reg, idx)), data});

    return data;
}

pub fn set32(idx: u32, data: u32) void {
    switch (idx) {
        @enumToInt(COP0Reg.Compare) => {
            compare = data;

            clearPending(7);
        },
        @enumToInt(COP0Reg.Status) => {
            status = @bitCast(Status, @bitCast(u32, status) & ~statusMask);
            status = @bitCast(Status, @bitCast(u32, status) | (data & statusMask));

            checkForInterrupts();
        },
        @enumToInt(COP0Reg.Cause) => {
            cause = @bitCast(Cause, @bitCast(u32, cause) & ~causeMask);
            cause = @bitCast(Cause, @bitCast(u32, cause) | (data & causeMask));

            checkForInterrupts();
        },
        @enumToInt(COP0Reg.EPC) => {
            epc = data;
        },
        @enumToInt(COP0Reg.ErrorEPC) => {
            errorEPC = data;
        },
        else => {}
    }
    
    std.log.info("[COP0] Write {s}, data: {X}h.", .{@tagName(@intToEnum(COP0Reg, idx)), data});
}

fn checkForInterrupts() void {
    if (((cause.ip & status.im) != 0) and status.ie and !status.exl and !status.erl) {
        raiseException(ExceptionCode.Interrupt);
    }
}

pub fn clearPending(comptime i: u8) void {
    cause.ip &= ~@intCast(u8, (1 << i));

    checkForInterrupts();
}

pub fn setPending(comptime i: u8) void {
    cause.ip |= (1 << i);

    checkForInterrupts();
}

pub fn tickCount(c: u32) void {
    count +%= c;

    if (count == compare) {
        std.log.info("[COP0] Compare interrupt pending.", .{});

        setPending(7);
    }
}
