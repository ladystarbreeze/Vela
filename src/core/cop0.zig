//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! cop0.zig - System Coprocessor module. 
//!

const std = @import("std");

const raiseException = @import("cpu.zig").raiseException;

/// COP0 register aliases
pub const COP0Reg = enum(u32) {
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

const Index = packed struct {
    index: u5   = 0,
    _pad0: u26  = 0,
    p    : bool = false,
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

pub const EntryHi = packed struct {
    asid  : u8   = 0,
    _pad0 : u4   = 0,
    g     : bool = false,
    vpn2l : u3   = 0,
    vpn2h : u16  = 0,
};

pub const EntryLo = packed struct {
    g    : bool = false,
    v    : bool = false,
    d    : bool = false,
    c    : u3   = 0,
    pfn  : u20  = 0,
    _pad1: u6   = 0,
};

const PageMask = packed struct {
    _pad0: u13 = 0,
    mask : u12 = 0,
    _pad1: u7  = 0,
};

const TLBEntry = packed struct {
    entryLo0: EntryLo  = EntryLo{},
    entryLo1: EntryLo  = EntryLo{},
    entryHi : EntryHi  = EntryHi{},
    pageMask: PageMask = PageMask{},
};

pub var cause: Cause = Cause{};
const causeMask: u32 = 0x0000_0300;

pub var index: Index = Index{};
const indexMask: u32 = 0x8000_003F;

pub var status: Status = Status{};
const statusMask: u32 = 0xFE00_FFFF;

pub var entryLo0: EntryLo  = EntryLo{};
pub var entryLo1: EntryLo  = EntryLo{};
const entryLoMask: u32 = 0x03FF_FFFF;

pub var entryHi : EntryHi  = EntryHi{};
const entryHiMask: u32 = 0xFFFF_F0FF;

pub var pageMask: PageMask = PageMask{};
const pageMaskMask: u32 = 0x01FF_E000;

pub var tlbEntries: [32]TLBEntry = undefined;

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
        @enumToInt(COP0Reg.EntryLo0) => {
            data = @bitCast(u32, entryLo0);
        },
        @enumToInt(COP0Reg.EntryLo1) => {
            data = @bitCast(u32, entryLo1);
        },
        @enumToInt(COP0Reg.PageMask) => {
            data = @bitCast(u32, pageMask);
        },
        @enumToInt(COP0Reg.Count) => {
            data = count;
        },
        @enumToInt(COP0Reg.EntryHi) => {
            data = @bitCast(u32, entryHi);
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
        @enumToInt(COP0Reg.Index) => {
            index = @bitCast(Index, @bitCast(u32, index) & ~indexMask);
            index = @bitCast(Index, @bitCast(u32, index) | (data & indexMask));
        },
        @enumToInt(COP0Reg.EntryLo0) => {
            entryLo0 = @bitCast(EntryLo, @bitCast(u32, entryLo0) & ~entryLoMask);
            entryLo0 = @bitCast(EntryLo, @bitCast(u32, entryLo0) | (data & entryLoMask));
        },
        @enumToInt(COP0Reg.EntryLo1) => {
            entryLo1 = @bitCast(EntryLo, @bitCast(u32, entryLo1) & ~entryLoMask);
            entryLo1 = @bitCast(EntryLo, @bitCast(u32, entryLo1) | (data & entryLoMask));
        },
        @enumToInt(COP0Reg.PageMask) => {
            pageMask = @bitCast(PageMask, @bitCast(u32, pageMask) & ~pageMaskMask);
            pageMask = @bitCast(PageMask, @bitCast(u32, pageMask) | (data & pageMaskMask));
        },
        @enumToInt(COP0Reg.EntryHi) => {
            entryHi = @bitCast(EntryHi, @bitCast(u32, entryHi) & ~entryHiMask);
            entryHi = @bitCast(EntryHi, @bitCast(u32, entryHi) | (data & entryHiMask));
        },
        @enumToInt(COP0Reg.Compare) => {
            compare = data;

            clearPending(7);
        },
        @enumToInt(COP0Reg.Status) => {
            status = @bitCast(Status, @bitCast(u32, status) & ~statusMask);
            status = @bitCast(Status, @bitCast(u32, status) | (data & statusMask));
        },
        @enumToInt(COP0Reg.Cause) => {
            cause = @bitCast(Cause, @bitCast(u32, cause) & ~causeMask);
            cause = @bitCast(Cause, @bitCast(u32, cause) | (data & causeMask));
        },
        @enumToInt(COP0Reg.EPC) => {
            epc = data;
        },
        @enumToInt(COP0Reg.ErrorEPC) => {
            errorEPC = data;
        },
        else => {}
    }
    
    // std.log.info("[COP0] Write {s}, data: {X}h.", .{@tagName(@intToEnum(COP0Reg, idx)), data});
}

pub fn tlbTranslate(addr: u64) u64 {
    std.log.info("[COP0] TLB translate address {X}h", .{addr});

    var idx: u6 = 0;
    while (idx < 32) : (idx += 1) {
        const offsetMask: u64 = 0xFFF | (@intCast(u64, tlbEntries[idx].pageMask.mask) << 12);
        const vpnShift = @intCast(u6, @popCount(u12, tlbEntries[idx].pageMask.mask)) + 12;

        const vpn = addr >> vpnShift;

        var eVPN: u64 = ((@intCast(u64, tlbEntries[idx].entryHi.vpn2h) << 3) | @intCast(u64, tlbEntries[idx].entryHi.vpn2l)) >> (vpnShift - 12);

        // std.log.info("[COP0] TLB translation, VPN: {X}h, entry VPN: {X}h.", .{vpn, eVPN});
        // std.log.info("[COP0] Page Frame: {X}h:{X}h, G: {}", .{tlbEntries[idx].entryLo0.pfn, tlbEntries[idx].entryLo1.pfn, tlbEntries[idx].entryHi.g});
        // std.log.info("[COP0] ASID: {X}h", .{tlbEntries[idx].entryHi.asid});

        if (((vpn >> 1) == eVPN) and tlbEntries[idx].entryHi.g) {
            var pAddr: u64 = undefined;
            if ((vpn & 1) == 0) {
                pAddr = (@intCast(u64, tlbEntries[idx].entryLo0.pfn) << 12) | (addr & offsetMask);
            } else {
                pAddr = (@intCast(u64, tlbEntries[idx].entryLo1.pfn) << 12) | (addr & offsetMask);
            }

            std.log.info("[COP0] Translated address: {X}h", .{pAddr});

            return pAddr;
        }
    }

    @panic("TLB miss");
}

pub fn checkForInterrupts() bool {
    if (((cause.ip & status.im) != 0) and status.ie and !status.exl and !status.erl) {
        raiseException(ExceptionCode.Interrupt);

        return true;
    }

    return false;
}

pub fn clearPending(comptime i: u8) void {
    cause.ip &= ~@intCast(u8, (1 << i));
}

pub fn setPending(comptime i: u8) void {
    cause.ip |= (1 << i);
}

pub fn tickCount(c: u32) void {
    count +%= c;

    if (count == compare) {
        std.log.info("[COP0] Compare interrupt pending.", .{});

        setPending(7);
    }
}
