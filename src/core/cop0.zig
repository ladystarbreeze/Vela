//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! cop0.zig - System Coprocessor module. 
//!

const std = @import("std");

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

const COP0 = struct {
    pub fn init(self: COP0) void {
        self.set32(@enumToInt(COP0Reg.Random), 0x0000001F);
        self.set32(@enumToInt(COP0Reg.Status), 0x70400004);
        self.set32(@enumToInt(COP0Reg.PRID  ), 0x00000B00);
        self.set32(@enumToInt(COP0Reg.Config), 0x0006E463);
    }

    pub fn get32(self: COP0, idx: u32) u64 {
        return 0;
    }

    pub fn set32(self: COP0, idx: u32, data: u32) void {
    }
}
