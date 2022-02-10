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
const rdp = @import("rdp.zig");

const RDPStatus = rdp.RDPStatus;

const mi = @import("mi.zig");

const InterruptSource = mi.InterruptSource;

const rcpROM = @import("rsptable.zig").rcpROM;
const rsqROM = @import("rsptable.zig").rsqROM;

/// Clamp 32-bit signed data
fn clamps32(data: i32) u16 {
    if (data < -32768) return @bitCast(u16, @intCast(i16, -32768));
    if (data >  32767) return  32767;

    return @truncate(u16, @bitCast(u32, data));
}

/// Clamp 32-bit unsigned data
fn clampu32(data: i32) u16 {
    if (data < 0)     return 0;
    if (data > 32767) return 65535;

    return @truncate(u16, @bitCast(u32, data));
}

fn isSignExtended(hi: u16, lo: u16) bool {
    if (hi == 0) {
        return (lo & 0x8000) == 0;
    } else if (hi == 0xFFFF) {
        return (lo & 0x8000) == 0x8000;
    } else {
        return false;
    }
}

/// Sign extend 8-bit data
fn exts8(data: u8) u32 {
    return @bitCast(u32, @intCast(i32, @bitCast(i8, data)));
}

/// Sign extend 16-bit data
fn exts16(data: u16) u32 {
    return @bitCast(u32, @intCast(i32, @bitCast(i16, data)));
}

/// Sign extend 32-bit data
fn exts32(data: u32) u48 {
    return @bitCast(u48, @intCast(i48, @bitCast(i32, data)));
}

/// Get VU lane index
fn getIdxL(idx: u32) u32 {
    return 7 - idx;
}

/// Get Accumulator lane index
fn getIdxA(idx: u32) u32 {
    return 2 - idx;
}

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
    RSPDMABusy  = 0x18,
};

const RSPOpcode = enum(u32) {
    SPECIAL = 0x00,
    REGIMM  = 0x01,

    J     = 0x02,
    JAL   = 0x03,
    BEQ   = 0x04,
    BNE   = 0x05,
    BLEZ  = 0x06,
    BGTZ  = 0x07,
    ADDI  = 0x08,
    ADDIU = 0x09,
    ANDI  = 0x0C,
    ORI   = 0x0D,
    XORI  = 0x0E,
    LUI   = 0x0F,
    COP0  = 0x10,
    COP2  = 0x12,
    LB    = 0x20,
    LH    = 0x21,
    LW    = 0x23,
    LBU   = 0x24,
    LHU   = 0x25,
    SB    = 0x28,
    SH    = 0x29,
    SW    = 0x2B,
    LWC2  = 0x32,
    SWC2  = 0x3A,
};

const RSPSpecial = enum(u32) {
    SLL   = 0x00,
    SRL   = 0x02,
    SRA   = 0x03,
    SLLV  = 0x04,
    SRLV  = 0x06,
    JR    = 0x08,
    BREAK = 0x0D,
    ADD   = 0x20,
    ADDU  = 0x21,
    SUB   = 0x22,
    SUBU  = 0x23,
    AND   = 0x24,
    OR    = 0x25,
    XOR   = 0x26,
    SLT   = 0x2A,
};

const RSPRegimm = enum(u32) {
    BLTZ = 0x00,
    BGEZ = 0x01,
};

const RSPCOPOpcode = enum(u32) {
    MF = 0x00,
    CF = 0x02,
    MT = 0x04,
};

const RSPCP2Opcode = enum(u32) {
    COMPUTE = 0x10,
};

const RSPVULoadOpcode = enum(u32) {
    LSV = 0x01,
    LLV = 0x02,
    LDV = 0x03,
    LQV = 0x04,
    LRV = 0x05,
};

const RSPVUStoreOpcode = enum(u32) {
    SBV = 0x00,
    SSV = 0x01,
    SLV = 0x02,
    SDV = 0x03,
    SQV = 0x04,
};

const RSPVUOpcode = enum(u32) {
    VMULF = 0x00,
    VMULU = 0x01,
    VRNDP = 0x02,
    VMUDL = 0x04,
    VMUDM = 0x05,
    VMUDN = 0x06,
    VMUDH = 0x07,
    VMACF = 0x08,
    VMACU = 0x09,
    VMADL = 0x0C,
    VMADM = 0x0D,
    VMADN = 0x0E,
    VMADH = 0x0F,
    VADD  = 0x10,
    VSUB  = 0x11,
    VADDC = 0x14,
    VSUBC = 0x15,
    VSAR  = 0x1D,
    VLT   = 0x20,
    VGE   = 0x23,
    VCL   = 0x24,
    VCH   = 0x25,
    VMRG  = 0x27,
    VAND  = 0x28,
    VOR   = 0x2A,
    VXOR  = 0x2C,
    VRCPL = 0x31,
    VRCPH = 0x32,
    VMOV  = 0x33,
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

const VectorReg = packed union {
    uLane: [8]u16,

    pub fn getSLane(self: VectorReg, idx: u32) i16 {
        return @bitCast(i16, self.uLane[getIdxL(idx)]);
    }

    pub fn getULane(self: VectorReg, idx: u32) u16 {
        return self.uLane[getIdxL(idx)];
    }

    pub fn setSLane(self: *VectorReg, idx: u32, data: i16) void {
        self.uLane[getIdxL(idx)] = @bitCast(u16, data);
    }

    pub fn setULane(self: *VectorReg, idx: u32, data: u16) void {
        self.uLane[getIdxL(idx)] = data;
    }
};

const Accumulator = struct {
    uLane: [8]u48,

    pub fn getSLane(self: Accumulator, idx: u32) i48 {
        return @bitCast(i48, self.uLane[getIdxL(idx)]);
    }

    pub fn getULane(self: Accumulator, idx: u32) u48 {
        return self.uLane[getIdxL(idx)];
    }

    pub fn getLane(self: Accumulator, idx: u32, e: u32) u16 {
        return @truncate(u16, self.uLane[getIdxL(idx)] >> @truncate(u6, 16 * getIdxA(e)));
    }

    pub fn setSLane(self: *Accumulator, idx: u32, data: i48) void {
        self.uLane[getIdxL(idx)] = @bitCast(u48, data);
    }

    pub fn setULane(self: *Accumulator, idx: u32, data: u48) void {
        self.uLane[getIdxL(idx)] = data;
    }

    pub fn setLane(self: *Accumulator, idx: u32, e: u32, data: u16) void {
        const shift = @truncate(u6, 16 * getIdxA(e));

        const mask  = @intCast(u48, 0xFFFF) << shift;
        const uLane = self.uLane[getIdxL(idx)] & ~mask;

        self.uLane[getIdxL(idx)] = uLane | (@intCast(u48, data) << shift);
    }
};

const VCC = struct {
    gte : [8]bool = undefined,
    lten: [8]bool = undefined,
};

const VCO = struct {
    c : [8]bool = undefined,
    ne: [8]bool = undefined,
};

const VCE = struct {
    n1: [8]bool = undefined,
};

const RSPRegs = struct {
    rspAddr  : RSPAddr   = RSPAddr{},
    rspDMALen: RSPDMALen = RSPDMALen{},
    rspStatus: RSPStatus = RSPStatus{},

    rspSema: bool = false,

    rspDRAMAddr: u24 = 0,

    gprs: [32]u32 = undefined,

    vprs: [32]VectorReg = undefined,
    
    acc: Accumulator = undefined,

    vcc: VCC = undefined,
    vco: VCO = undefined,
    vce: VCE = undefined,

    divIn : u16 = undefined,
    divOut: u16 = undefined,

    isDIVInLoaded: bool = false,

    pc : u12 = undefined,
    cpc: u12 = undefined,
    npc: u12 = undefined,

    pub fn get(self: RSPRegs, idx: u32) u32 {
        return self.gprs[idx];
    }

    pub fn getVPR(self: RSPRegs, idx: u32) VectorReg {
        return self.vprs[idx];
    }

    pub fn getByte(self: RSPRegs, idx: u32, element: u32) u8 {
        return @truncate(u8, self.vprs[idx].getULane(element >> 1) >> @truncate(u4, (8 * ((element ^ 1) & 1))));
    }

    pub fn getLane(self: RSPRegs, idx: u32, element: u32) u16 {
        return self.vprs[idx].getULane(element);
    }

    pub fn broadcast(self: RSPRegs, idx: u32, e: u32) VectorReg {
        const v = self.vprs[idx];

        var vBroadcast: VectorReg = undefined;

        var mask: u32 = undefined;

        switch (e) {
             0,1 => mask = 0x76543210,
             2   => mask = 0x66442200,
             3   => mask = 0x77553311,
             4   => mask = 0x44440000,
             5   => mask = 0x55551111,
             6   => mask = 0x66662222,
             7   => mask = 0x77773333,
             8   => mask = 0x00000000,
             9   => mask = 0x11111111,
            10   => mask = 0x22222222,
            11   => mask = 0x33333333,
            12   => mask = 0x44444444,
            13   => mask = 0x55555555,
            14   => mask = 0x66666666,
            15   => mask = 0x77777777,
            else => @panic("invalid broadcast modifier"),
        }

        vBroadcast.setULane(0, v.getULane((mask >>  0) & 0xF));
        vBroadcast.setULane(1, v.getULane((mask >>  4) & 0xF));
        vBroadcast.setULane(2, v.getULane((mask >>  8) & 0xF));
        vBroadcast.setULane(3, v.getULane((mask >> 12) & 0xF));
        vBroadcast.setULane(4, v.getULane((mask >> 16) & 0xF));
        vBroadcast.setULane(5, v.getULane((mask >> 20) & 0xF));
        vBroadcast.setULane(6, v.getULane((mask >> 24) & 0xF));
        vBroadcast.setULane(7, v.getULane((mask >> 28) & 0xF));

        return vBroadcast;
    }

    pub fn getCP0(self: RSPRegs, idx: u32) u32 {
        var data: u32 = undefined;

        switch (idx) {
            4  => data = @intCast(u32, @bitCast(u15, rspRegs.rspStatus)),
            5  => data = @intCast(u32, @bitCast(u1, rspRegs.rspStatus.df)),
            6  => data = @intCast(u32, @bitCast(u1, rspRegs.rspStatus.db)),
            7  => {
                data = @intCast(u32, @bitCast(u1, rspRegs.rspSema));

                rspRegs.rspSema = true;
            },
            9 => {
                data = @intCast(u32, rdp.rdpRegs.rdpCMDEnd);
            },
            10 => {
                data = @intCast(u32, rdp.rdpRegs.rdpCMDCurr);
            },
            11 => {
                data = @intCast(u32, @bitCast(u11, rdp.rdpRegs.rdpStatus));
            },
            else => {
                warn("[RSP] Unhandled CP0 read @ ${}.", .{idx});

                @panic("unhandled CP0 read");
            }
        }

        return data;
    }

    pub fn set(self: *RSPRegs, idx: u32, data: u32) void {
        self.gprs[idx] = data;

        self.gprs[0] = 0;
    }

    pub fn setByte(self: *RSPRegs, idx: u32, element: u32, data: u8) void {
        const uLane = self.vprs[idx].getULane(element >> 1);

        const shift = @truncate(u4, 8 * ((element ^ 1) & 1));
        const mask  = @intCast(u16, 0xFF) << shift;

        self.vprs[idx].setULane(element >> 1, (uLane & ~mask) | (@intCast(u16, data) << shift));
    }

    pub fn setLane(self: *RSPRegs, idx: u32, element: u32, data: u16) void {
        self.vprs[idx].setULane(element, data);
    }

    pub fn setSLane(self: *RSPRegs, idx: u32, element: u32, data: i16) void {
        self.vprs[idx].setSLane(element, data);
    }

    pub fn setVector(self: *RSPRegs, idx: u32, data: u128) void {
        const data_ = @byteSwap(u128, data);

        @memcpy(@ptrCast([*]u8, &vprs[idx].byte), @ptrCast([*]const u8, &data_), 16);
    }

    pub fn setCP0(self: RSPRegs, idx: u32, data: u32) void {
        info("[RSP] CP0 write @ ${}, data: {X}h.", .{idx, data});

        switch (idx) {
            0 => rspRegs.rspAddr = @bitCast(RSPAddr, @truncate(u13, data)),
            1 => rspRegs.rspDRAMAddr = @truncate(u24, data),
            2 => {
                rspRegs.rspDMALen = @bitCast(RSPDMALen, data);

                doDMAToRSP();
            },
            3 => {
                rspRegs.rspDMALen = @bitCast(RSPDMALen, data);

                doDMAToRAM();
            },
            4 => {
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
            },
            7 => {
                if (data == 0) rspRegs.rspSema = false;
            },
            8 => {
                rdp.rdpRegs.rdpCMDStart = @truncate(u24, data);
                rdp.rdpRegs.rdpCMDCurr  = rdp.rdpRegs.rdpCMDStart;

                rdp.rdpRegs.rdpStatus.sv = true;
            },
            9 => {
                rdp.rdpRegs.rdpCMDEnd = @truncate(u24, data);

                rdp.rdpRegs.rdpStatus.ev = true;

                rdp.processDP();
            },
            11 => {
                if ((data & (1 <<  0)) != 0) rdp.rdpRegs.rdpStatus.x  = false;
                if ((data & (1 <<  1)) != 0) rdp.rdpRegs.rdpStatus.x  = true;
                if ((data & (1 <<  2)) != 0) rdp.rdpRegs.rdpStatus.f  = false;
                if ((data & (1 <<  0)) != 0) rdp.rdpRegs.rdpStatus.f  = false;
                if ((data & (1 <<  1)) != 0) rdp.rdpRegs.rdpStatus.fl = true;
                if ((data & (1 <<  5)) != 0) rdp.rdpRegs.rdpStatus.fl = false;
            },
            else => {
                warn("[RSP] Unhandled CP0 write @ ${}, data: {X}h.", .{idx, data});

                @panic("unhandled CP0 write");
            }
        }
    }

    pub fn setPC(self: *RSPRegs, data: u32) void {
        self.pc  = @truncate(u12, data);
        self.npc = self.pc +% 4;
    }
};

// RSP memory
pub var spDMEM: [0x1000]u8 = undefined;
pub var spIMEM: [0x1000]u8 = undefined;

var rspRegs = RSPRegs{};

var rspIRQ = false;

const isDisasm = true;

fn getImm16(instr: u32) u16 {
    return @truncate(u16, instr);
}

fn getRd(instr: u32) u32 {
    return (instr >> 11) & 0x1F;
}

fn getRs(instr: u32) u32 {
    return (instr >> 21) & 0x1F;
}

fn getRt(instr: u32) u32 {
    return (instr >> 16) & 0x1F;
}

fn getSa(instr: u32) u32 {
    return (instr >> 6) & 0x1F;
}

fn getTarget(instr: u32) u32 {
    return (instr << 2) & 0xFFF_FFFF;
}

fn getFunct(instr: u32) u32 {
    return instr & 0x3F;
}

fn getElement(instr: u32) u32 {
    return (instr >> 7) & 0xF;
}

fn getDElement(instr: u32) u32 {
    return (instr >> 11) & 7;
}

fn getBroadcastMod(instr: u32) u32 {
    return (instr >> 21) & 0xF;
}

fn getOffset(instr: u32, comptime n: comptime_int) u32 {
    return @bitCast(u32, @intCast(i32, @bitCast(i7, @truncate(u7, instr)))) << n;
}

const getVd = getSa;
const getVs = getRd;
const getVt = getRt;

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

pub fn read16(pAddr: u64) u16 {
    var data: u16 = undefined;

    switch ((pAddr >> 12) & 0xFF) {
        @enumToInt(RSPMemory.DMEM) => {
            @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &spDMEM[pAddr & 0xFFF]), 2);
            data = @byteSwap(u16, data);
        },
        @enumToInt(RSPMemory.IMEM) => {
            @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &spIMEM[pAddr & 0xFFF]), 2);
            data = @byteSwap(u16, data);
        },
        else => {
            warn("[RSP] Unhandled read16 @ pAddr {X}h.", .{pAddr});

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
                @enumToInt(RSPReg.RSPAddr) => {
                    info("[RSP] Read32 @ pAddr {X}h (RSP Address).", .{pAddr});

                    data = @intCast(u32, @bitCast(u13, rspRegs.rspAddr));
                },
                @enumToInt(RSPReg.RSPDRAMAddr) => {
                    info("[RSP] Read32 @ pAddr {X}h (RSP DRAM Address).", .{pAddr});

                    data = @intCast(u32, rspRegs.rspDRAMAddr);
                },
                @enumToInt(RSPReg.RSPDMALenRD) => {
                    info("[RSP] Read32 @ pAddr {X}h (RSP DMA Length RD).", .{pAddr});

                    data = @bitCast(u32, rspRegs.rspDMALen);
                },
                @enumToInt(RSPReg.RSPDMALenWR) => {
                    info("[RSP] Read32 @ pAddr {X}h (RSP DMA Length WR).", .{pAddr});

                    data = @bitCast(u32, rspRegs.rspDMALen);
                },
                @enumToInt(RSPReg.RSPStatus) => {
                    info("[RSP] Read32 @ pAddr {X}h (RSP Status).", .{pAddr});

                    data = @intCast(u32, @bitCast(u15, rspRegs.rspStatus));
                },
                @enumToInt(RSPReg.RSPDMABusy) => {
                    info("[RSP] Read32 @ pAddr {X}h (RSP DMA Busy).", .{pAddr});

                    data = @intCast(u32, @bitCast(u1, rspRegs.rspStatus.db));
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

fn readDMEM(comptime T: type, pAddr: u32) T {
    var data: T = undefined;

    @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &spDMEM[pAddr & 0xFFF]), @sizeOf(T));

    if (pAddr == 0x364) info("[RSP] Read audio buffer size, data: {X}h.", .{@byteSwap(T, data)});
    
    return @byteSwap(T, data);
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
                @enumToInt(RSPReg.RSPDMALenWR) => {
                    info("[RSP] Write32 @ pAddr {X}h (RSP DMA Length WR), data: {X}h.", .{pAddr, data});

                    rspRegs.rspDMALen = @bitCast(RSPDMALen, data);

                    doDMAToRAM();
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
                },
                else => {
                    warn("[RSP] Unhandled write32 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

                    @panic("unhandled RSP write");
                }
            }
        },
        @enumToInt(RSPMemory.PC) => {
            info("[RSP] Write32 @ pAddr {X}h (RSP Program Counter), data: {X}h.", .{pAddr, data});

            rspRegs.setPC(data);
        },
        else => {
            warn("[RSP] Unhandled write32 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

            @panic("unhandled RSP write");
        }
    }
}

fn writeDMEM(comptime T: type, pAddr: u32, data: T) void {
    var data_: T = @byteSwap(T, data);

    if (pAddr ==  0x364) info("[RSP] Set audio buffer size, data: {X}h.", .{data});

    if (pAddr >= 0x1000) @panic("DMEM write out of bounds");

    @memcpy(@ptrCast([*]u8, &spDMEM[pAddr & 0xFFF]), @ptrCast([*]u8, &data_), @sizeOf(T));
}

fn fetchInstr() u32 {
    var data: u32 = undefined;

    rspRegs.cpc = rspRegs.pc;

    if ((rspRegs.cpc & 3) != 0) {
        @panic("unaligned program counter");
    }

    @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &spIMEM[rspRegs.pc & 0xFFF]), 4);

    rspRegs.pc  = rspRegs.npc;
    rspRegs.npc +%= 4;

    return @byteSwap(u32, data);
}

fn doDMAToRAM() void {
    const ramAddr = @intCast(u64, rspRegs.rspDRAMAddr)  & 0xFF_FFF8;
    const rspAddr = @intCast(u64, rspRegs.rspAddr.addr) & 0xFF8;

    const length = (@intCast(u64, rspRegs.rspDMALen.length) | 7) + 1;
    const count  = @intCast(u64, rspRegs.rspDMALen.count ) + 1;
    const skip   = @intCast(u64, rspRegs.rspDMALen.skip);

    var mem: ?*[0x1000]u8 = null;

    if (rspRegs.rspAddr.isIMEM) {
        info("[RSP] IMEM->RAM DMA, DRAM pAddr: {X}h, IMEM pAddr: {X}h, length: {X}h.", .{ramAddr, rspAddr, length});

        mem = &spIMEM;
    } else {
        info("[RSP] DMEM->RAM DMA, DRAM pAddr: {X}h, DMEM pAddr: {X}h, length: {X}h.", .{ramAddr, rspAddr, length});

        mem = &spDMEM;
    }

    var count_: u64 = 0;
    while (count_ < count) : (count_ += 1) {
        var length_: u64 = 0;
        while (length_ < length) : (length_ += 1) {
            bus.ram[ramAddr + count_ * ((length + skip) & 0xFFFFFFFF_FFFFFFF8) + length_] = mem.?[rspAddr + count_ * length + length_];
        }
    }
}

fn doDMAToRSP() void {
    const ramAddr = @intCast(u64, rspRegs.rspDRAMAddr)  & 0xFF_FFF8;
    const rspAddr = @intCast(u64, rspRegs.rspAddr.addr) & 0xFF8;

    const length = (@intCast(u64, rspRegs.rspDMALen.length) | 7) + 1;
    const count  = @intCast(u64, rspRegs.rspDMALen.count ) + 1;
    const skip   = @intCast(u64, rspRegs.rspDMALen.skip);

    var mem: ?*[0x1000]u8 = null;

    if (rspRegs.rspAddr.isIMEM) {
        info("[RSP] RAM->IMEM DMA, DRAM pAddr: {X}h, IMEM pAddr: {X}h, length: {X}h.", .{ramAddr, rspAddr, length});

        mem = &spIMEM;
    } else {
        info("[RSP] RAM->DMEM DMA, DRAM pAddr: {X}h, DMEM pAddr: {X}h, length: {X}h.", .{ramAddr, rspAddr, length});

        mem = &spDMEM;
    }

    var count_: u64 = 0;
    while (count_ < count) : (count_ += 1) {
        var length_: u64 = 0;
        while (length_ < length) : (length_ += 1) {
            mem.?[rspAddr + count_ * length + length_] = bus.ram[ramAddr + count_ * ((length + skip) & 0xFFFFFFFF_FFFFFFF8) + length_];
        }
    }
}

fn decodeInstr(instr: u32) void {
    const opcode = instr >> 26;

    switch (opcode) {
        @enumToInt(RSPOpcode.SPECIAL) => {
            const funct = instr & 0x3F;

            switch (funct) {
                @enumToInt(RSPSpecial.SLL  ) => iSLL  (instr),
                @enumToInt(RSPSpecial.SRL  ) => iSRL  (instr),
                @enumToInt(RSPSpecial.SRA  ) => iSRA  (instr),
                @enumToInt(RSPSpecial.SLLV ) => iSLLV (instr),
                @enumToInt(RSPSpecial.SRLV ) => iSRLV (instr),
                @enumToInt(RSPSpecial.JR   ) => iJR   (instr),
                @enumToInt(RSPSpecial.BREAK) => iBREAK(instr),
                @enumToInt(RSPSpecial.ADD  ) => iADDU (instr),
                @enumToInt(RSPSpecial.ADDU ) => iADDU (instr),
                @enumToInt(RSPSpecial.SUB  ) => iSUBU (instr),
                @enumToInt(RSPSpecial.SUBU ) => iSUBU (instr),
                @enumToInt(RSPSpecial.AND  ) => iAND  (instr),
                @enumToInt(RSPSpecial.OR   ) => iOR   (instr),
                @enumToInt(RSPSpecial.XOR  ) => iXOR  (instr),
                @enumToInt(RSPSpecial.SLT  ) => iSLT  (instr),
                else => {
                    warn("[RSP] Unhandled function {X}h ({X}h) @ {X}h.", .{funct, instr, rspRegs.cpc});

                    @panic("unhandled RSP instruction");
                }
            }
        },
        @enumToInt(RSPOpcode.REGIMM) => {
            const rt = getRt(instr);

            switch (rt) {
                @enumToInt(RSPRegimm.BLTZ) => iBLTZ(instr),
                @enumToInt(RSPRegimm.BGEZ) => iBGEZ(instr),
                else => {
                    warn("[RSP] Unhandled REGIMM opcode {X}h ({X}h) @ {X}h.", .{rt, instr, rspRegs.cpc});

                    @panic("unhandled RSP instruction");
                }
            }
        },
        @enumToInt(RSPOpcode.J    ) => iJ    (instr),
        @enumToInt(RSPOpcode.JAL  ) => iJAL  (instr),
        @enumToInt(RSPOpcode.BEQ  ) => iBEQ  (instr),
        @enumToInt(RSPOpcode.BNE  ) => iBNE  (instr),
        @enumToInt(RSPOpcode.BLEZ ) => iBLEZ (instr),
        @enumToInt(RSPOpcode.BGTZ ) => iBGTZ (instr),
        @enumToInt(RSPOpcode.ADDI ) => iADDIU(instr),
        @enumToInt(RSPOpcode.ADDIU) => iADDIU(instr),
        @enumToInt(RSPOpcode.ANDI ) => iANDI (instr),
        @enumToInt(RSPOpcode.ORI  ) => iORI  (instr),
        @enumToInt(RSPOpcode.XORI ) => iXORI (instr),
        @enumToInt(RSPOpcode.LUI  ) => iLUI  (instr),
        @enumToInt(RSPOpcode.COP0 ) => {
            switch (getRs(instr)) {
                @enumToInt(RSPCOPOpcode.MF) => iMFC(instr, 0),
                @enumToInt(RSPCOPOpcode.MT) => iMTC(instr, 0),
                else => {
                    warn("[RSP] Unhandled COP0 opcode {X}h ({X}h) @ {X}h.", .{getRs(instr), instr, rspRegs.cpc});

                    @panic("unhandled RSP instruction");
                }
            }
        },
        @enumToInt(RSPOpcode.COP2 ) => {
            switch (getRs(instr)) {
                @enumToInt(RSPCOPOpcode.MF) => iMFC(instr, 2),
                @enumToInt(RSPCOPOpcode.CF) => iCFC(instr, 2),
                @enumToInt(RSPCOPOpcode.MT) => iMTC(instr, 2),
                @enumToInt(RSPCP2Opcode.COMPUTE) ... @enumToInt(RSPCP2Opcode.COMPUTE) + 0xF => {
                    switch (getFunct(instr)) {
                        @enumToInt(RSPVUOpcode.VMULF) => iVMULF(instr),
                        @enumToInt(RSPVUOpcode.VMULU) => iVMULU(instr),
                        @enumToInt(RSPVUOpcode.VRNDP) => iVRNDP(instr),
                        @enumToInt(RSPVUOpcode.VMUDL) => iVMUDL(instr),
                        @enumToInt(RSPVUOpcode.VMUDM) => iVMUDM(instr),
                        @enumToInt(RSPVUOpcode.VMUDN) => iVMUDN(instr),
                        @enumToInt(RSPVUOpcode.VMUDH) => iVMUDH(instr),
                        @enumToInt(RSPVUOpcode.VMACF) => iVMACF(instr),
                        @enumToInt(RSPVUOpcode.VMACU) => iVMACU(instr),
                        @enumToInt(RSPVUOpcode.VMADL) => iVMADL(instr),
                        @enumToInt(RSPVUOpcode.VMADM) => iVMADM(instr),
                        @enumToInt(RSPVUOpcode.VMADN) => iVMADN(instr),
                        @enumToInt(RSPVUOpcode.VMADH) => iVMADH(instr),
                        @enumToInt(RSPVUOpcode.VADD ) => iVADD (instr),
                        @enumToInt(RSPVUOpcode.VSUB ) => iVSUB (instr),
                        @enumToInt(RSPVUOpcode.VADDC) => iVADDC(instr),
                        @enumToInt(RSPVUOpcode.VSUBC) => iVSUBC(instr),
                        @enumToInt(RSPVUOpcode.VSAR ) => iVSAR (instr),
                        @enumToInt(RSPVUOpcode.VLT  ) => iVLT  (instr),
                        @enumToInt(RSPVUOpcode.VGE  ) => iVGE  (instr),
                        @enumToInt(RSPVUOpcode.VCL  ) => iVCL  (instr),
                        @enumToInt(RSPVUOpcode.VCH  ) => iVCH  (instr),
                        @enumToInt(RSPVUOpcode.VMRG ) => iVMRG (instr),
                        @enumToInt(RSPVUOpcode.VAND ) => iVAND (instr),
                        @enumToInt(RSPVUOpcode.VOR  ) => iVOR  (instr),
                        @enumToInt(RSPVUOpcode.VXOR ) => iVXOR (instr),
                        @enumToInt(RSPVUOpcode.VRCPL) => iVRCPL(instr),
                        @enumToInt(RSPVUOpcode.VRCPH) => iVRCPH(instr),
                        @enumToInt(RSPVUOpcode.VMOV ) => iVMOV (instr),
                        else => {
                            warn("[RSP] Unhandled COP2 function {X}h ({X}h) @ {X}h.", .{getFunct(instr), instr, rspRegs.cpc});

                            @panic("unhandled RSP instruction");
                        }
                    }
                },
                else => {
                    warn("[RSP] Unhandled COP2 opcode {X}h ({X}h) @ {X}h.", .{getRs(instr), instr, rspRegs.cpc});

                    @panic("unhandled RSP instruction");
                }
            }
        },
        @enumToInt(RSPOpcode.LB   ) => iLB   (instr),
        @enumToInt(RSPOpcode.LH   ) => iLH   (instr),
        @enumToInt(RSPOpcode.LW   ) => iLW   (instr),
        @enumToInt(RSPOpcode.LBU  ) => iLBU  (instr),
        @enumToInt(RSPOpcode.LHU  ) => iLHU  (instr),
        @enumToInt(RSPOpcode.SB   ) => iSB   (instr),
        @enumToInt(RSPOpcode.SH   ) => iSH   (instr),
        @enumToInt(RSPOpcode.SW   ) => iSW   (instr),
        @enumToInt(RSPOpcode.LWC2 ) => {
            switch (getRd(instr)) {
                @enumToInt(RSPVULoadOpcode.LSV) => iLSV(instr),
                @enumToInt(RSPVULoadOpcode.LLV) => iLLV(instr),
                @enumToInt(RSPVULoadOpcode.LDV) => iLDV(instr),
                @enumToInt(RSPVULoadOpcode.LQV) => iLQV(instr),
                @enumToInt(RSPVULoadOpcode.LRV) => iLRV(instr),
                else => {
                    warn("[RSP] Unhandled VU load opcode {X}h ({X}h) @ {X}h.", .{getRd(instr), instr, rspRegs.cpc});

                    @panic("unhandled RSP instruction");
                }
            }
        },
        @enumToInt(RSPOpcode.SWC2 ) => {
            switch (getRd(instr)) {
                @enumToInt(RSPVUStoreOpcode.SBV) => iSBV(instr),
                @enumToInt(RSPVUStoreOpcode.SSV) => iSSV(instr),
                @enumToInt(RSPVUStoreOpcode.SLV) => iSLV(instr),
                @enumToInt(RSPVUStoreOpcode.SDV) => iSDV(instr),
                @enumToInt(RSPVUStoreOpcode.SQV) => iSQV(instr),
                else => {
                    warn("[RSP] Unhandled VU store opcode {X}h ({X}h) @ {X}h.", .{getRd(instr), instr, rspRegs.cpc});

                    @panic("unhandled RSP instruction");
                }
            }
        },
        else => {
            warn("[RSP] Unhandled instruction {X}h ({X}h) @ {X}h.", .{opcode, instr, rspRegs.cpc});

            @panic("unhandled RSP instruction");
        }
    }
}

fn doBranch(target: u32, isCondition: bool, isLink: comptime bool) void {
    if (isLink) rspRegs.set(31, rspRegs.npc);

    if (isCondition) {
        rspRegs.npc = @truncate(u12, target);

        // isBranchDelay = true;
    }
}

fn doReciprocal(input: i32) u32 {
    if (input == 0) return 0x7FFFFFFF;

    var iAbs: u32 = undefined;

    if (input < 0) {
        iAbs = @bitCast(u32, -input);
    } else {
        iAbs = @bitCast(u32, input);
    }

    const idxShift = @truncate(u5, @clz(u32, iAbs) + 1);
    const idx = (iAbs << idxShift) >> 23;

    const resShift = @truncate(u5, 32 - @intCast(u6, idxShift));
    const res = ((0x10000 | @intCast(u32, rcpROM[idx])) << 14) >> resShift;

    if (@bitCast(u32, input) != iAbs) {
        return ~res;
    } else {
        return res;
    }
}

/// ADDIU - ADD Immediate Unsigned
fn iADDIU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rt, rspRegs.get(rs) +% imm);

    if (isDisasm) info("[RSP] ADDIU ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, rspRegs.get(rt)});
}

/// ADDU - ADD Unsigned
fn iADDU(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rs) +% rspRegs.get(rt));

    if (isDisasm) info("[RSP] ADDU ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, rspRegs.get(rd)});
}

/// AND - AND
fn iAND(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rs) & rspRegs.get(rt));

    if (isDisasm) info("[RSP] AND ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, rspRegs.get(rd)});
}

/// ANDI - AND Immediate
fn iANDI(instr: u32) void {
    const imm = @intCast(u32, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rt, rspRegs.get(rs) & imm);

    if (isDisasm) info("[RSP] ANDI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, rspRegs.get(rt)});
}

/// BEQ - Branch on EQual
fn iBEQ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = rspRegs.pc +% offset;

    doBranch(target, rspRegs.get(rs) == rspRegs.get(rt), false);

    if (isDisasm) info("[RSP] BEQ ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, rspRegs.get(rs), rt, rspRegs.get(rt)});
}

/// BGEZ - Branch on Greater than or Equal Zero
fn iBGEZ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = rspRegs.pc +% offset;

    doBranch(target, @bitCast(i32, rspRegs.get(rs)) >= 0, false);

    if (isDisasm) info("[RSP] BGEZ ${}, {X}h; ${} = {X}h", .{rs, target, rs, rspRegs.get(rs)});
}

/// BGTZ - Branch on Greater Than Zero
fn iBGTZ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = rspRegs.pc +% offset;

    doBranch(target, @bitCast(i32, rspRegs.get(rs)) > 0, false);

    if (isDisasm) info("[RSP] BGTZ ${}, {X}h; ${} = {X}h", .{rs, target, rs, rspRegs.get(rs)});
}

/// BLEZ - Branch on Less than or Equal Zero
fn iBLEZ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = rspRegs.pc +% offset;

    doBranch(target, @bitCast(i32, rspRegs.get(rs)) <= 0, false);

    if (isDisasm) info("[RSP] BLEZ ${}, {X}h; ${} = {X}h", .{rs, target, rs, rspRegs.get(rs)});
}

/// BLTZ - Branch on Less Than Zero
fn iBLTZ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = rspRegs.pc +% offset;

    doBranch(target, @bitCast(i32, rspRegs.get(rs)) < 0, false);

    if (isDisasm) info("[RSP] BLTZ ${}, {X}h; ${} = {X}h", .{rs, target, rs, rspRegs.get(rs)});
}

/// BNE - Branch on Not Equal
fn iBNE(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = rspRegs.pc +% offset;

    doBranch(target, rspRegs.get(rs) != rspRegs.get(rt), false);

    if (isDisasm) info("[RSP] BNE ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, rspRegs.get(rs), rt, rspRegs.get(rt)});
}

/// BREAK - Breakpoint
fn iBREAK(instr: u32) void {
    info("BREAK", .{});

    rspRegs.rspStatus.h = true;
    rspRegs.rspStatus.b = true;

    if (rspRegs.rspStatus.ib) {
        rspIRQ = true;

        mi.setPending(InterruptSource.SP);
    }
}

/// CFC - Move From Control
fn iCFC(instr: u32, comptime copN: comptime_int) void {
    const rd = getRd(instr);
    const rt = getRt(instr);

    if (copN == 2) {
        rspRegs.set(rt, 0);
    } else {
        warn("[RSP] Unhandled Coprocessor {}.", .{copN});

        @panic("unhandled coprocessor");
    }

    if (isDisasm) info("[RSP] CFC{} ${}, ${}; ${} = {X}h", .{copN, rt, rd, rt, rspRegs.get(rt)});
}

/// J - Jump
fn iJ(instr: u32) void {
    const target = getTarget(instr);

    doBranch(target, true, false);

    if (isDisasm) info("[RSP] J {X}h", .{target});
}

/// JAL - Jump And Link
fn iJAL(instr: u32) void {
    const target = getTarget(instr);

    doBranch(target, true, true);

    if (isDisasm) info("[RSP] JAL {X}h", .{target});
}

/// JR - Jump Register
fn iJR(instr: u32) void {
    const rs = getRs(instr);

    const target = rspRegs.get(rs);

    doBranch(target, true, false);

    if (isDisasm) info("[RSP] JR ${}; PC = {X}h", .{rs, target});
}

/// LB - Load Byte
fn iLB(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = rspRegs.get(base) +% imm;

    rspRegs.set(rt, @intCast(u32, exts8(readDMEM(u8, addr))));

    if (isDisasm) info("[RSP] LB ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i32, imm), rt, addr, rspRegs.get(rt)});
}

/// LBU - Load Byte Unsigned
fn iLBU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = rspRegs.get(base) +% imm;

    rspRegs.set(rt, @intCast(u32, readDMEM(u8, addr)));

    if (isDisasm) info("[RSP] LBU ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i32, imm), rt, addr, rspRegs.get(rt)});
}

/// LDV - Load Double into Vector register
fn iLDV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 3);

    var addr = rspRegs.get(base) +% offset;

    var element = getElement(instr);

    var end: u32 = undefined;

    if ((element + 8) < 16) {
        end = element + 8;
    } else {
        end = 16;
    }

    while (element < end) : (element += 1) {
        rspRegs.setByte(vt, element, readDMEM(u8, addr));

        addr +%= 1;
    }

    if (isDisasm) info("[RSP] LDV ${}[{}], ${X}({})", .{vt, element, base, @bitCast(i32, offset)});
}

/// LH - Load Halfword
fn iLH(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = rspRegs.get(base) +% imm;

    rspRegs.set(rt, exts16(readDMEM(u16, addr)));

    if (isDisasm) info("[RSP] LH ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i32, imm), rt, addr, rspRegs.get(rt)});
}

/// LHU - Load Halfword Unsigned
fn iLHU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = rspRegs.get(base) +% imm;

    rspRegs.set(rt, @intCast(u32, readDMEM(u16, addr)));

    if (isDisasm) info("[RSP] LHU ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i32, imm), rt, addr, rspRegs.get(rt)});
}

/// LLV - Load Long into Vector register
fn iLLV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 2);

    const addr = rspRegs.get(base) +% offset;

    var i: u32 = 0;
    llvLoop: while (i < 4) : (i += 1) {
        const element = getElement(instr) + i;

        if (element > 15) break :llvLoop;

        rspRegs.setByte(vt, element, readDMEM(u8, addr + i));
    }

    if (isDisasm) info("[RSP] LLV ${}[{}], ${X}({})", .{vt, getElement(instr), base, @bitCast(i32, offset)});
}

/// LQV - Load Quad into Vector register
fn iLQV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 4);

    const addr = rspRegs.get(base) +% offset;

    const element = getElement(instr);

    var i: u32 = 0;
    while ((addr + i) <= ((addr & 0xFFFF_FFF0) + 15)) : (i += 1) {
        rspRegs.setByte(vt, (element + i) & 15, readDMEM(u8, addr + i));
    }

    if (isDisasm) info("[RSP] LQV ${}[0], ${X}({})", .{vt, base, @bitCast(i32, offset)});

    if (isDisasm) {
        i = 0;
        while (i < 8) : (i += 1) {
            info ("{X}h", .{rspRegs.getLane(vt, i)});
        }
    }
}

/// LRV - Load quad Right into Vector register
fn iLRV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 4);

    var addr = rspRegs.get(base) +% offset;

    var i = 16 - ((addr & 0xF) - getElement(instr));
    while (i < 16) : (i += 1) {
        rspRegs.setByte(vt, i & 15, readDMEM(u8, addr));

        addr +%= 1;
    }

    if (isDisasm) info("[RSP] LRV ${}[0], ${X}({})", .{vt, base, @bitCast(i32, offset)});
}

/// LSV - Load Short into Vector register
fn iLSV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 1);

    const addr = rspRegs.get(base) +% offset;

    var data = readDMEM(u16, addr);

    const element = getElement(instr);

    rspRegs.setByte(vt, element, @truncate(u8, data >> 8));

    if (element < 15) {
        rspRegs.setByte(vt, element + 1, @truncate(u8, data));
    }

    if (isDisasm) info("[RSP] LSV ${}[{}], ${X}({})", .{vt, element, base, @bitCast(i32, offset)});
}

/// LUI - Load Upper Immediate
fn iLUI(instr: u32) void {
    const imm = getImm16(instr);

    const rt = getRt(instr);

    rspRegs.set(rt, exts16(imm) << 16);

    if (isDisasm) info("[RSP] LUI ${}, {X}h; ${} = {X}h", .{rt, imm, rt, rspRegs.get(rt)});
}

/// LW - Load Word
fn iLW(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = rspRegs.get(base) +% imm;

    rspRegs.set(rt, readDMEM(u32, addr));

    if (isDisasm) info("[RSP] LW ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i32, imm), rt, addr, rspRegs.get(rt)});
}

/// MFC - Move From Coprocessor
fn iMFC(instr: u32, comptime copN: comptime_int) void {
    const rd = getRd(instr);
    const rt = getRt(instr);

    if (copN == 0) {
        rspRegs.set(rt, rspRegs.getCP0(rd));
    } else if (copN == 2) {
        const lane = getElement(instr) >> 1;

        rspRegs.set(rt, rspRegs.getLane(rd, lane));
    } else {
        warn("[RSP] Unhandled Coprocessor {}.", .{copN});

        @panic("unhandled coprocessor");
    }

    if (isDisasm) info("[RSP] MFC{} ${}, ${}; ${} = {X}h", .{copN, rt, rd, rt, rspRegs.get(rt)});
}

/// MTC - Move To Coprocessor
fn iMTC(instr: u32, comptime copN: comptime_int) void {
    const rd = getRd(instr);
    const rt = getRt(instr);

    const data = rspRegs.get(rt);

    if (copN == 0) {
        rspRegs.setCP0(rd, data);
    } else if (copN == 2) {
        const lane = getElement(instr) >> 1;

        rspRegs.setLane(rd, lane, @truncate(u16, data));
    } else {
        warn("[RSP] Unhandled Coprocessor {}.", .{copN});

        @panic("unhandled coprocessor");
    }

    if (isDisasm) info("[RSP] MTC{} ${}, ${}; ${} = {X}h", .{copN, rt, rd, rd, data});
}

/// OR - OR
fn iOR(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rs) | rspRegs.get(rt));

    if (isDisasm) info("[RSP] OR ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, rspRegs.get(rd)});
}

/// ORI - OR Immediate
fn iORI(instr: u32) void {
    const imm = @intCast(u32, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rt, rspRegs.get(rs) | imm);

    if (isDisasm) info("[RSP] ORI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, rspRegs.get(rt)});
}

/// SB - Store Byte
fn iSB(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = rspRegs.get(base) +% imm;

    writeDMEM(u8, addr, @truncate(u8, rspRegs.get(rt)));

    if (isDisasm) info("[RSP] SB ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i32, imm), addr, @truncate(u8, rspRegs.get(rt))});
}

/// SBV - Store Byte from Vector register
fn iSBV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 0);

    const addr = rspRegs.get(base) +% offset;

    const element = getElement(instr);

    writeDMEM(u8, addr, rspRegs.getByte(vt, element));

    if (isDisasm) info("[RSP] SBV ${}[{}], ${X}({})", .{vt, element, base, @bitCast(i32, offset)});
}

/// SDV - Store Double from Vector register
fn iSDV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 3);

    const addr = rspRegs.get(base) +% offset;

    const element = getElement(instr);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        writeDMEM(u8, addr + i, rspRegs.getByte(vt, (element + i) & 15));
    }

    if (isDisasm) info("[RSP] SDV ${}[{}], ${X}({})", .{vt, element, base, @bitCast(i32, offset)});
}

/// SH - Store Halfword
fn iSH(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = rspRegs.get(base) +% imm;

    writeDMEM(u16, addr, @truncate(u16, rspRegs.get(rt)));

    if (isDisasm) info("[RSP] SH ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i32, imm), addr, @truncate(u16, rspRegs.get(rt))});
}

/// SLL - Shift Left Logical
fn iSLL(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rt) << @truncate(u5, sa));

    if (rd == 0) {
        if (isDisasm) info("[RSP] NOP", .{});
    } else {
        if (isDisasm) info("[RSP] SLL ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, rspRegs.get(rd)});
    }
}

/// SLLV - Shift Left Logical Variable
fn iSLLV(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rt) << @truncate(u5, rspRegs.get(rs)));

    if (isDisasm) info("[RSP] SLLV ${}, ${}, ${}; ${} = {X}h", .{rd, rt, rs, rd, rspRegs.get(rd)});
}

/// SLT - Set on Less Than
fn iSLT(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, @intCast(u32, @bitCast(u1, @bitCast(i32, rspRegs.get(rs)) < @bitCast(i32, rspRegs.get(rt)))));

    if (isDisasm) info("[RSP] SLT ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, rspRegs.get(rd)});
}

/// SLV - Store Long from Vector register
fn iSLV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 2);

    const addr = rspRegs.get(base) +% offset;

    const element = getElement(instr);
    
    var i: u32 = 0;
    while (i < 4) : (i += 1) {
        writeDMEM(u8, addr + i, @truncate(u8, rspRegs.getByte(vt, (element + i) & 15)));
    }

    if (isDisasm) info("[RSP] SLV ${}[{}], ${X}({})", .{vt, element, base, @bitCast(i32, offset)});
}

/// SQV - Store Quad from Vector register
fn iSQV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 4);

    const addr = rspRegs.get(base) +% offset;

    const element = getElement(instr);

    var i: u32 = 0;
    while ((addr + i) <= ((addr & 0xFFFF_FFF0) + 15)) : (i += 1) {
        writeDMEM(u8, addr + i, @truncate(u8, rspRegs.getByte(vt, (element + i) & 15)));
    }

    if (isDisasm) info("[RSP] SQV ${}[{}], ${X}({})", .{vt, element, base, @bitCast(i32, offset)});
}

/// SRA - Shift Right Arithmetic
fn iSRA(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, @bitCast(u32, @bitCast(i32, rspRegs.get(rt)) >> @truncate(u5, sa)));

    if (isDisasm) info("[RSP] SRA ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, rspRegs.get(rd)});
}

/// SRL - Shift Right Logical
fn iSRL(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rt) >> @truncate(u5, sa));

    if (isDisasm) info("[RSP] SRL ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, rspRegs.get(rd)});
}

/// SRLV - Shift Right Logical Variable
fn iSRLV(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rt) >> @truncate(u5, rspRegs.get(rs)));

    if (isDisasm) info("[RSP] SRLV ${}, ${}, ${}; ${} = {X}h", .{rd, rt, rs, rd, rspRegs.get(rd)});
}

/// SSV - Store Short from Vector register
fn iSSV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 1);

    const addr = rspRegs.get(base) +% offset;

    const element = getElement(instr);

    writeDMEM(u16, addr, rspRegs.getLane(vt, element >> 1));

    if (isDisasm) info("[RSP] SSV ${}[{}], ${X}({})", .{vt, element, base, @bitCast(i32, offset)});
}

/// SUBU - SUB Unsigned
fn iSUBU(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rs) -% rspRegs.get(rt));

    if (isDisasm) info("[RSP] SUBU ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, rspRegs.get(rd)});
}

/// SW - Store Word
fn iSW(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = rspRegs.get(base) +% imm;

    writeDMEM(u32, addr, rspRegs.get(rt));

    if (isDisasm) info("[RSP] SW ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i32, imm), addr, rspRegs.get(rt)});
}

//const VCC = packed struct {
//    gte : [8]bool = undefined,
//    lten: [8]bool = undefined,
//};

//const VCO = packed struct {
//    c : [8]bool = undefined,
//    ne: [8]bool = undefined,
//};

//const VCE = packed struct {
//    n1: [8]bool = undefined,
//};

/// VADD - Vector ADD
fn iVADD(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const res = @intCast(i32, s.getSLane(i)) + @intCast(i32, t.getSLane(i)) + @intCast(i32, @bitCast(u1, rspRegs.vco.c[i]));

        rspRegs.acc.setLane(i, 0, @truncate(u16, @bitCast(u32, res)));

        rspRegs.setLane(vd, i, clamps32(res));

        rspRegs.vco.c [i] = false;
        rspRegs.vco.ne[i] = false;
    }

    if (isDisasm) info("[RSP] VADD ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VADDC - Vector ADD Carry
fn iVADDC(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        var res: u16 = undefined;

        rspRegs.vco.c [i] = @subWithOverflow(u16, s.getULane(i), s.getULane(i), &res);
        rspRegs.vco.ne[i] = res != 0;

        rspRegs.acc.setLane(i, 0, res);
        rspRegs.setLane(vd, i, res);
    }

    if (isDisasm) info("[RSP] VADD ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VAND - Vector AND
fn iVAND(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        rspRegs.acc.setLane(i, 0, s.getULane(i) & t.getULane(i));

        rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 0));
    }

    if (isDisasm) info("[RSP] VAND ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VCH - Vector Select Clip Test High
fn iVCH(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const eS = s.getSLane(i);
        const eT = t.getSLane(i);

        rspRegs.vco.c[i] = ((eS >> 15) & 1) != ((eT >> 15) & 1);

        const c = rspRegs.vcc.gte[i];

        var eAbs: i16 = undefined;

        if (c) {
            eAbs = -eT;
        } else {
            eAbs = eT;
        }

        rspRegs.vce.n1[i] = c and (eS == (-eT -% 1));

        const n1 = rspRegs.vce.n1[i];

        rspRegs.vco.ne[i] = !n1 and (eS != eAbs);

        rspRegs.vcc.lten[i] = eS <= -eT;
        rspRegs.vcc.gte [i] = eS >= eT;

        var clip: bool = undefined;

        if (c) {
            clip = rspRegs.vcc.lten[i];
        } else {
            clip = rspRegs.vcc.gte[i];
        }

        if (clip) {
            rspRegs.acc.setSLane(i, @intCast(i48, eAbs));
        } else {
            rspRegs.acc.setSLane(i, @intCast(i48, eS));
        }

        rspRegs.setSLane(vd, i, @truncate(i16, rspRegs.acc.getSLane(i)));
    }

    if (isDisasm) info("[RSP] VCH ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VCL - Vector Select Clip Test Low
fn iVCL(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const eS = s.getULane(i);
        const eT = t.getULane(i);

        const c  = rspRegs.vco.c [i];
        const ne = rspRegs.vco.ne[i];

        if (!c and !ne) {
            rspRegs.vcc.gte[i] = eS >= eT;
        }

        const nET = @bitCast(u16, -@bitCast(i16, eT));

        if (c and !ne) {
            const lte = eS <= nET;
            const eql = eS == nET;

            rspRegs.vcc.lten[i] = lte and eql;
        }

        var clip: bool = undefined;

        if (c) {
            clip = rspRegs.vcc.lten[i];
        } else {
            clip = rspRegs.vcc.gte[i];
        }

        var eAbs: u16 = undefined;

        if (c) {
            eAbs = nET;
        } else {
            eAbs = eT;
        }

        if (clip) {
            rspRegs.acc.setSLane(i, @bitCast(i48, @intCast(u48, eAbs)));
        } else {
            rspRegs.acc.setSLane(i, @bitCast(i48, @intCast(u48, eS)));
        }

        rspRegs.setSLane(vd, i, @truncate(i16, rspRegs.acc.getSLane(i)));
    }

    if (isDisasm) info("[RSP] VCL ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VGE - Vector select Greater than or Equal
fn iVGE(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const eql = s.getSLane(i) == t.getSLane(i);
        const neg = eql and !(rspRegs.vco.c[i] and rspRegs.vco.ne[i]);

        rspRegs.vcc.gte[i] = neg or (s.getSLane(i) > t.getSLane(i));

        if (rspRegs.vcc.gte[i]) {
            rspRegs.acc.setLane(i, 0, s.getULane(i));
        } else {
            rspRegs.acc.setLane(i, 0, t.getULane(i));
        }

        rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 0));

        rspRegs.vcc.lten[i] = false;
        rspRegs.vco.c   [i] = false;
        rspRegs.vco.ne  [i] = false;
    }

    if (isDisasm) info("[RSP] VGE ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VLT - Vector select Less Than
fn iVLT(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const eql = s.getSLane(i) == t.getSLane(i);
        const neg = eql and rspRegs.vco.c[i] and rspRegs.vco.ne[i];

        rspRegs.vcc.gte[i] = neg or (s.getSLane(i) < t.getSLane(i));

        if (rspRegs.vcc.gte[i]) {
            rspRegs.acc.setLane(i, 0, s.getULane(i));
        } else {
            rspRegs.acc.setLane(i, 0, t.getULane(i));
        }

        rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 0));

        rspRegs.vcc.lten[i] = false;
        rspRegs.vco.c   [i] = false;
        rspRegs.vco.ne  [i] = false;
    }

    if (isDisasm) info("[RSP] VLT ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMACF - Vector Multiply and ACcumulate of signed Fractions
fn iVMACF(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getSLane(i)) * @intCast(i32, t.getSLane(i)) * 2;

        rspRegs.acc.setSLane(i, rspRegs.acc.getSLane(i) + @intCast(i48, prod));

        rspRegs.setLane(vd, i, clamps32(@truncate(i32, rspRegs.acc.getSLane(i) >> 16)));
    }

    if (isDisasm) info("[RSP] VMACF ${}, ${}, ${}[{}]", .{vd, vs, vt, e});

    if (isDisasm) {
        i = 0;
        while (i < 8) : (i += 1) {
            info ("{X}h", .{rspRegs.getLane(vd, i)});
        }
    }
}

/// VMACU - Vector Multiply and ACcumulate of Unsigned fractions
fn iVMACU(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getSLane(i)) * @intCast(i32, t.getSLane(i)) * 2;

        rspRegs.acc.setSLane(i, rspRegs.acc.getSLane(i) + @intCast(i48, prod));

        rspRegs.setLane(vd, i, clampu32(@truncate(i32, rspRegs.acc.getSLane(i) >> 16)));
    }

    if (isDisasm) info("[RSP] VMACU ${}, ${}, ${}[{}]", .{vd, vs, vt, e});

    if (isDisasm) {
        i = 0;
        while (i < 8) : (i += 1) {
            info ("{X}h", .{rspRegs.getLane(vd, i)});
        }
    }
}

/// VMADH - Vector Multiply of high partial products?
fn iVMADH(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getSLane(i)) * @intCast(i32, t.getSLane(i));

        rspRegs.acc.setSLane(i, rspRegs.acc.getSLane(i) + (@intCast(i48, prod) << 16));

        rspRegs.setLane(vd, i, clamps32(@truncate(i32, rspRegs.acc.getSLane(i) >> 16)));
    }

    if (isDisasm) info("[RSP] VMADH ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMADL - Vector Multiply of low partial products?
fn iVMADL(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(u32, s.getULane(i)) * @intCast(u32, t.getULane(i));

        rspRegs.acc.setSLane(i, rspRegs.acc.getSLane(i) + (@intCast(i48, prod) >> 16));

        // Taken from Dillonb's N64 emulator (see iVMADN())
        if (isSignExtended(rspRegs.acc.getLane(i, 0), rspRegs.acc.getLane(i, 1))) {
            rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 2));
        } else if (@bitCast(i16, rspRegs.acc.getLane(i, 0)) < 0) {
            rspRegs.setLane(vd, i, 0);
        } else {
            rspRegs.setLane(vd, i, 0xFFFF);
        }
    }

    if (isDisasm) info("[RSP] VMADL ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMADM - Vector Multiply of mid partial products?
fn iVMADM(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getSLane(i)) * @intCast(i32, t.getULane(i));

        rspRegs.acc.setSLane(i, rspRegs.acc.getSLane(i) + @intCast(i48, prod));

        rspRegs.setLane(vd, i, clamps32(@truncate(i32, rspRegs.acc.getSLane(i) >> 16)));
    }

    if (isDisasm) info("[RSP] VMADM ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMADN - Vector Multiply of mid partial products?
fn iVMADN(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getULane(i)) * @intCast(i32, t.getSLane(i));

        rspRegs.acc.setSLane(i, rspRegs.acc.getSLane(i) + @intCast(i48, prod));

        // Taken from Dillonb's N64 emulator (https://github.com/Dillonb/n64/blob/68a0c186d29b3fa02ec038782313c5ae8e181c06/src/cpu/rsp_vector_instructions.c#L1058)
        if (isSignExtended(rspRegs.acc.getLane(i, 0), rspRegs.acc.getLane(i, 1))) {
            rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 2));
        } else if (@bitCast(i16, rspRegs.acc.getLane(i, 0)) < 0) {
            rspRegs.setLane(vd, i, 0);
        } else {
            rspRegs.setLane(vd, i, 0xFFFF);
        }
    }

    if (isDisasm) info("[RSP] VMADN ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMOV - Vector MOVe
fn iVMOV(instr: u32) void {
    const vd = getVd(instr);
    const vt = getVt(instr);

    const eD = getDElement(instr);

    var eS: u32 = undefined;
    
    switch (getElement(instr)) {
        0 ...  1 => eS = (getElement(instr) & 0) | (getVs(instr) & 7),
        2 ...  3 => eS = (getElement(instr) & 1) | (getVs(instr) & 6),
        4 ...  7 => eS = (getElement(instr) & 3) | (getVs(instr) & 4),
        8 ... 15 => eS = (getElement(instr) & 7) | (getVs(instr) & 0),
        else => unreachable,
    }

    rspRegs.setLane(vd, eD, rspRegs.broadcast(vt, getBroadcastMod(instr)).getULane(eS));

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        rspRegs.acc.setLane(i, 0, rspRegs.broadcast(vt, getBroadcastMod(instr)).getULane(i));
    }

    if (isDisasm) info("[RSP] VMOV ${}[{}], ${}[{}]", .{vd, eD, vt, eS});
}

/// VMRG - Vector MeRGe
fn iVMRG(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        if (rspRegs.vcc.gte[i]) {
            rspRegs.acc.setLane(i, 0, s.getULane(i));
        } else {
            rspRegs.acc.setLane(i, 0, t.getULane(i));
        }

        rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 0));

        rspRegs.vco.c   [i] = false;
        rspRegs.vco.ne  [i] = false;
    }

    if (isDisasm) info("[RSP] VMRG ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMUDH - Vector Multiply of high partial products?
fn iVMUDH(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getSLane(i) * @intCast(i32, t.getSLane(i)));

        rspRegs.acc.setSLane(i, @intCast(i48, prod) << 16);

        rspRegs.setLane(vd, i, clamps32(prod));
    }

    if (isDisasm) info("[RSP] VMUDH ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMUDL - Vector Multiply of low partial products?
fn iVMUDL(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(u32, s.getULane(i)) * @intCast(u32, t.getULane(i));

        rspRegs.acc.setULane(i, @intCast(u48, prod) >> 16);

        rspRegs.setLane(vd, i, clampu32(@truncate(i32, rspRegs.acc.getSLane(i))));
    }

    if (isDisasm) info("[RSP] VMUDL ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMUDM - Vector Multiply of mid partial products?
fn iVMUDM(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getSLane(i)) * @intCast(i32, t.getSLane(i));

        rspRegs.acc.setSLane(i, @intCast(i48, prod));

        rspRegs.setLane(vd, i, clamps32(@truncate(i32, rspRegs.acc.getSLane(i)) >> 16));
    }

    if (isDisasm) info("[RSP] VMUDM ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMUDN - Vector Multiply of mid partial products?
fn iVMUDN(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getULane(i) * @intCast(i32, t.getSLane(i)));

        rspRegs.acc.setSLane(i, @intCast(i48, prod));

        // Taken from Dillonb's N64 emulator (see iVMADN())
        if (isSignExtended(rspRegs.acc.getLane(i, 0), rspRegs.acc.getLane(i, 1))) {
            rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 2));
        } else if (@bitCast(i16, rspRegs.acc.getLane(i, 0)) < 0) {
            rspRegs.setLane(vd, i, 0);
        } else {
            rspRegs.setLane(vd, i, 0xFFFF);
        }
    }

    if (isDisasm) info("[RSP] VMUDN ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMULF - Vector MULtiply of signed Fractions
fn iVMULF(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getSLane(i)) * @intCast(i32, t.getSLane(i)) * 2 + 0x8000;

        rspRegs.acc.setSLane(i, @intCast(i48, prod));

        rspRegs.setLane(vd, i, clamps32(@truncate(i32, rspRegs.acc.getSLane(i) >> 16)));
    }

    if (isDisasm) info("[RSP] VMULF ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VMULU - Vector MULtiply of Unsigned fractions
fn iVMULU(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const prod = @intCast(i32, s.getSLane(i)) * @intCast(i32, t.getSLane(i)) * 2 + 0x8000;

        rspRegs.acc.setSLane(i, @intCast(i48, prod));

        rspRegs.setLane(vd, i, clampu32(@truncate(i32, rspRegs.acc.getSLane(i) >> 16)));
    }

    if (isDisasm) info("[RSP] VMULU ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VOR - Vector OR
fn iVOR(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        rspRegs.acc.setLane(i, 0, s.getULane(i) | t.getULane(i));

        rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 0));
    }

    if (isDisasm) info("[RSP] VOR ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VRCPH - Vector ReCiProcal High
fn iVRCPH(instr: u32) void {
    const vd = getVd(instr);
    const vt = getVt(instr);

    const eD = getDElement(instr);

    var eS: u32 = undefined;
    
    switch (getElement(instr)) {
        0 ...  1 => eS = (getElement(instr) & 0) | (getVs(instr) & 7),
        2 ...  3 => eS = (getElement(instr) & 1) | (getVs(instr) & 6),
        4 ...  7 => eS = (getElement(instr) & 3) | (getVs(instr) & 4),
        8 ... 15 => eS = (getElement(instr) & 7) | (getVs(instr) & 0),
        else => unreachable,
    }

    rspRegs.setLane(vd, eD, rspRegs.divOut);

    rspRegs.divIn = rspRegs.getLane(vt, eS);

    rspRegs.isDIVInLoaded = true;

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        rspRegs.acc.setLane(i, 0, rspRegs.broadcast(vt, getBroadcastMod(instr)).getULane(i));
    }

    if (isDisasm) info("[RSP] VRCPH ${}[{}], ${}[{}]", .{vd, eD, vt, eS});
}

/// VRCPL - Vector ReCiProcal Low
fn iVRCPL(instr: u32) void {
    const vd = getVd(instr);
    const vt = getVt(instr);

    const eD = getDElement(instr);

    var eS: u32 = undefined;
    
    switch (getElement(instr)) {
        0 ...  1 => eS = (getElement(instr) & 0) | (getVs(instr) & 7),
        2 ...  3 => eS = (getElement(instr) & 1) | (getVs(instr) & 6),
        4 ...  7 => eS = (getElement(instr) & 3) | (getVs(instr) & 4),
        8 ... 15 => eS = (getElement(instr) & 7) | (getVs(instr) & 0),
        else => unreachable,
    }

    var input: i32 = undefined;

    if (rspRegs.isDIVInLoaded) {
        input = @bitCast(i32, (@intCast(u32, rspRegs.divIn) << 16) | rspRegs.getLane(vt, eS));
    } else {
        input = @intCast(i32, @bitCast(i16, rspRegs.getLane(vt, eS)));
    }

    const res = doReciprocal(input);

    rspRegs.setLane(vd, eD, @truncate(u16, res));

    rspRegs.divOut = @truncate(u16, res >> 16);

    rspRegs.divIn = 0;
    rspRegs.isDIVInLoaded = false;

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        rspRegs.acc.setLane(i, 0, rspRegs.broadcast(vt, getBroadcastMod(instr)).getULane(i));
    }

    if (isDisasm) info("[RSP] VRCPL ${}[{}], ${}[{}]", .{vd, eD, vt, eS});
}

/// VRNDP - Vector RouND Positive
fn iVRNDP(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        var prod: u32 = undefined;

        if (vs == 1) {
            prod = exts16(t.getULane(i)) << 16;
        } else {
            prod = exts16(t.getULane(i));
        }

        rspRegs.acc.setSLane(i, rspRegs.acc.getSLane(i) + @bitCast(i48, exts32(prod)));

        rspRegs.setLane(vd, i, clamps32(@truncate(i32, rspRegs.acc.getSLane(i) >> 16)));
    }

    if (isDisasm) info("[RSP] VRNDP ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VSAR - Vector Select Accumulator Read
fn iVSAR(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        switch (e) {
            0x8  => rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 0)),
            0x9  => rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 1)),
            0xA  => rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 2)),
            else => rspRegs.setLane(vd, i, 0),
        }
    }

    if (isDisasm) info("[RSP] VSAR ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VSUB - Vector SUBtract
fn iVSUB(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        const res = @intCast(i32, s.getSLane(i)) - @intCast(i32, t.getSLane(i)) - @intCast(i32, @bitCast(u1, rspRegs.vco.c[i]));

        rspRegs.acc.setLane(i, 0, @truncate(u16, @bitCast(u32, res)));

        rspRegs.setLane(vd, i, clamps32(res));

        rspRegs.vco.c [i] = false;
        rspRegs.vco.ne[i] = false;
    }

    if (isDisasm) info("[RSP] VSUB ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VSUBC - Vector SUBtract Carry
fn iVSUBC(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        var res: u16 = undefined;

        rspRegs.vco.c [i] = @subWithOverflow(u16, s.getULane(i), t.getULane(i), &res);
        rspRegs.vco.ne[i] = res != 0;

        rspRegs.acc.setLane(i, 0, res);
        rspRegs.setLane(vd, i, res);
    }

    if (isDisasm) info("[RSP] VSUBC ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// VXOR - Vector XOR
fn iVXOR(instr: u32) void {
    const vd = getVd(instr);
    const vs = getVs(instr);
    const vt = getVt(instr);

    const e = getBroadcastMod(instr);

    const s = rspRegs.getVPR(vs);
    const t = rspRegs.broadcast(vt, e);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        rspRegs.acc.setLane(i, 0, s.getULane(i) ^ t.getULane(i));

        rspRegs.setLane(vd, i, rspRegs.acc.getLane(i, 0));
    }

    if (isDisasm) info("[RSP] VXOR ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

/// XOR - XOR
fn iXOR(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rs) ^ rspRegs.get(rt));

    if (isDisasm) info("[RSP] XOR ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, rspRegs.get(rd)});
}

/// XORI - XOR Immediate
fn iXORI(instr: u32) void {
    const imm = @intCast(u32, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rt, rspRegs.get(rs) ^ imm);

    if (isDisasm) info("[RSP] XORI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, rspRegs.get(rt)});
}

pub fn step() void {
    if (rspRegs.rspStatus.h ) return;
    if (rspRegs.rspStatus.ss) @panic("rsp single step");

    const instr = fetchInstr();

    decodeInstr(instr);
}
