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

const mi = @import("mi.zig");

const InterruptSource = mi.InterruptSource;

/// Clamp 32-bit signed data
fn clamps32(data: i32) i16 {
    if (data < -32768) return -32768;
    if (data >  32767) return  32767;

    return @truncate(i16, data);
}

/// Sign extend 8-bit data
fn exts8(data: u8) u32 {
    return @bitCast(u32, @intCast(i32, @bitCast(i8, data)));
}

/// Sign extend 16-bit data
fn exts16(data: u16) u32 {
    return @bitCast(u32, @intCast(i32, @bitCast(i16, data)));
}

/// Get VU byte index
fn getIdxB(idx: u32) u32 {
    return 15 - idx;
}

/// Get VU lane index
fn getIdxL(idx: u32) u32 {
    return 7 - idx;
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
    LUI   = 0x0F,
    COP0  = 0x10,
    COP2  = 0x12,
    LH    = 0x21,
    LW    = 0x23,
    LHU   = 0x25,
    SH    = 0x29,
    SW    = 0x2B,
    LWC2  = 0x32,
    SWC2  = 0x3A,
};

const RSPSpecial = enum(u32) {
    SLL  = 0x00,
    SRL  = 0x02,
    JR   = 0x08,
    ADD  = 0x20,
    ADDU = 0x21,
};

const RSPCOPOpcode = enum(u32) {
    MF = 0x00,
    MT = 0x04,
};

const RSPCP2Opcode = enum(u32) {
    COMPUTE = 0x10,
};

const RSPVULoadOpcode = enum(u32) {
    LDV = 0x03,
    LQV = 0x04,
};

const RSPVUStoreOpcode = enum(u32) {
    SDV = 0x03,
    SQV = 0x04,
};

const RSPVUOpcode = enum(u32) {
    VMULF = 0x00,
    VMACF = 0x08,
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
    sLane: [8 ]i16,
    uLane: [8 ]u16,
    byte : [16]u8,

    pub fn getSLane(self: VectorReg, idx: u32) i16 {
        return self.sLane[getIdxL(idx)];
    }

    pub fn getULane(self: VectorReg, idx: u32) u16 {
        return self.uLane[getIdxL(idx)];
    }

    pub fn setSLane(self: *VectorReg, idx: u32, data: i16) void {
        self.sLane[getIdxL(idx)] = data;
    }

    pub fn setULane(self: *VectorReg, idx: u32, data: u16) void {
        self.uLane[getIdxL(idx)] = data;
    }
};

const Accumulator = union {
    sLane: [8]i48,
    uLane: [8]u48,

    pub fn getSLane(self: Accumulator, idx: u32) i48 {
        return self.sLane[getIdxL(idx)];
    }

    pub fn getULane(self: Accumulator, idx: u32) u48 {
        return self.uLane[getIdxL(idx)];
    }

    pub fn setSLane(self: *Accumulator, idx: u32, data: i48) void {
        self.sLane[getIdxL(idx)] = data;
    }

    pub fn setULane(self: *Accumulator, idx: u32, data: u48) void {
        self.uLane[getIdxL(idx)] = data;
    }
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
        return self.vprs[idx].byte[getIdxB(element)];
    }

    pub fn getLane(self: RSPRegs, idx: u32, element: u32) u16 {
        return self.vprs[idx].uLane[getIdxL(element)];
    }

    pub fn getVector(self: RSPRegs, idx: u32) u128 {
        var data: u128 = undefined;

        @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &vprs[idx].byte), 16);

        return @byteSwap(u128, data);
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
            11 => {
                warn("[RSP] Unhandled CP0 read @ ${} (RDP Status).", .{idx});

                data = 0;
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
        self.vprs[idx].byte[getIdxB(element)] = data;
    }

    pub fn setLane(self: *RSPRegs, idx: u32, element: u32, data: u16) void {
        self.vprs[idx].uLane[getIdxL(element)] = data;
    }

    pub fn setSLane(self: *RSPRegs, idx: u32, element: u32, data: i16) void {
        self.vprs[idx].sLane[getIdxL(element)] = data;
    }

    pub fn setVector(self: *RSPRegs, idx: u32, data: u128) void {
        const data_ = @byteSwap(u128, data);

        @memcpy(@ptrCast([*]u8, &vprs[idx].byte), @ptrCast([*]const u8, &data_), 16);
    }

    pub fn setCP0(self: RSPRegs, idx: u32, data: u32) void {
        switch (idx) {
            0 => rspRegs.rspAddr = @bitCast(RSPAddr, @truncate(u13, data)),
            1 => rspRegs.rspDRAMAddr = @truncate(u24, data),
            2 => {
                rspRegs.rspDMALen = @bitCast(RSPDMALen, data);

                doDMAToRSP();
            },
            7 => {
                if (data == 0) rspRegs.rspSema = false;
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

fn getBroadcastMod(instr: u32) u32 {
    return (instr >> 21) & 0xF;
}

fn getOffset(instr: u32, comptime n: comptime_int) u32 {
    return @bitCast(u32, @intCast(i32, @bitCast(i7, @truncate(u7, instr)))) << n;
}

const getVd = getRd;
const getVs = getRs;
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
            info("[RSP] Write32 @ pAddr {X}h (RSP Status), data: {X}h.", .{pAddr, data});

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

    @memcpy(@ptrCast([*]u8, &spDMEM[pAddr & 0xFFF]), @ptrCast([*]u8, &data_), @sizeOf(T));
}

fn fetchInstr() u32 {
    var data: u32 = undefined;

    rspRegs.cpc = rspRegs.pc;

    @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &spIMEM[rspRegs.pc & 0xFFF]), 4);

    rspRegs.pc  = rspRegs.npc;
    rspRegs.npc +%= 4;

    return @byteSwap(u32, data);
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
            mem.?[rspAddr + count_ * length + length_] = bus.ram[ramAddr + count_ * (length + skip) + length_];
        }
    }

    rspIRQ = true;

    mi.setPending(InterruptSource.SP);
}

fn decodeInstr(instr: u32) void {
    const opcode = instr >> 26;

    switch (opcode) {
        @enumToInt(RSPOpcode.SPECIAL) => {
            const funct = instr & 0x3F;

            switch (funct) {
                @enumToInt(RSPSpecial.SLL ) => iSLL (instr),
                @enumToInt(RSPSpecial.SRL ) => iSRL (instr),
                @enumToInt(RSPSpecial.JR  ) => iJR  (instr),
                @enumToInt(RSPSpecial.ADD ) => iADDU(instr),
                @enumToInt(RSPSpecial.ADDU) => iADDU(instr),
                else => {
                    warn("[RSP] Unhandled function {X}h ({X}h) @ {X}h.", .{funct, instr, rspRegs.cpc});

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
                @enumToInt(RSPCOPOpcode.MT) => iMTC(instr, 2),
                @enumToInt(RSPCP2Opcode.COMPUTE) ... @enumToInt(RSPCP2Opcode.COMPUTE) + 0xF => {
                    switch (getFunct(instr)) {
                        @enumToInt(RSPVUOpcode.VMULF) => iVMULF(instr),
                        @enumToInt(RSPVUOpcode.VMACF) => iVMACF(instr),
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
        @enumToInt(RSPOpcode.LH   ) => iLH   (instr),
        @enumToInt(RSPOpcode.LW   ) => iLW   (instr),
        @enumToInt(RSPOpcode.LHU  ) => iLHU  (instr),
        @enumToInt(RSPOpcode.SH   ) => iSH   (instr),
        @enumToInt(RSPOpcode.SW   ) => iSW   (instr),
        @enumToInt(RSPOpcode.LWC2 ) => {
            switch (getRd(instr)) {
                @enumToInt(RSPVULoadOpcode.LDV) => iLDV(instr),
                @enumToInt(RSPVULoadOpcode.LQV) => iLQV(instr),
                else => {
                    warn("[RSP] Unhandled VU load opcode {X}h ({X}h) @ {X}h.", .{getRd(instr), instr, rspRegs.cpc});

                    @panic("unhandled RSP instruction");
                }
            }
        },
        @enumToInt(RSPOpcode.SWC2 ) => {
            switch (getRd(instr)) {
                @enumToInt(RSPVUStoreOpcode.SDV) => iSDV(instr),
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

/// BNE - Branch on Not Equal
fn iBNE(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = rspRegs.pc +% offset;

    doBranch(target, rspRegs.get(rs) != rspRegs.get(rt), false);

    if (isDisasm) info("[RSP] BNE ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, rspRegs.get(rs), rt, rspRegs.get(rt)});
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

/// LDV - Load Double into Vector register
fn iLDV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 3);

    const addr = rspRegs.get(base) +% offset;

    var data = readDMEM(u64, addr);

    const element  = getElement(instr);
    var   element_ = element;
    while (element_ < (element + 8) and element_ < 16) : (element_ += 1) {
        rspRegs.setByte(vt, element_, @truncate(u8, data));

        data >>= 8;
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

/// LQV - Load Quad into Vector register
fn iLQV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 4);

    const addr = rspRegs.get(base) +% offset;

    var data = readDMEM(u128, addr);

    var i: u32 = 0;
    while (i < (addr & 15)) : (i += 1) {
        rspRegs.setByte(vt, i, @truncate(u8, data));

        data >>= 8;
    }

    if (isDisasm) info("[RSP] LQV ${}[0], ${X}({})", .{vt, base, @bitCast(i32, offset)});
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

/// ORI - OR Immediate
fn iORI(instr: u32) void {
    const imm = @intCast(u32, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    rspRegs.set(rt, rspRegs.get(rs) | imm);

    if (isDisasm) info("[RSP] ORI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, rspRegs.get(rt)});
}

/// SDV - Store Double from Vector register
fn iSDV(instr: u32) void {
    const base = getRs(instr);
    const vt   = getVt(instr);

    const offset = getOffset(instr, 3);

    const addr = rspRegs.get(base) +% offset;

    var data: u64 = 0;

    const element = getElement(instr);

    var i: u32 = 0;
    while (i < 8) : (i += 1) {
        data |= @intCast(u64, rspRegs.getByte(vt, (element + i) & 31));

        data <<= 8;
    }

    writeDMEM(u64, addr, data);

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

/// SRL - Shift Right Logical
fn iSRL(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    rspRegs.set(rd, rspRegs.get(rt) >> @truncate(u5, sa));

    if (isDisasm) info("[RSP] SRL ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, rspRegs.get(rd)});
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
        const prod = @intCast(u32, s.getULane(i)) * @intCast(u32, t.getULane(i)) * 2;

        rspRegs.acc.setSLane(i, rspRegs.acc.getSLane(i) + @intCast(i48, @bitCast(i32, prod)));

        rspRegs.setSLane(vd, i, clamps32(@truncate(i32, rspRegs.acc.getSLane(i) >> 16)));
    }

    if (isDisasm) info("[RSP] VMACF ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
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
        const prod = @intCast(u32, s.getULane(i)) * @intCast(u32, t.getULane(i)) * 2;

        rspRegs.acc.setSLane(i, @intCast(i48, @bitCast(i32, prod + 0x8000)));

        rspRegs.setSLane(vd, i, clamps32(@truncate(i32, rspRegs.acc.getSLane(i) >> 16)));
    }

    if (isDisasm) info("[RSP] VMULF ${}, ${}, ${}[{}]", .{vd, vs, vt, e});
}

pub fn step() void {
    if (rspRegs.rspStatus.h) return;

    const instr = fetchInstr();

    decodeInstr(instr);
}
