//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! cpu.zig - VR4300i CPU interpreter module. 
//!

const std = @import("std");

const info = std.log.info;
const warn = std.log.warn;

const bus  = @import("bus.zig");
const cop0 = @import("cop0.zig");
const cop1 = @import("cop1.zig");

/// Sign extend 8-bit data
fn exts8(data: u8) u64 {
    return @bitCast(u64, @intCast(i64, @bitCast(i8, data)));
}

/// Sign extend 16-bit data
fn exts16(data: u16) u64 {
    return @bitCast(u64, @intCast(i64, @bitCast(i16, data)));
}

/// Sign extend 32-bit data
fn exts32(data: u32) u64 {
    return @bitCast(u64, @intCast(i64, @bitCast(i32, data)));
}

/// Register aliases
const CPUReg = enum(u32) {
    R0 =  0, AT =  1, V0 =  2, V1 =  3,
    A0 =  4, A1 =  5, A2 =  6, A3 =  7,
    T0 =  8, T1 =  9, T2 = 10, T3 = 11,
    T4 = 12, T5 = 13, T6 = 14, T7 = 15,
    S0 = 16, S1 = 17, S2 = 18, S3 = 19,
    S4 = 20, S5 = 21, S6 = 22, S7 = 23,
    T8 = 24, T9 = 25, K0 = 26, K1 = 27,
    GP = 28, SP = 29, S8 = 30, RA = 31,
};

/// OPCODE field
const Opcode = enum(u32) {
    SPECIAL = 0x00,
    REGIMM  = 0x01,

    J      = 0x02,
    JAL    = 0x03,
    BEQ    = 0x04,
    BNE    = 0x05,
    BLEZ   = 0x06,
    BGTZ   = 0x07,
    ADDI   = 0x08,
    ADDIU  = 0x09,
    SLTI   = 0x0A,
    SLTIU  = 0x0B,
    ANDI   = 0x0C,
    ORI    = 0x0D,
    XORI   = 0x0E,
    LUI    = 0x0F,
    COP0   = 0x10,
    COP1   = 0x11,
    BEQL   = 0x14,
    BNEL   = 0x15,
    BLEZL  = 0x16,
    BGTZL  = 0x17,
    DADDI  = 0x18,
    DADDIU = 0x19,
    LDL    = 0x1A,
    LDR    = 0x1B,
    LB     = 0x20,
    LH     = 0x21,
    LWL    = 0x22,
    LW     = 0x23,
    LBU    = 0x24,
    LHU    = 0x25,
    LWR    = 0x26,
    LWU    = 0x27,
    SB     = 0x28,
    SH     = 0x29,
    SWL    = 0x2A,
    SW     = 0x2B,
    SWR    = 0x2E,
    CACHE  = 0x2F,
    LWC1   = 0x31,
    LDC1   = 0x35,
    LD     = 0x37,
    SWC1   = 0x39,
    SDC1   = 0x3D,
    SD     = 0x3F,
};

/// SPECIAL
const Special = enum(u32) {
    SLL    = 0x00,
    SRL    = 0x02,
    SRA    = 0x03,
    SLLV   = 0x04,
    SRLV   = 0x06,
    SRAV   = 0x07,
    JR     = 0x08,
    JALR   = 0x09,
    MFHI   = 0x10,
    MTHI   = 0x11,
    MFLO   = 0x12,
    MTLO   = 0x13,
    MULT   = 0x18,
    MULTU  = 0x19,
    DIV    = 0x1A,
    DIVU   = 0x1B,
    DMULTU = 0x1D,
    DDIVU  = 0x1F,
    ADD    = 0x20,
    ADDU   = 0x21,
    SUBU   = 0x23,
    SUB    = 0x22,
    AND    = 0x24,
    OR     = 0x25,
    XOR    = 0x26,
    NOR    = 0x27,
    SLT    = 0x2A,
    SLTU   = 0x2B,
    DADD   = 0x2C,
    DADDU  = 0x2D,
    DSLL   = 0x38,
    DSLL32 = 0x3C,
    DSRA32 = 0x3F,
};

/// REGIMM
const Regimm = enum(u32) {
    BLTZ   = 0x00,
    BGEZ   = 0x01,
    BLTZL  = 0x02,
    BGEZL  = 0x03,
    BGEZAL = 0x11,
};

/// COP opcode
const COP = enum(u32) {
    MF = 0x00,
    CF = 0x02,
    MT = 0x04,
    CT = 0x06,
    BC = 0x08,
    CO = 0x10,
};

/// BC opcode
const BC = enum(u32) {
    BCF, BCT, BCFL, BCTL,
};

/// COP1 Opcode
const COP1 = enum(u32) {
    S = 0x10,
    D = 0x11,
    W = 0x14,
};

/// CO function
const CO = enum(u32) {
    TLBR  = 0x01,
    TLBWI = 0x02,
    TLBP  = 0x08,
    ERET  = 0x18,
};

/// COP1 function
const CO1 = enum(u32) {
    ADD     = 0x00,
    SUB     = 0x01,
    MUL     = 0x02,
    DIV     = 0x03,
    SQRT    = 0x04,
    MOV     = 0x06,
    TRUNC_W = 0x0D,
    CVT_S   = 0x20,
    CVT_D   = 0x21,
    CVT_W   = 0x24,
    C       = 0x30,
};

/// VR4300i register file
const RegFile = struct {
    gprs: [32]u64 = undefined,

    lo: u64 = undefined,
    hi: u64 = undefined,

    pc : u64 = undefined,
    cpc: u64 = undefined,
    npc: u64 = undefined,

    /// Reads GPR (64-bit)
    pub fn get(self: RegFile, idx: u32) u64 {
        //if (idx == 26) isDisasm = true;
        return self.gprs[idx];
    }

    /// Sets GPR (32-bit), optionally sign extends it
    pub fn set32(self: *RegFile, idx: u32, data: u32, isSignExtend: bool) void {
        var data_ = @intCast(u64, data);

        if (isSignExtend) {
            data_ = exts32(data);
        }

        self.gprs[idx] = data_;

        self.gprs[@enumToInt(CPUReg.R0)] = 0;

        // if (idx == 26) isDisasm = true;
    }

    /// Sets PC (32-bit), sign extends it
    pub fn setPC32(self: *RegFile, data: u32) void {
        if ((data & 3) != 0) @panic("unaligned pc");

        self.pc  = exts32(data);
        self.npc = self.pc +% 4;
    }

    /// Sets GPR (64-bit)
    pub fn set64(self: *RegFile, idx: u32, data: u64) void {
        self.gprs[idx] = data;

        self.gprs[@enumToInt(CPUReg.R0)] = 0;

        // if (idx == 26) isDisasm = true;
    }

    /// Sets PC (64-bit)
    pub fn setPC64(self: *RegFile, data: u64) void {
        if ((data & 3) != 0) @panic("unaligned pc");

        self.pc  = data;
        self.npc = self.pc +% 4;
    }
};

/// RegFile instance
var regs = RegFile{};

/// Are we in a branch delay slot?
var isBranchDelay = false;

pub var isRunning = true;
pub var isDisasm  = false;

/// Initializes the VR4300i module
pub fn init(isFastBoot: bool) void {
    if (isFastBoot) {
        info("[CPU] Fast boot.", .{});

        // Simulate PIF ROM
        regs.set64(@enumToInt(CPUReg.T3), 0xFFFFFFFF_A4000040);
        regs.set64(@enumToInt(CPUReg.S4), 0x00000000_00000001);
        regs.set64(@enumToInt(CPUReg.S6), 0x00000000_0000003F);
        regs.set64(@enumToInt(CPUReg.SP), 0xFFFFFFFF_A4001FF0);

        regs.setPC32(0xA4000040);
    } else {
        info("[CPU] PIF boot.", .{});

        regs.setPC32(0xBFC00000);
    }

    cop0.init();
}

// Instruction field decoders

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

/// Reads an 8-bit byte from memory
fn read8(addr: u64) u8 {
    // TODO: address translation

    return bus.read8(addr);
}

/// Reads a 16-bit halfword from memory
fn read16(addr: u64) u16 {
    // TODO: address translation

    return bus.read16(addr);
}

/// Reads a 32-bit word from memory
fn read32(addr: u64) u32 {
    // TODO: address translation

    return bus.read32(addr);
}

/// Reads a 64-bit word from memory
fn read64(addr: u64) u64 {
    // TODO: address translation


    return bus.read64(addr);
}

/// Writes an 8-bit byte to memory
fn store8(addr: u64, data: u8) void {
    // TODO: address translation

    bus.write8(addr, data);
}

/// Writes a 16-bit halfword to memory
fn store16(addr: u64, data: u16) void {
    // TODO: address translation

    bus.write16(addr, data);
}

/// Writes a 32-bit word to memory
fn store32(addr: u64, data: u32) void {
    // TODO: address translation

    bus.write32(addr, data);
}

/// Writes a 64-bit doubleword to memory
fn store64(addr: u64, data: u64) void {
    // TODO: address translation

    bus.write64(addr, data);
}

/// Reads an instruction, increments PC
fn fetchInstr() u32 {
    const data = read32(regs.pc);

    if (isDisasm) info("{X}h", .{regs.pc});

    regs.pc  = regs.npc;
    regs.npc +%= 4;

    isBranchDelay = false;

    return data;
}

const ExceptionCode = cop0.ExceptionCode;

pub fn raiseException(excCode: ExceptionCode) void {
    const vectorBase: u32 = 0x8000_0180;

    info("[CPU] {s} exception @ {X}h!", .{@tagName(excCode), regs.cpc});

    cop0.cause.excCode = @enumToInt(excCode);

    const epc = regs.cpc;

    if (!cop0.status.exl) {
        cop0.cause.bd = isBranchDelay;

        if (isBranchDelay) {
            cop0.epc = @truncate(u32, epc -% 4);
        } else {
            cop0.epc = @truncate(u32, epc);
        }

        info("EPC: {X}h, BD: {}", .{cop0.epc, isBranchDelay});
    }

    cop0.status.exl = true;

    info("Status: {X}h, Cause: {X}h", .{@bitCast(u32, cop0.status), @bitCast(u32, cop0.cause)});

    regs.setPC32(vectorBase);
}

fn checkCOPUsable(comptime copN: comptime_int) bool {
    if ((cop0.status.cu & (1 << copN)) == 0) {
        cop0.cause.ce = copN;

        raiseException(ExceptionCode.CoprocessorUnusable);

        return false;
    }

    return true;
}

/// Decodes and executes an instruction
fn decodeInstr(instr: u32) void {
    const opcode = instr >> 26;

    switch (opcode) {
        @enumToInt(Opcode.SPECIAL) => {
            const funct = instr & 0x3F;

            switch (funct) {
                @enumToInt(Special.SLL   ) => iSLL   (instr),
                @enumToInt(Special.SRL   ) => iSRL   (instr),
                @enumToInt(Special.SRA   ) => iSRA   (instr),
                @enumToInt(Special.SLLV  ) => iSLLV  (instr),
                @enumToInt(Special.SRLV  ) => iSRLV  (instr),
                @enumToInt(Special.SRAV  ) => iSRAV  (instr),
                @enumToInt(Special.JR    ) => iJR    (instr),
                @enumToInt(Special.JALR  ) => iJALR  (instr),
                @enumToInt(Special.MFHI  ) => iMFHI  (instr),
                @enumToInt(Special.MTHI  ) => iMTHI  (instr),
                @enumToInt(Special.MFLO  ) => iMFLO  (instr),
                @enumToInt(Special.MTLO  ) => iMTLO  (instr),
                @enumToInt(Special.MULT  ) => iMULT  (instr),
                @enumToInt(Special.MULTU ) => iMULTU (instr),
                @enumToInt(Special.DIV   ) => iDIV   (instr),
                @enumToInt(Special.DIVU  ) => iDIVU  (instr),
                @enumToInt(Special.DMULTU) => iDMULTU(instr),
                @enumToInt(Special.DDIVU ) => iDDIVU (instr),
                @enumToInt(Special.ADD   ) => iADD   (instr),
                @enumToInt(Special.ADDU  ) => iADDU  (instr),
                @enumToInt(Special.SUB   ) => iSUB   (instr),
                @enumToInt(Special.SUBU  ) => iSUBU  (instr),
                @enumToInt(Special.AND   ) => iAND   (instr),
                @enumToInt(Special.OR    ) => iOR    (instr),
                @enumToInt(Special.XOR   ) => iXOR   (instr),
                @enumToInt(Special.NOR   ) => iNOR   (instr),
                @enumToInt(Special.SLT   ) => iSLT   (instr),
                @enumToInt(Special.SLTU  ) => iSLTU  (instr),
                @enumToInt(Special.DADD  ) => iDADD  (instr),
                @enumToInt(Special.DADDU ) => iDADDU (instr),
                @enumToInt(Special.DSLL  ) => iDSLL  (instr),
                @enumToInt(Special.DSLL32) => iDSLL32(instr),
                @enumToInt(Special.DSRA32) => iDSRA32(instr),
                else => {
                    warn("[CPU] Unhandled function {X}h ({X}h).", .{funct, instr});

                    unreachable;
                }
            }
        },
        @enumToInt(Opcode.REGIMM) => {
            const regimm = getRt(instr);

            switch (regimm) {
                @enumToInt(Regimm.BLTZ  ) => iBLTZ  (instr),
                @enumToInt(Regimm.BGEZ  ) => iBGEZ  (instr),
                @enumToInt(Regimm.BLTZL ) => iBLTZL (instr),
                @enumToInt(Regimm.BGEZL ) => iBGEZL (instr),
                @enumToInt(Regimm.BGEZAL) => iBGEZAL(instr),
                else => {
                    warn("[CPU] Unhandled REGIMM opcode {X}h ({X}h).", .{regimm, instr});

                    unreachable;
                }
            }
        },
        @enumToInt(Opcode.J     ) => iJ     (instr),
        @enumToInt(Opcode.JAL   ) => iJAL   (instr),
        @enumToInt(Opcode.BEQ   ) => iBEQ   (instr),
        @enumToInt(Opcode.BNE   ) => iBNE   (instr),
        @enumToInt(Opcode.BLEZ  ) => iBLEZ  (instr),
        @enumToInt(Opcode.BGTZ  ) => iBGTZ  (instr),
        @enumToInt(Opcode.ADDI  ) => iADDI  (instr),
        @enumToInt(Opcode.ADDIU ) => iADDIU (instr),
        @enumToInt(Opcode.SLTI  ) => iSLTI  (instr),
        @enumToInt(Opcode.SLTIU ) => iSLTIU (instr),
        @enumToInt(Opcode.ANDI  ) => iANDI  (instr),
        @enumToInt(Opcode.ORI   ) => iORI   (instr),
        @enumToInt(Opcode.XORI  ) => iXORI  (instr),
        @enumToInt(Opcode.LUI   ) => iLUI   (instr),
        @enumToInt(Opcode.COP0  ) => {
            switch (getRs(instr)) {
                @enumToInt(COP.MF) => iMFC(instr, 0),
                @enumToInt(COP.MT) => iMTC(instr, 0),
                @enumToInt(COP.CO) => {
                    const funct = instr & 0x3F;

                    switch (funct) {
                        @enumToInt(CO.TLBR) => {
                            if (isDisasm) warn("[CPU] Unhandled TLBR instruction.", .{});
                        },
                        @enumToInt(CO.TLBWI) => {
                            if (isDisasm) warn("[CPU] Unhandled TLBWI instruction.", .{});
                        },
                        @enumToInt(CO.TLBP) => {
                            if (isDisasm) warn("[CPU] Unhandled TLBP instruction.", .{});
                        },
                        @enumToInt(CO.ERET ) => iERET(),
                        else => {
                            warn("[CPU] Unhandled CO function {X}h ({X}h).", .{funct, instr});

                            @panic("unhandled CO function");
                        }
                    }
                },
                else => {
                    warn("[CPU] Unhandled COP0 opcode {X}h ({X}h).", .{getRs(instr), instr});

                    unreachable;
                }
            }
        },
        @enumToInt(Opcode.COP1  ) => {
            if (!checkCOPUsable(1)) return;

            switch (getRs(instr)) {
                @enumToInt(COP.MF) => iMFC(instr, 1),
                @enumToInt(COP.CF) => iCFC(instr, 1),
                @enumToInt(COP.MT) => iMTC(instr, 1),
                @enumToInt(COP.CT) => iCTC(instr, 1),
                @enumToInt(COP.BC) => {
                    const funct = getRt(instr);

                    switch (funct) {
                        @enumToInt(BC.BCF ) => iBC(instr, false, false, 1),
                        @enumToInt(BC.BCT ) => iBC(instr, true , false, 1),
                        @enumToInt(BC.BCFL) => iBC(instr, false, true , 1),
                        @enumToInt(BC.BCTL) => iBC(instr, true , true , 1),
                        else => {
                            warn("[CPU] Unhandled BC1 opcode {X}h ({X}h).", .{funct, instr});

                            @panic("unhandled BC1 opcode");
                        }
                    }
                },
                @enumToInt(COP1.S) => {
                    const Fmt = cop1.Fmt;

                    const funct = instr & 0x3F;

                    switch (funct) {
                        @enumToInt(CO1.ADD    ) => cop1.fADD    (instr, Fmt.S),
                        @enumToInt(CO1.SUB    ) => cop1.fSUB    (instr, Fmt.S),
                        @enumToInt(CO1.MUL    ) => cop1.fMUL    (instr, Fmt.S),
                        @enumToInt(CO1.DIV    ) => cop1.fDIV    (instr, Fmt.S),
                        @enumToInt(CO1.SQRT   ) => cop1.fSQRT   (instr, Fmt.S),
                        @enumToInt(CO1.MOV    ) => cop1.fMOV    (instr, Fmt.S),
                        @enumToInt(CO1.TRUNC_W) => cop1.fTRUNC_W(instr, Fmt.S),
                        // @enumToInt(CO1.CVT_S  ) => cop1.fCVT_S   (instr, Fmt.S),
                        // @enumToInt(CO1.CVT_D  ) => cop1.fCVT_D   (instr, Fmt.S),
                        @enumToInt(CO1.CVT_W  ) => cop1.fCVT_W   (instr, Fmt.S),
                        @enumToInt(CO1.C) ... @enumToInt(CO1.C) + 0xF => {
                            cop1.fC(instr, @truncate(u4, instr), Fmt.S);
                        },
                        else => {
                            warn("[CPU] Unhandled CO1(S) function {X}h ({X}h).", .{funct, instr});

                            @panic("unhandled CO1(S) function");
                        }
                    }
                },
                @enumToInt(COP1.D) => {
                    const Fmt = cop1.Fmt;

                    const funct = instr & 0x3F;

                    switch (funct) {
                        @enumToInt(CO1.ADD    ) => cop1.fADD    (instr, Fmt.D),
                        @enumToInt(CO1.SUB    ) => cop1.fSUB    (instr, Fmt.D),
                        @enumToInt(CO1.MUL    ) => cop1.fMUL    (instr, Fmt.D),
                        @enumToInt(CO1.DIV    ) => cop1.fDIV    (instr, Fmt.D),
                        @enumToInt(CO1.MOV    ) => cop1.fMOV    (instr, Fmt.D),
                        @enumToInt(CO1.CVT_S  ) => cop1.fCVT_S  (instr, Fmt.D),
                        @enumToInt(CO1.TRUNC_W) => cop1.fTRUNC_W(instr, Fmt.D),
                        // @enumToInt(CO1.CVT_D) => cop1.fCVT_D(instr, Fmt.D),
                        @enumToInt(CO1.C) ... @enumToInt(CO1.C) + 0xF => {
                            cop1.fC(instr, @truncate(u4, instr), Fmt.D);
                        },
                        else => {
                            warn("[CPU] Unhandled CO1(D) function {X}h ({X}h).", .{funct, instr});

                            @panic("unhandled CO1(D) function");
                        }
                    }
                },
                @enumToInt(COP1.W) => {
                    const Fmt = cop1.Fmt;

                    const funct = instr & 0x3F;

                    switch (funct) {
                        @enumToInt(CO1.CVT_S) => cop1.fCVT_S(instr, Fmt.W),
                        @enumToInt(CO1.CVT_D) => cop1.fCVT_D(instr, Fmt.W),
                        else => {
                            warn("[CPU] Unhandled CO1(W) function {X}h ({X}h).", .{funct, instr});

                            @panic("unhandled CO1(W) function");
                        }
                    }
                },
                else => {
                    warn("[CPU] Unhandled COP1 opcode {X}h ({X}h).", .{getRs(instr), instr});

                    unreachable;
                }
            }
        },
        @enumToInt(Opcode.BEQL  ) => iBEQL  (instr),
        @enumToInt(Opcode.BNEL  ) => iBNEL  (instr),
        @enumToInt(Opcode.BLEZL ) => iBLEZL (instr),
        @enumToInt(Opcode.BGTZL ) => iBGTZL (instr),
        @enumToInt(Opcode.DADDI ) => iDADDI (instr),
        @enumToInt(Opcode.DADDIU) => iDADDIU(instr),
        @enumToInt(Opcode.LDL   ) => iLDL   (instr),
        @enumToInt(Opcode.LDR   ) => iLDR   (instr),
        @enumToInt(Opcode.LB    ) => iLB    (instr),
        @enumToInt(Opcode.LH    ) => iLH    (instr),
        @enumToInt(Opcode.LWL   ) => iLWL   (instr),
        @enumToInt(Opcode.LW    ) => iLW    (instr),
        @enumToInt(Opcode.LBU   ) => iLBU   (instr),
        @enumToInt(Opcode.LHU   ) => iLHU   (instr),
        @enumToInt(Opcode.LWR   ) => iLWR   (instr),
        @enumToInt(Opcode.LWU   ) => iLWU   (instr),
        @enumToInt(Opcode.SB    ) => iSB    (instr),
        @enumToInt(Opcode.SH    ) => iSH    (instr),
        @enumToInt(Opcode.SWL   ) => iSWL   (instr),
        @enumToInt(Opcode.SW    ) => iSW    (instr),
        @enumToInt(Opcode.SWR   ) => iSWR   (instr),
        @enumToInt(Opcode.CACHE ) => {
            //warn("[CPU] Unhandled CACHE instruction.", .{});
        },
        @enumToInt(Opcode.LWC1  ) => iLWC1  (instr),
        @enumToInt(Opcode.LDC1  ) => iLDC1  (instr),
        @enumToInt(Opcode.LD    ) => iLD    (instr),
        @enumToInt(Opcode.SWC1  ) => iSWC1  (instr),
        @enumToInt(Opcode.SDC1  ) => iSDC1  (instr),
        @enumToInt(Opcode.SD    ) => iSD    (instr),
        else => {
            warn("[CPU] Unhandled opcode {X}h ({X}h).", .{opcode, instr});

            unreachable;
        }
    }

    // if (regs.pc >= 0xFFFFFFFF_800012E0 and regs.pc < 0xFFFFFFFF_80800000) unreachable;
}

// Instruction helpers

fn doBranch(target: u64, isCondition: bool, isLink: comptime bool, isLikely: comptime bool) void {
    if (isLink) regs.set64(@enumToInt(CPUReg.RA), regs.npc);

    if (isCondition) {
        regs.npc = target;

        isBranchDelay = true;
    } else {
        if (isLikely) {
            regs.pc = regs.npc;
            regs.npc +%= 4;
        }
    }
}

// Instruction handlers

/// ADD - ADD
fn iADD(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    var result: i32 = undefined;

    if (@addWithOverflow(i32, @bitCast(i32, @truncate(u32, regs.get(rs))), @bitCast(i32, @truncate(u32, regs.get(rt))), &result)) {
        warn("[CPU] ADD overflow.", .{});

        unreachable;
    }

    regs.set32(rd, @bitCast(u32, result), true);

    if (isDisasm) info("[CPU] ADD ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// ADDI - ADD Immediate
fn iADDI(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    var result: i32 = undefined;

    if (@addWithOverflow(i32, @bitCast(i32, @truncate(u32, regs.get(rs))), @bitCast(i32, @truncate(u32, imm)), &result)) {
        warn("[CPU] ADDI overflow. rs: {X}h, imm: {X}h", .{@truncate(u32, regs.get(rs)), @truncate(u32, imm)});

        unreachable;
    }

    regs.set32(rt, @bitCast(u32, result), true);

    if (isDisasm) info("[CPU] ADDI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// ADDIU - ADD Immediate Unsigned
fn iADDIU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set32(rt, @truncate(u32, regs.get(rs) +% @truncate(u32, imm)), true);

    if (isDisasm) info("[CPU] ADDIU ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// ADDU - ADD Unsigned
fn iADDU(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, regs.get(rs) +% @truncate(u32, regs.get(rt))), true);

    if (isDisasm) info("[CPU] ADDU ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// AND - AND
fn iAND(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rd, regs.get(rs) & regs.get(rt));

    if (isDisasm) info("[CPU] AND ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// ANDI - AND Immediate
fn iANDI(instr: u32) void {
    const imm = @intCast(u64, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, regs.get(rs) & imm);

    if (isDisasm) info("[CPU] ANDI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// Branch On Coprocessor z
fn iBC(instr: u32, isTrue: comptime bool, isLikely: comptime bool, comptime copN: comptime_int) void {
    const offset = exts16(getImm16(instr)) << 2;

    const target = regs.pc +% offset;

    var coc: bool = undefined;

    if (copN == 1) {
        coc = cop1.coc1;
    } else {
        @panic("bc: unhandled copN");
    }

    if (isTrue) {
        if (isLikely) {
            doBranch(target, coc, false, true);

            if (isDisasm) info("[CPU] BC{}TL, {X}h", .{copN, target});
        } else {
            doBranch(target, coc, false, false);

            if (isDisasm) info("[CPU] BC{}T, {X}h", .{copN, target});
        }
    } else {
        if (isLikely) {
            doBranch(target, !coc, false, true);

            if (isDisasm) info("[CPU] BC{}FL, {X}h", .{copN, target});
        } else {
            doBranch(target, !coc, false, false);

            if (isDisasm) info("[CPU] BC{}F, {X}h", .{copN, target});
        }
    }
}

/// BEQ - Branch on EQual
fn iBEQ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = regs.pc +% offset;

    doBranch(target, regs.get(rs) == regs.get(rt), false, false);

    if (isDisasm) info("[CPU] BEQ ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, regs.get(rs), rt, regs.get(rt)});
}

/// BEQL - Branch on EQual Likely
fn iBEQL(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = regs.pc +% offset;

    doBranch(target, regs.get(rs) == regs.get(rt), false, true);

    if (isDisasm) info("[CPU] BEQL ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, regs.get(rs), rt, regs.get(rt)});
}

/// BGEZ - Branch on Greater than or Equal Zero
fn iBGEZ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = regs.pc +% offset;

    doBranch(target, @bitCast(i64, regs.get(rs)) >= 0, false, false);

    if (isDisasm) info("[CPU] BGEZ ${}, {X}h; ${} = {X}h", .{rs, target, rs, regs.get(rs)});
}

/// BGEZAL - Branch on Greater than or Equal Zero And Link
fn iBGEZAL(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = regs.pc +% offset;

    doBranch(target, @bitCast(i64, regs.get(rs)) >= 0, true, false);

    if (isDisasm) info("[CPU] BGEZAL ${}, {X}h; ${} = {X}h", .{rs, target, rs, regs.get(rs)});
}

/// BGEZL - Branch on Greater than or Equal Zero Likely
fn iBGEZL(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = regs.pc +% offset;

    doBranch(target, @bitCast(i64, regs.get(rs)) >= 0, false, true);

    if (isDisasm) info("[CPU] BGEZL ${}, {X}h; ${} = {X}h", .{rs, target, rs, regs.get(rs)});
}

/// BGTZ - Branch on Greater Than Zero
fn iBGTZ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = regs.pc +% offset;

    doBranch(target, @bitCast(i64, regs.get(rs)) > 0, false, false);

    if (isDisasm) info("[CPU] BGTZ ${}, {X}h; ${} = {X}h", .{rs, target, rs, regs.get(rs)});
}

/// BGTZL - Branch on Greater Than Zero Likely
fn iBGTZL(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = regs.pc +% offset;

    doBranch(target, @bitCast(i64, regs.get(rs)) > 0, false, true);

    if (isDisasm) info("[CPU] BGTZL ${}, {X}h; ${} = {X}h", .{rs, target, rs, regs.get(rs)});
}

/// BLEZ - Branch on Less than or Equal Zero
fn iBLEZ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = regs.pc +% offset;

    doBranch(target, @bitCast(i64, regs.get(rs)) <= 0, false, false);

    if (isDisasm) info("[CPU] BLEZ ${}, {X}h; ${} = {X}h", .{rs, target, rs, regs.get(rs)});
}

/// BLEZL - Branch on Less than or Equal Zero Likely
fn iBLEZL(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = regs.pc +% offset;

    doBranch(target, @bitCast(i64, regs.get(rs)) <= 0, false, true);

    if (isDisasm) info("[CPU] BLEZL ${}, {X}h; ${} = {X}h", .{rs, target, rs, regs.get(rs)});
}

/// BLTZ - Branch on Less Than Zero
fn iBLTZ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = regs.pc +% offset;

    doBranch(target, @bitCast(i64, regs.get(rs)) < 0, false, false);

    if (isDisasm) info("[CPU] BLTZ ${}, {X}h; ${} = {X}h", .{rs, target, rs, regs.get(rs)});
}

/// BLTZL - Branch on Less Than Zero Likely
fn iBLTZL(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);

    const target = regs.pc +% offset;

    doBranch(target, @bitCast(i64, regs.get(rs)) < 0, false, true);

    if (isDisasm) info("[CPU] BLTZL ${}, {X}h; ${} = {X}h", .{rs, target, rs, regs.get(rs)});
}

/// BNE - Branch on Not Equal
fn iBNE(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = regs.pc +% offset;

    doBranch(target, regs.get(rs) != regs.get(rt), false, false);

    if (isDisasm) info("[CPU] BNE ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, regs.get(rs), rt, regs.get(rt)});

    if (rs == 30 and rt == 0) {
        if (regs.get(rs) == 0) {
            if (isDisasm) info("[CPU] All tests passed!", .{});
        }
        else
        {
            if (isDisasm) info("[CPU] Failed test {}!", .{regs.get(rs)});
        }
    }
}

/// BNEL - Branch on Not Equal Likely
fn iBNEL(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = regs.pc +% offset;

    doBranch(target, regs.get(rs) != regs.get(rt), false, true);

    if (isDisasm) info("[CPU] BNEL ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, regs.get(rs), rt, regs.get(rt)});
}

/// CFC - Move From Control
fn iCFC(instr: u32, copN: comptime i32) void {
    const rd = getRd(instr);
    const rt = getRt(instr);

    var data: u32 = undefined;

    if (copN == 1) {
        data = cop1.getCtrl32(rd);
        if (isDisasm) info("[CPU] CFC1 ${}, ${}; ${} = {X}h", .{rt, rd, rt, data});
    }

    regs.set32(rt, data, true);
}

/// CTC - Move To Control
fn iCTC(instr: u32, copN: comptime i32) void {
    const rd = getRd(instr);
    const rt = getRt(instr);

    const data = @truncate(u32, regs.get(rt));

    if (copN == 1) {
        cop1.setCtrl32(rd, data);
        if (isDisasm) info("[CPU] CTC1 ${}, ${}; ${} = {X}h", .{rt, rd, rd, data});
    }
}

/// DADD - Doubleword ADD
fn iDADD(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    var result: i64 = undefined;

    if (@addWithOverflow(i64, @bitCast(i64, regs.get(rs)), @bitCast(i64, regs.get(rt)), &result)) {
        @panic("dadd: overflow");
    }

    regs.set64(rd, @bitCast(u64, result));

    if (isDisasm) info("[CPU] DADD ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// DADDI - Doubleword ADD Immediate
fn iDADDI(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    var result: i64 = undefined;

    if (@addWithOverflow(i64, @bitCast(i64, regs.get(rs)), @bitCast(i64, imm), &result)) {
        @panic("daddi: overflow");
    }

    regs.set64(rt, @bitCast(u64, result));

    if (isDisasm) info("[CPU] DADDI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// DADDIU - Doubleword ADD Immediate Unsigned
fn iDADDIU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, regs.get(rs) +% imm);

    if (isDisasm) info("[CPU] DADDIU ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// DADDU - Doubleword ADD Unsigned
fn iDADDU(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rd, regs.get(rs) +% regs.get(rt));

    if (isDisasm) info("[CPU] DADDU ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// DDIVU - Doubleword DIVide Unsigned
fn iDDIVU(instr: u32) void {
    const rs = getRs(instr);
    const rt = getRt(instr);

    const n = regs.get(rs);
    const d = regs.get(rt);

    if (d == 0) {
        warn("[CPU] DDIVU by zero.", .{});

        regs.lo = 0xFFFFFFFF_FFFFFFFF;
        regs.hi = n;
    } else {
        regs.lo = n / d;
        regs.hi = n % d;
    }

    if (isDisasm) info("[CPU] DDIVU ${}, ${}; HI = {X}h, LO = {X}h", .{rs, rt, regs.hi, regs.lo});
}

/// DIV - DIVide
fn iDIV(instr: u32) void {
    const rs = getRs(instr);
    const rt = getRt(instr);

    const n = @bitCast(i32, @truncate(u32, regs.get(rs)));
    const d = @bitCast(i32, @truncate(u32, regs.get(rt)));

    if (d == 0) {
        warn("[CPU] DIV by zero.", .{});

        if (n < 0) {
            regs.lo = 1;
        } else {
            regs.lo = 0xFFFFFFFF_FFFFFFFF;
        }

        regs.hi = exts32(@bitCast(u32, n));
    } else if (n == -0x80000000 and d == -1) {
        regs.lo = 0xFFFFFFFF_80000000;
        regs.hi = 0;
    } else {
        regs.lo = exts32(@bitCast(u32, @divFloor(n, d)));
        if (d < 0) {
            regs.hi = exts32(@bitCast(u32, @rem(n, -d)));
        } else {
            regs.hi = exts32(@bitCast(u32, n) % @bitCast(u32, d));
        }
    }

    if (isDisasm) info("[CPU] DIV ${}, ${}; HI = {X}h, LO = {X}h", .{rs, rt, regs.hi, regs.lo});
}

/// DIVU - DIVide Unsigned
fn iDIVU(instr: u32) void {
    const rs = getRs(instr);
    const rt = getRt(instr);

    const n = @truncate(u32, regs.get(rs));
    const d = @truncate(u32, regs.get(rt));

    if (d == 0) {
        warn("[CPU] DIVU by zero.", .{});

        regs.lo = 0xFFFFFFFF_FFFFFFFF;
        regs.hi = exts32(n);
    } else {
        regs.lo = exts32(n / d);
        regs.hi = exts32(n % d);
    }

    if (isDisasm) info("[CPU] DIVU ${}, ${}; HI = {X}h, LO = {X}h", .{rs, rt, regs.hi, regs.lo});
}

/// DMULTU - Doubleword MULTply Unsigned
fn iDMULTU(instr: u32) void {
    const rs = getRs(instr);
    const rt = getRt(instr);

    const result = @intCast(u128, regs.get(rs)) * @intCast(u128, regs.get(rt));

    regs.lo = @truncate(u64, result >>  0);
    regs.hi = @truncate(u64, result >> 64);

    if (isDisasm) info("[CPU] DMULTU ${}, ${}; HI = {X}h, LO = {X}h", .{rs, rt, regs.hi, regs.lo});
}

/// DSLL - Doubleword Shift Left Logical
fn iDSLL(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set64(rd, regs.get(rt) << @truncate(u6, sa));

    if (isDisasm) info("[CPU] DSLL ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// DSLL32 - Doubleword Shift Left Logical + 32
fn iDSLL32(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set64(rd, regs.get(rt) << @truncate(u6, sa + 32));

    if (isDisasm) info("[CPU] DSLL32 ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// DSRA32 - Doubleword Shift Right Arithmetic + 32
fn iDSRA32(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set64(rd, @bitCast(u64, @bitCast(i64, regs.get(rt)) >> @truncate(u6, sa + 32)));

    if (isDisasm) info("[CPU] DSRA32 ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// ERET - Exception RETurn
fn iERET() void {
    if (cop0.status.erl) {
        cop0.status.erl = false;

        regs.setPC32(cop0.errorEPC);
    } else {
        cop0.status.exl = false;

        regs.setPC32(cop0.epc);
    }

    // TODO: set LL to false

    info("ERET, PC: {X}h", .{regs.pc});

    if (isDisasm) info("[CPU] ERET", .{});
}

/// J - Jump
fn iJ(instr: u32) void {
    const target = (regs.pc & 0xFFFFFFFF_F0000000) | getTarget(instr);

    doBranch(target, true, false, false);

    if (isDisasm) info("[CPU] J {X}h", .{target});
}

/// JAL - Jump And Link
fn iJAL(instr: u32) void {
    const target = (regs.pc & 0xFFFFFFFF_F0000000) | getTarget(instr);

    doBranch(target, true, true, false);

    if (isDisasm) info("[CPU] JAL {X}h", .{target});
}

/// JALR - Jump And Link Register
fn iJALR(instr: u32) void {
    const rs = getRs(instr);

    const target = regs.get(rs);

    doBranch(target, true, true, false);

    if (isDisasm) info("[CPU] JALR ${}; PC = {X}h", .{rs, target});
}

/// JR - Jump Register
fn iJR(instr: u32) void {
    const rs = getRs(instr);

    const target = regs.get(rs);

    doBranch(target, true, false, false);

    if (isDisasm) info("[CPU] JR ${}; PC = {X}h", .{rs, target});
}

/// LB - Load Byte
fn iLB(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    regs.set64(rt, exts8(read8(addr)));

    if (isDisasm) info("[CPU] LB ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LBU - Load Byte Unsigned
fn iLBU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    regs.set64(rt, @intCast(u64, read8(addr)));

    if (isDisasm) info("[CPU] LBU ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LD - Load Doubleword
fn iLD(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    if ((addr & 7) != 0) @panic("unaligned load");

    regs.set64(rt, read64(addr));

    if (isDisasm) info("[CPU] LD ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LDC1 - Load Doubleword Coprocessor 1
fn iLDC1(instr: u32) void {
    if (!checkCOPUsable(1)) return;

    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    if ((addr & 7) != 0) @panic("unaligned load");

    cop1.setFGR64(rt, read64(addr));

    if (isDisasm) info("[CPU] LDC1 ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, cop1.getFGR64(rt)});
}

/// LDL - Load Doubleword Left
fn iLDL(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    const shift = @truncate(u6, 8 * (addr & 7));
    const mask  = @intCast(u64, 0xFFFFFFFF_FFFFFFFF) << shift;

    regs.set64(rt, (regs.get(rt) & ~mask) | (read64(addr & 0xFFFFFFF8) << shift));

    if (isDisasm) info("[CPU] LDL ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LDR - Load Doubleword Right
fn iLDR(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    const shift = @truncate(u6, 8 * ((addr ^ 7) & 7));
    const mask  = @intCast(u64, 0xFFFFFFFF_FFFFFFFF) >> shift;

    regs.set64(rt, (regs.get(rt) & ~mask) | (read64(addr & 0xFFFFFFF8) >> shift));

    if (isDisasm) info("[CPU] LDR ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LH - Load Halfword
fn iLH(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    if ((addr & 1) != 0) @panic("unaligned load");

    regs.set64(rt, exts16(read16(addr)));

    if (isDisasm) info("[CPU] LH ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LHU - Load Halfword Unsigned
fn iLHU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    if ((addr & 1) != 0) @panic("unaligned load");

    regs.set64(rt, @intCast(u64, read16(addr)));

    if (isDisasm) info("[CPU] LHU ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LUI - Load Upper Immediate
fn iLUI(instr: u32) void {
    const imm = getImm16(instr);

    const rt = getRt(instr);

    regs.set64(rt, exts16(imm) << 16);

    if (isDisasm) info("[CPU] LUI ${}, {X}h; ${} = {X}h", .{rt, imm, rt, regs.get(rt)});
}

/// LW - Load Word
fn iLW(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    if ((addr & 3) != 0) @panic("unaligned load");

    regs.set32(rt, read32(addr), true);

    if (isDisasm) info("[CPU] LW ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LWC1 - Load Word Coprocessor 1
fn iLWC1(instr: u32) void {
    if (!checkCOPUsable(1)) return;

    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    if ((addr & 3) != 0) @panic("unaligned load");

    cop1.setFGR32(rt, read32(addr));

    if (isDisasm) info("[CPU] LWC1 ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, cop1.getFGR32(rt)});
}

/// LWL - Load Word Left
fn iLWL(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    const shift = @truncate(u5, 8 * (addr & 3));
    const mask  = @intCast(u32, 0xFFFFFFFF) << shift;

    regs.set32(rt, (@truncate(u32, regs.get(rt)) & ~mask) | (read32(addr & 0xFFFFFFFC) << shift), true);

    if (isDisasm) info("[CPU] LWL ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LWR - Load Word Right
fn iLWR(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    const shift = @truncate(u5, 8 * ((addr ^ 3) & 3));
    const mask  = @intCast(u32, 0xFFFFFFFF) >> shift;

    regs.set32(rt, (@truncate(u32, regs.get(rt)) & ~mask) | (read32(addr & 0xFFFFFFFC) >> shift), true);

    if (isDisasm) info("[CPU] LWR ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LWU - Load Word Unsigned
fn iLWU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) + imm;

    if ((addr & 3) != 0) @panic("unaligned load");

    regs.set64(rt, @intCast(u64, read32(addr)));

    if (isDisasm) info("[CPU] LWU ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// MFC - Move From Coprocessor
fn iMFC(instr: u32, copN: comptime i32) void {
    const rd = getRd(instr);
    const rt = getRt(instr);

    const data = @truncate(u32, regs.get(rt));

    if (copN == 0) {
        regs.set32(rt, cop0.get32(rd), true);
    } else if (copN == 1) {
        regs.set32(rt, cop1.getFGR32(rd), true);
    }

    if (isDisasm) info("[CPU] MFC{} ${}, ${}; ${} = {X}h", .{copN, rt, rd, rd, data});
}

/// MFHI - Move From HI
fn iMFHI(instr: u32) void {
    const rd = getRd(instr);

    regs.set64(rd, regs.hi);

    if (isDisasm) info("[CPU] MFHI ${}; ${} = {X}h", .{rd, rd, regs.get(rd)});
}

/// MFLO - Move From LO
fn iMFLO(instr: u32) void {
    const rd = getRd(instr);

    regs.set64(rd, regs.lo);

    if (isDisasm) info("[CPU] MFLO ${}; ${} = {X}h", .{rd, rd, regs.get(rd)});
}

/// MTC - Move To Coprocessor
fn iMTC(instr: u32, copN: comptime i32) void {
    const rd = getRd(instr);
    const rt = getRt(instr);

    const data = @truncate(u32, regs.get(rt));

    if (copN == 0) {
        cop0.set32(rd, data);

        if (isDisasm) info("[CPU] MTC0 ${}, ${}; ${} = {X}h", .{rt, rd, rd, data});
    } else if (copN == 1) {
        if (!checkCOPUsable(1)) return;

        cop1.setFGR32(rd, data);

        if (isDisasm) info("[CPU] MTC1 ${}, ${}; ${} = {X}h", .{rt, rd, rd, data});
    }
}

/// MTHI - Move To HI
fn iMTHI(instr: u32) void {
    const rd = getRd(instr);

    regs.hi = regs.get(rd);

    if (isDisasm) info("[CPU] MTHI ${}; HI = {X}h", .{rd, regs.hi});
}

/// MTLO - Move To LO
fn iMTLO(instr: u32) void {
    const rd = getRd(instr);

    regs.lo = regs.get(rd);

    if (isDisasm) info("[CPU] MTLO ${}; LO = {X}h", .{rd, regs.lo});
}

/// MULT - MULTply
fn iMULT(instr: u32) void {
    const rs = getRs(instr);
    const rt = getRt(instr);

    const result = @intCast(i64, @bitCast(i32, @truncate(u32, regs.get(rs)))) * @intCast(i64, @bitCast(i32, @truncate(u32, regs.get(rt))));

    regs.lo = exts32(@truncate(u32, @bitCast(u64, result) >>  0));
    regs.hi = exts32(@truncate(u32, @bitCast(u64, result) >> 32));

    if (isDisasm) info("[CPU] MULT ${}, ${}; HI = {X}h, LO = {X}h", .{rs, rt, regs.hi, regs.lo});
}

/// MULTU - MULTply Unsigned
fn iMULTU(instr: u32) void {
    const rs = getRs(instr);
    const rt = getRt(instr);

    const result = @intCast(u64, @truncate(u32, regs.get(rs))) * @intCast(u64, @truncate(u32, regs.get(rt)));

    regs.lo = exts32(@truncate(u32, result >>  0));
    regs.hi = exts32(@truncate(u32, result >> 32));

    if (isDisasm) info("[CPU] MULTU ${}, ${}; HI = {X}h, LO = {X}h", .{rs, rt, regs.hi, regs.lo});
}

/// NOR - NOR
fn iNOR(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rd, ~(regs.get(rs) | regs.get(rt)));

    if (isDisasm) info("[CPU] NOR ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// OR - OR
fn iOR(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rd, regs.get(rs) | regs.get(rt));

    if (isDisasm) info("[CPU] OR ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// ORI - OR Immediate
fn iORI(instr: u32) void {
    const imm = @intCast(u64, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, regs.get(rs) | imm);

    if (isDisasm) info("[CPU] ORI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// SB - Store Byte
fn iSB(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    store8(addr, @truncate(u8, regs.get(rt)));

    if (isDisasm) info("[CPU] SB ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), addr, @truncate(u8, regs.get(rt))});
}

/// SD - Store Doubleword
fn iSD(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    store64(addr, regs.get(rt));

    if (isDisasm) info("[CPU] SD ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), addr, regs.get(rt)});
}
/// SDC1 - Store Doubleword Coprocessor 1
fn iSDC1(instr: u32) void {
    if (!checkCOPUsable(1)) return;

    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    store64(addr, cop1.getFGR64(rt));

    if (isDisasm) info("[CPU] SDC1 ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), addr, cop1.getFGR64(rt)});
}

/// SH - Store Halfword
fn iSH(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    store16(addr, @truncate(u16, regs.get(rt)));

    if (isDisasm) info("[CPU] SH ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), addr, @truncate(u16, regs.get(rt))});
}

/// SLL - Shift Left Logical
fn iSLL(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, regs.get(rt)) << @truncate(u5, sa), true);

    if (rd == @enumToInt(CPUReg.R0)) {
        if (isDisasm) info("[CPU] NOP", .{});
    } else {
        if (isDisasm) info("[CPU] SLL ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
    }
}

/// SLLV - Shift Right Logical Variable
fn iSLLV(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, regs.get(rt)) << @truncate(u5, regs.get(rs)), true);

    if (isDisasm) info("[CPU] SLLV ${}, ${}, ${}; ${} = {X}h", .{rd, rt, rs, rd, regs.get(rd)});
}

/// SLT - Set on Less Than
fn iSLT(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rd, @intCast(u64, @bitCast(u1, @bitCast(i64, regs.get(rs)) < @bitCast(i64, regs.get(rt)))));

    if (isDisasm) info("[CPU] SLT ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// SLTI - Set on Less Than Immediate
fn iSLTI(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, @intCast(u64, @bitCast(u1, @bitCast(i64, regs.get(rs)) < @bitCast(i64, imm))));

    if (isDisasm) info("[CPU] SLTI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// SLTIU - Set on Less Than Immediate
fn iSLTIU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, @intCast(u64, @bitCast(u1, regs.get(rs) < imm)));

    if (isDisasm) info("[CPU] SLTIU ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// SLTU - Set on Less Than Unsigned
fn iSLTU(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rd, @intCast(u64, @bitCast(u1, regs.get(rs) < regs.get(rt))));

    if (isDisasm) info("[CPU] SLTU ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// SRA - Shift Right Arithmetic
fn iSRA(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, @bitCast(u64, @bitCast(i64, regs.get(rt)) >> @truncate(u6, sa))), true);

    if (isDisasm) info("[CPU] SRA ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// SRAV - Shift Right Arithmetic Variable
fn iSRAV(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, @bitCast(u64, @bitCast(i64, regs.get(rt)) >> @truncate(u6, regs.get(rs)))), true);

    if (isDisasm) info("[CPU] SRAV ${}, ${}, ${}; ${} = {X}h", .{rd, rt, rs, rd, regs.get(rd)});
}

/// SRL - Shift Right Logical
fn iSRL(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, regs.get(rt)) >> @truncate(u5, sa), true);

    if (isDisasm) info("[CPU] SRL ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// SRLV - Shift Right Logical Variable
fn iSRLV(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, regs.get(rt)) >> @truncate(u5, regs.get(rs)), true);

    if (isDisasm) info("[CPU] SRLV ${}, ${}, ${}; ${} = {X}h", .{rd, rt, rs, rd, regs.get(rd)});
}

/// SUB - SUB
fn iSUB(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    var result: i32 = undefined;

    if (@subWithOverflow(i32, @bitCast(i32, @truncate(u32, regs.get(rs))), @bitCast(i32, @truncate(u32, regs.get(rt))), &result)) {
        warn("[CPU] SUB overflow.", .{});

        unreachable;
    }

    regs.set32(rd, @bitCast(u32, result), true);

    if (isDisasm) info("[CPU] SUB ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// SUBU - SUB Unsigned
fn iSUBU(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, regs.get(rs) -% @truncate(u32, regs.get(rt))), true);

    if (isDisasm) info("[CPU] SUBU ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// SW - Store Word
fn iSW(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    store32(addr, @truncate(u32, regs.get(rt)));

    if (isDisasm) info("[CPU] SW ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), addr, @truncate(u32, regs.get(rt))});
}
/// SWC1 - Store Word Coprocessor 1
fn iSWC1(instr: u32) void {
    if (!checkCOPUsable(1)) return;

    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    store32(addr, cop1.getFGR32(rt));

    if (isDisasm) info("[CPU] SWC1 ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), addr, cop1.getFGR64(rt)});
}

/// SWL - Store Word Left
fn iSWL(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    const shift = @truncate(u5, 8 * (addr & 3));
    const mask  = @intCast(u32, 0xFFFFFFFF) >> shift;

    const data = read32(addr & 0xFFFFFFFC);

    store32(addr & 0xFFFFFFFC, (data & ~mask) | (@truncate(u32, regs.get(rt)) >> shift));

    if (isDisasm) info("[CPU] SWL ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), addr, @truncate(u32, regs.get(rt))});
}

/// SWR - Store Word Right
fn iSWR(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) +% imm;

    const shift = @truncate(u5, 8 * ((addr ^ 3) & 3));
    const mask  = @intCast(u32, 0xFFFFFFFF) << shift;

    const data = read32(addr & 0xFFFFFFFC);

    store32(addr & 0xFFFFFFFC, (data & ~mask) | (@truncate(u32, regs.get(rt)) << shift));

    if (isDisasm) info("[CPU] SWR ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), addr, @truncate(u32, regs.get(rt))});
}

/// XOR - XOR
fn iXOR(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rd, regs.get(rs) ^ regs.get(rt));

    if (isDisasm) info("[CPU] XOR ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// XORI - XOR Immediate
fn iXORI(instr: u32) void {
    const imm = @intCast(u64, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, regs.get(rs) ^ imm);

    if (isDisasm) info("[CPU] XORI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// Steps the CPU module, returns elapsed cycles
pub fn step() i64 {
    regs.cpc = regs.pc;

    if(cop0.checkForInterrupts()) {
        cop0.tickCount(1);

        return 2;
    }

    const instr = fetchInstr();

    decodeInstr(instr);

    if (isDisasm) info("{X}h:{X}h", .{regs.cpc, instr});

    cop0.tickCount(1);

    return 2;
}
