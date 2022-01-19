//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! cpu.zig - VR4300i CPU interpreter module. 
//!

const std = @import("std");

const info = std.log.info;
const warn = std.log.warn;

const bus = @import("bus.zig");

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

    J      = 0x02,
    JAL    = 0x03,
    BEQ    = 0x04,
    BNE    = 0x05,
    ADDI   = 0x08,
    ADDIU  = 0x09,
    SLTI   = 0x0A,
    SLTIU  = 0x0B,
    ANDI   = 0x0C,
    ORI    = 0x0D,
    XORI   = 0x0E,
    LUI    = 0x0F,
    BEQL   = 0x14,
    BNEL   = 0x15,
    DADDI  = 0x18,
    DADDIU = 0x19,
    LH     = 0x21,
    LW     = 0x23,
    LBU    = 0x24,
    LHU    = 0x25,
    LWU    = 0x27,
    SW     = 0x2B,
    LD     = 0x37,
};

/// SPECIAL field
const Special = enum(u32) {
    SLL    = 0x00,
    SRL    = 0x02,
    SRA    = 0x03,
    JR     = 0x08,
    JALR   = 0x09,
    ADD    = 0x20,
    ADDU   = 0x21,
    SLT    = 0x2A,
    DSLL   = 0x38,
    DSLL32 = 0x3C,
    DSRA32 = 0x3F,
};

/// VR4300i register file
const RegFile = struct {
    gprs: [32]u64 = undefined,

    pc : u64 = undefined,
    npc: u64 = undefined,

    /// Reads GPR (64-bit)
    pub fn get(self: RegFile, idx: u32) u64 {
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
    }

    /// Sets PC (32-bit), sign extends it
    pub fn setPC32(self: *RegFile, data: u32) void {
        self.pc  = exts32(data);
        self.npc = self.pc +% 4;
    }

    /// Sets GPR (64-bit)
    pub fn set64(self: *RegFile, idx: u32, data: u64) void {
        self.gprs[idx] = data;

        self.gprs[@enumToInt(CPUReg.R0)] = 0;
    }

    /// Sets PC (64-bit)
    pub fn setPC64(self: *RegFile, data: u64) void {
        self.pc  = data;
        self.npc = self.pc +% 4;
    }
};

/// RegFile instance
var regs = RegFile{};

/// Are we in a branch delay slot?
var isBranchDelay = false;

pub var isRunning = true;

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

/// Writes a 32-bit word to memory
fn store32(addr: u64, data: u32) void {
    // TODO: address translation

    bus.write32(addr, data);
}

/// Reads an instruction, increments PC
fn fetchInstr() u32 {
    const data = read32(regs.pc);

    // info("{X}h", .{regs.pc});

    regs.pc  = regs.npc;
    regs.npc +%= 4;

    isBranchDelay = false;

    return data;
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
                @enumToInt(Special.JR    ) => iJR    (instr),
                @enumToInt(Special.JALR  ) => iJALR  (instr),
                @enumToInt(Special.ADD   ) => iADD   (instr),
                @enumToInt(Special.ADDU  ) => iADDU  (instr),
                @enumToInt(Special.SLT   ) => iSLT   (instr),
                @enumToInt(Special.DSLL  ) => iDSLL  (instr),
                @enumToInt(Special.DSLL32) => iDSLL32(instr),
                @enumToInt(Special.DSRA32) => iDSRA32(instr),
                else => {
                    warn("[CPU] Unhandled function {X}h ({X}h).", .{funct, instr});

                    unreachable;
                }
            }
        },
        @enumToInt(Opcode.J     ) => iJ     (instr),
        @enumToInt(Opcode.JAL   ) => iJAL   (instr),
        @enumToInt(Opcode.BEQ   ) => iBEQ   (instr),
        @enumToInt(Opcode.BNE   ) => iBNE   (instr),
        @enumToInt(Opcode.ADDI  ) => iADDI  (instr),
        @enumToInt(Opcode.ADDIU ) => iADDIU (instr),
        @enumToInt(Opcode.SLTI  ) => iSLTI  (instr),
        @enumToInt(Opcode.SLTIU ) => iSLTIU (instr),
        @enumToInt(Opcode.ANDI  ) => iANDI  (instr),
        @enumToInt(Opcode.ORI   ) => iORI   (instr),
        @enumToInt(Opcode.XORI  ) => iXORI  (instr),
        @enumToInt(Opcode.LUI   ) => iLUI   (instr),
        @enumToInt(Opcode.BEQL  ) => iBEQL  (instr),
        @enumToInt(Opcode.BNEL  ) => iBNEL  (instr),
        @enumToInt(Opcode.DADDI ) => iDADDI (instr),
        @enumToInt(Opcode.DADDIU) => iDADDIU(instr),
        // @enumToInt(Opcode.LH   ) => iLH   (instr),
        @enumToInt(Opcode.LW    ) => iLW    (instr),
        @enumToInt(Opcode.LBU   ) => iLBU   (instr),
        @enumToInt(Opcode.LHU   ) => iLHU   (instr),
        @enumToInt(Opcode.LWU   ) => iLWU   (instr),
        @enumToInt(Opcode.SW    ) => iSW    (instr),
        @enumToInt(Opcode.LD    ) => iLD    (instr),
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

    var result: u32 = undefined;

    if (@addWithOverflow(u32, @truncate(u32, regs.get(rs)), @truncate(u32, regs.get(rt)), &result)) {
        warn("[CPU] ADD overflow.", .{});

        unreachable;
    }

    regs.set32(rd, result, true);

    info("[CPU] ADD ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// ADDI - ADD Immediate
fn iADDI(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    var result: u32 = undefined;

    if (@addWithOverflow(u32, @truncate(u32, regs.get(rs)), @truncate(u32, imm), &result)) {
        warn("[CPU] ADDI overflow.", .{});

        unreachable;
    }

    regs.set32(rt, result, true);

    info("[CPU] ADDI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// ADDIU - ADD Immediate Unsigned
fn iADDIU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set32(rt, @truncate(u32, regs.get(rs) +% @truncate(u32, imm)), true);

    info("[CPU] ADDIU ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// ADDU - ADD Unsigned
fn iADDU(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, regs.get(rs) +% @truncate(u32, regs.get(rt))), true);

    info("[CPU] ADDU ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// ANDI - AND Immediate
fn iANDI(instr: u32) void {
    const imm = @intCast(u64, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, regs.get(rs) & imm);

    info("[CPU] ANDI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// BEQ - Branch on EQual
fn iBEQ(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = regs.pc +% offset;

    doBranch(target, regs.get(rs) == regs.get(rt), false, false);

    info("[CPU] BEQ ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, regs.get(rs), rt, regs.get(rt)});
}

/// BEQL - Branch on EQual Likely
fn iBEQL(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = regs.pc +% offset;

    doBranch(target, regs.get(rs) == regs.get(rt), false, true);

    info("[CPU] BEQL ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, regs.get(rs), rt, regs.get(rt)});
}

/// BNE - Branch on Not Equal
fn iBNE(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = regs.pc +% offset;

    doBranch(target, regs.get(rs) != regs.get(rt), false, false);

    info("[CPU] BNE ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, regs.get(rs), rt, regs.get(rt)});

    if (rs == 30 and rt == 0) {
        if (regs.get(rs) == 0) {
            info("[CPU] All tests passed!", .{});
        }
        else
        {
            info("[CPU] Failed test {}!", .{regs.get(rs)});
        }
        
        unreachable;
    }
}

/// BNEL - Branch on Not Equal Likely
fn iBNEL(instr: u32) void {
    const offset = exts16(getImm16(instr)) << 2;

    const rs = getRs(instr);
    const rt = getRt(instr);

    const target = regs.pc +% offset;

    doBranch(target, regs.get(rs) != regs.get(rt), false, true);

    info("[CPU] BNEL ${}, ${}, {X}h; ${} = {X}h, ${} = {X}h", .{rs, rt, target, rs, regs.get(rs), rt, regs.get(rt)});
}

/// DADDI - Doubleword ADD Immediate
fn iDADDI(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    var result: u64 = undefined;

    if (@addWithOverflow(u64, regs.get(rs), imm, &result)) {
        warn("[CPU] DADDI overflow.", .{});

        unreachable;
    }

    regs.set64(rt, result);

    info("[CPU] DADDI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// DADDIU - Doubleword ADD Immediate Unsigned
fn iDADDIU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, regs.get(rs) +% imm);

    info("[CPU] DADDIU ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// DSLL - Doubleword Shift Left Logical
fn iDSLL(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set64(rd, regs.get(rt) << @truncate(u6, sa));

    info("[CPU] DSLL ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// DSLL32 - Doubleword Shift Left Logical + 32
fn iDSLL32(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set64(rd, regs.get(rt) << @truncate(u6, sa + 32));

    info("[CPU] DSLL32 ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// DSRA32 - Doubleword Shift Right Arithmetic + 32
fn iDSRA32(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set64(rd, @bitCast(u64, @bitCast(i64, regs.get(rt)) >> @truncate(u6, sa + 32)));

    info("[CPU] DSRA32 ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// J - Jump
fn iJ(instr: u32) void {
    const target = (regs.pc & 0xFFFFFFFF_F0000000) | getTarget(instr);

    doBranch(target, true, false, false);

    info("[CPU] J {X}h", .{target});
}

/// JAL - Jump And Link
fn iJAL(instr: u32) void {
    const target = (regs.pc & 0xFFFFFFFF_F0000000) | getTarget(instr);

    doBranch(target, true, true, false);

    info("[CPU] JAL {X}h", .{target});
}

/// JALR - Jump And Link Register
fn iJALR(instr: u32) void {
    const rs = getRs(instr);

    const target = regs.get(rs);

    doBranch(target, true, true, false);

    info("[CPU] JALR ${}; PC = {X}h", .{rs, target});
}

/// JR - Jump Register
fn iJR(instr: u32) void {
    const rs = getRs(instr);

    const target = regs.get(rs);

    doBranch(target, true, false, false);

    info("[CPU] JR ${}; PC = {X}h", .{rs, target});
}

/// LBU - Load Byte Unsigned
fn iLBU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) + imm;

    regs.set64(rt, @intCast(u64, read8(addr)));

    info("[CPU] LWU ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LD - Load Doubleword
fn iLD(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) + imm;

    regs.set64(rt, read64(addr));

    info("[CPU] LD ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LH - Load Halfword
fn iLH(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) + imm;

    regs.set64(rt, exts16(read16(addr)));

    info("[CPU] LH ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LHU - Load Halfword Unsigned
fn iLHU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) + imm;

    regs.set64(rt, @intCast(u64, read16(addr)));

    info("[CPU] LHU ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LUI - Load Upper Immediate
fn iLUI(instr: u32) void {
    const imm = getImm16(instr);

    const rt = getRt(instr);

    regs.set64(rt, exts16(imm) << 16);

    info("[CPU] LUI ${}, {X}h; ${} = {X}h", .{rt, imm, rt, regs.get(rt)});
}

/// LW - Load Word
fn iLW(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) + imm;

    regs.set32(rt, read32(addr), true);

    info("[CPU] LW ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}

/// LWU - Load Word Unsigned
fn iLWU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) + imm;

    regs.set64(rt, @intCast(u64, read32(addr)));

    info("[CPU] LWU ${}, ${}({}); ${} = ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), rt, addr, regs.get(rt)});
}


/// ORI - OR Immediate
fn iORI(instr: u32) void {
    const imm = @intCast(u64, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, regs.get(rs) | imm);

    info("[CPU] ORI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// SLL - Shift Left Logical
fn iSLL(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, regs.get(rt)) << @truncate(u5, sa), true);

    if (rd == @enumToInt(CPUReg.R0)) {
        info("[CPU] NOP", .{});
    } else {
        info("[CPU] SLL ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
    }
}

/// SLT - Set on Less Than
fn iSLT(instr: u32) void {
    const rd = getRd(instr);
    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rd, @intCast(u64, @bitCast(u1, @bitCast(i64, regs.get(rs)) < @bitCast(i64, regs.get(rt)))));

    info("[CPU] SLT ${}, ${}, ${}; ${} = {X}h", .{rd, rs, rt, rd, regs.get(rd)});
}

/// SLTI - Set on Less Than Immediate
fn iSLTI(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, @intCast(u64, @bitCast(u1, @bitCast(i64, regs.get(rs)) < @bitCast(i64, imm))));

    info("[CPU] SLTI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// SLTIU - Set on Less Than Immediate
fn iSLTIU(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, @intCast(u64, @bitCast(u1, regs.get(rs) < imm)));

    info("[CPU] SLTIU ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// SRA - Shift Right Arithmetic
fn iSRA(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, @bitCast(u64, @bitCast(i64, regs.get(rt)) >> @truncate(u6, sa))), true);

    info("[CPU] SRA ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// SRL - Shift Right Logical
fn iSRL(instr: u32) void {
    const sa = getSa(instr);

    const rd = getRd(instr);
    const rt = getRt(instr);

    regs.set32(rd, @truncate(u32, regs.get(rt)) >> @truncate(u5, sa), true);

    info("[CPU] SRL ${}, ${}, {}; ${} = {X}h", .{rd, rt, sa, rd, regs.get(rd)});
}

/// SW - Store Word
fn iSW(instr: u32) void {
    const imm = exts16(getImm16(instr));

    const base = getRs(instr);
    const rt   = getRt(instr);

    const addr = regs.get(base) + imm;

    store32(addr, @truncate(u32, regs.get(rt)));

    info("[CPU] SW ${}, ${}({}); ({X}h) = {X}h", .{rt, base, @bitCast(i64, imm), addr, @truncate(u32, regs.get(rt))});
}

/// XORI - XOR Immediate
fn iXORI(instr: u32) void {
    const imm = @intCast(u64, getImm16(instr));

    const rs = getRs(instr);
    const rt = getRt(instr);

    regs.set64(rt, regs.get(rs) ^ imm);

    info("[CPU] XORI ${}, ${}, {X}h; ${} = {X}h", .{rt, rs, imm, rt, regs.get(rt)});
}

/// Steps the CPU module, returns elapsed cycles
pub fn step() i32 {
    const instr = fetchInstr();

    decodeInstr(instr);

    if (regs.pc == 0xFFFFFFFF_800012E0) {
        isRunning = false;
    }

    return 1;
}
