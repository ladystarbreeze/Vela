//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! si.zig - Serial Interface module. 
//!

const std = @import("std");

const info = std.log.info;
const warn = std.log.warn;

const bus = @import("bus.zig");
const pif = @import("pif.zig");

const mi = @import("mi.zig");

const InterruptSource = mi.InterruptSource;

const SIReg = enum(u64) {
    SIDRAMAddr    = 0x00,
    SIPIFAddrRD64 = 0x04,
    SIPIFAddrWR4  = 0x08,
    SIPIFAddrWR64 = 0x10,
    SIPIFAddrRD4  = 0x14,
    SIStatus      = 0x18,
};

const SIStatus = packed struct {
    dmaBusy    : bool = false,
    ioBusy     : bool = false,
    readPending: bool = false,
    dmaError   : bool = false,
    pchState   : u3   = 0,
    dmaState   : u3   = 0,
    dmaIRQ     : bool = false,
    _pad0      : u21  = 0,
};

const SIRegs = struct {
    siDRAMAddr: u24 = 0,
    siPIFAddr : u32 = 0,
    siStatus  : SIStatus = SIStatus{},
};

var siRegs = SIRegs{};

pub fn read32(pAddr: u64) u32 {
    var data: u32 = undefined;

    switch (pAddr & 0xFF) {
        @enumToInt(SIReg.SIStatus) => {
            info("[SI] Read32 @ pAddr: {X}h (SI Status).", .{pAddr});

            data = @bitCast(u32, siRegs.siStatus);
        },
        else => {
            warn("[SI] Unhandled read32 @ pAddr: {X}h.", .{pAddr});

            @panic("unhandled SI read");
        }
    }

    return data;
}

pub fn write32(pAddr: u64, data: u32) void {
    switch (pAddr & 0xFF) {
        @enumToInt(SIReg.SIDRAMAddr) => {
            info("[SI] Write32 @ pAddr: {X}h (SI DRAM Address), data: {X}h.", .{pAddr, data});

            siRegs.siDRAMAddr = @truncate(u24, data);
        },
        @enumToInt(SIReg.SIPIFAddrRD64) => {
            info("[SI] Write32 @ pAddr: {X}h (SI PIF Address RD64), data: {X}h.", .{pAddr, data});

            siRegs.siPIFAddr = data;

            doDMAToRAM(64);
        },
        @enumToInt(SIReg.SIPIFAddrWR64) => {
            info("[SI] Write32 @ pAddr: {X}h (SI PIF Address WR64), data: {X}h.", .{pAddr, data});

            siRegs.siPIFAddr = data;

            doDMAToPIF(64);
        },
        @enumToInt(SIReg.SIStatus) => {
            info("[SI] Write32 @ pAddr: {X}h (SI Status), data: {X}h.", .{pAddr, data});

            siRegs.siStatus.dmaIRQ = false;

            mi.clearPending(InterruptSource.SI);
        },
        else => {
            warn("[SI] Unhandled write32 @ pAddr: {X}h, data: {X}h.", .{pAddr, data});

            @panic("unhandled SI write");
        }
    }
}

fn doDMAToPIF(comptime len: comptime_int) void {
    const ramAddr = @intCast(usize, siRegs.siDRAMAddr);
    const pifAddr = @intCast(usize, siRegs.siPIFAddr);

    info("[SI] RAM->PIF DMA, DRAM pAddr: {X}h, PIF pAddr: {X}h, length: {}.", .{ramAddr, pifAddr, len});

    var idx: usize = 0;
    while (idx < len) : (idx += 1) {
        pif.pifRAM[(pifAddr + idx) - pif.pifRAMBase] = bus.ram[ramAddr + idx];
    }

    siRegs.siStatus.dmaIRQ = true;

    mi.setPending(InterruptSource.SI);
}

fn doDMAToRAM(comptime len: comptime_int) void {
    const ramAddr = @intCast(usize, siRegs.siDRAMAddr);
    const pifAddr = @intCast(usize, siRegs.siPIFAddr);

    info("[SI] PIF->RAM DMA, DRAM pAddr: {X}h, PIF pAddr: {X}h, length: {}.", .{ramAddr, pifAddr, len});

    pif.checkStatus();

    var idx: usize = 0;
    while (idx < len) : (idx += 1) {
        bus.ram[ramAddr + idx] = pif.pifRAM[(pifAddr + idx) - pif.pifRAMBase];
    }

    siRegs.siStatus.dmaIRQ = true;

    mi.setPending(InterruptSource.SI);
}
