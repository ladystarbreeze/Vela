//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! ai.zig - Audio Interface module. 
//!

const std = @import("std");

const info = std.log.info;
const warn = std.log.warn;

const bus = @import("bus.zig");

const mi = @import("mi.zig");

const InterruptSource = mi.InterruptSource;

const AIReg = enum(u64) {
    AIDRAMAddr = 0x00,
    AILen      = 0x04,
    AIControl  = 0x08,
    AIStatus   = 0x0C,
    AIDACRate  = 0x10,
    AIBitRate  = 0x14,
};

const AIStatus = packed struct {
    aiFull0: bool = false,
    _pad0  : u15  = 0,
    _pad1  : u14  = 0,
    aiBusy : bool = false,
    aiFull1: bool = false,
};

const AIRegs = struct {
    aiDRAMAddr: [2]u24 = undefined,
    aiDMALen  : [2]u18 = undefined,

    aiStatus: AIStatus = AIStatus{},

    aiActiveDMAs: u32 = 0,
};

var aiRegs: AIRegs = AIRegs{};

pub fn read32(pAddr: u64) u32 {
    var data: u32 = undefined;

    switch (pAddr & 0xFF) {
        @enumToInt(AIReg.AILen) => {
            info("[AI] Read32 @ pAddr {X}h (AI Length).", .{pAddr});

            data = aiRegs.aiDMALen[0];
        },
        @enumToInt(AIReg.AIStatus) => {
            info("[AI] Read32 @ pAddr {X}h (AI Status).", .{pAddr});

            data = @bitCast(u32, aiRegs.aiStatus) | (1 << 24) | (1 << 20);
        },
        else => {
            warn("[AI] Unhandled read32 @ pAddr {X}h.", .{pAddr});

            @panic("unhandled AI read");
        }
    }

    return data;
}

pub fn write32(pAddr: u64, data: u32) void {
    switch (pAddr & 0xFF) {
        @enumToInt(AIReg.AIDRAMAddr) => {
            info("[AI] Write32 @ pAddr {X}h (AI DRAM Address), data: {X}h.", .{pAddr, data});

            if (aiRegs.aiActiveDMAs < 2) {
                aiRegs.aiDRAMAddr[aiRegs.aiActiveDMAs] = @truncate(u24, data & 0xFF_FFF8);
            }
        },
        @enumToInt(AIReg.AILen) => {
            info("[AI] Write32 @ pAddr {X}h (AI Length), data: {X}h.", .{pAddr, data});

            if (aiRegs.aiActiveDMAs < 2 and data != 0) {
                aiRegs.aiDMALen[aiRegs.aiActiveDMAs] = @truncate(u18, data & 0x3FFF8);

                aiRegs.aiActiveDMAs += 1;
            }
        },
        @enumToInt(AIReg.AIStatus) => {
            warn("[AI] Write32 @ pAddr {X}h (AI Status), data: {X}h.", .{pAddr, data});

            mi.clearPending(InterruptSource.AI);
        },
        else => {
            warn("[AI] Unhandled write32 @ pAddr {X}h, data: {X}h.", .{pAddr, data});

            @panic("unhandled AI write");
        }
    }
}

pub fn step(c: i64) void {
}
