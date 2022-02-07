//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! rdp.zig - Reality Display Processor module. 
//!

const std = @import("std");

const info = std.log.info;
const warn = std.log.warn;

const bus = @import("bus.zig");
const rsp = @import("rsp.zig");

const mi = @import("mi.zig");

const InterruptSource = mi.InterruptSource;

pub const RDPStatus = packed struct {
    x : bool = false,
    f : bool = false,
    fl: bool = false,
    g : bool = true,
    tb: bool = false,
    pb: bool = true,
    cb: bool = false,
    cr: bool = true,
    db: bool = false,
    ev: bool = false,
    sv: bool = false,
};

pub const RDPRegs = struct {
    rdpStatus: RDPStatus = RDPStatus{},

    rdpCMDStart: u24 = 0,
    rdpCMDEnd  : u24 = 0,
    rdpCMDCurr : u24 = 0,
};

const RDPCommand = enum(u64) {
    NoOperation     = 0x00,
    SyncLoad        = 0x26,
    SyncPipe        = 0x27,
    SyncTile        = 0x28,
    SyncFull        = 0x29,
    SetScissor      = 0x2D,
    SetOtherModes   = 0x2F,
    SetTileSize     = 0x32,
    LoadBlock       = 0x33,
    SetTile         = 0x35,
    FillRectangle   = 0x36,
    SetFillColor    = 0x37,
    SetEnvColor     = 0x3B,
    SetCombine      = 0x3C,
    SetTextureImage = 0x3D,
    SetMaskImage    = 0x3E,
    SetColorImage   = 0x3F,
};

pub var rdpRegs: RDPRegs = RDPRegs{};

fn getCommandWord() u64 {
    var data: u64 = undefined;

    if (rdpRegs.rdpStatus.x) {
        @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &rsp.spDMEM[@truncate(u12, rdpRegs.rdpCMDCurr)]), 8);
    } else {
        @memcpy(@ptrCast([*]u8, &data), @ptrCast([*]u8, &bus.ram[rdpRegs.rdpCMDCurr]), 8);
    }

    rdpRegs.rdpCMDCurr +%= 8;

    return @byteSwap(u64, data);
}

fn getCommand(cmdWord: u64) u64 {
    return (cmdWord >> 56) & 0x3F;
}

pub fn processDP() void {
    while (rdpRegs.rdpCMDCurr < rdpRegs.rdpCMDEnd) {
        const cmdWord = getCommandWord();

        switch (getCommand(cmdWord)) {
            @enumToInt(RDPCommand.NoOperation) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.NoOperation), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SyncLoad) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SyncLoad), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SyncPipe) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SyncPipe), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SyncTile) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SyncTile), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SyncFull) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SyncFull), cmdWord, rdpRegs.rdpCMDCurr -% 8});

                mi.setPending(InterruptSource.DP);
            },
            @enumToInt(RDPCommand.SetScissor) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetScissor), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SetOtherModes) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetOtherModes), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SetTileSize) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetTileSize), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.LoadBlock) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.LoadBlock), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SetTile) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetTile), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.FillRectangle) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.FillRectangle), cmdWord, rdpRegs.rdpCMDCurr -% 8});

                const yh = (cmdWord >>  0) & 0xFFF;
                const xh = (cmdWord >> 12) & 0xFFF;
                const yl = (cmdWord >> 32) & 0xFFF;
                const xl = (cmdWord >> 48) & 0xFFF;

                info("[RDP] YH: {}, XH: {}, YL: {}, XL: {}", .{yh, xh, yl, xl});
            },
            @enumToInt(RDPCommand.SetFillColor) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetFillColor), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SetEnvColor) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetEnvColor), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SetCombine) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetCombine), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SetTextureImage) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetTextureImage), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SetMaskImage) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetMaskImage), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            @enumToInt(RDPCommand.SetColorImage) => {
                info("[RDP] {s} ({X}h) @ {X}h", .{@tagName(RDPCommand.SetColorImage), cmdWord, rdpRegs.rdpCMDCurr -% 8});
            },
            else => {
                warn("[RDP] Unhandled command {X}h ({X}h) @ {X}h.", .{getCommand(cmdWord), cmdWord, rdpRegs.rdpCMDCurr -% 8});

                @panic("unhandled RDP command");
            }
        }
    }

    rdpRegs.rdpStatus.ev = false;
    rdpRegs.rdpStatus.sv = false;
}
