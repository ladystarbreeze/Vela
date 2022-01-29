//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! pif.zig - Peripheral InterFace module. 
//!

const std = @import("std");

const info = std.log.info;
const warn = std.log.warn;

const n64 = @import("n64.zig");

const Controller = n64.Controller;

const PIFCommand = enum(u8) {
    GetStatus    = 0x00,
    GetButtons   = 0x01,
    WriteMemcard = 0x03,
};

// PIF constants
pub const pifRAMBase = 0x1FC0_07C0;
const pifStatus = 0x3F;

pub var pifRAM: [0x40]u8 = undefined;

pub fn checkStatus() void {
    if ((pifRAM[pifStatus] & 1) != 0) {
        info("[PIF] Scanning PIF RAM...", .{});

        const hexB: []const u8 = &pifRAM;
        info("[PIF] Before: {}h", .{std.fmt.fmtSliceHexUpper(hexB)});

        var channel: i32 = 0;

        var idx: usize = 0;
        pifLoop: while (idx <= pifStatus) {
            const t = pifRAM[idx];

            info("[PIF] t: {X}h", .{t});

            if (t == 0) {
                channel += 1;

                idx += 1;
            } else if (t < 0x80) {
                const r = pifRAM[idx + 1] & 0x3F;

                const cmd = pifRAM[idx + 2];

                switch (cmd) {
                    @enumToInt(PIFCommand.GetStatus) => {
                        info("[PIF] Get Controller Status.", .{});

                        if (channel == 0) {
                            pifRAM[idx + 3 + 0] = 0x05;
                            pifRAM[idx + 3 + 1] = 0x00;
                            pifRAM[idx + 3 + 2] = 0x00;
                        } else {
                            // Write Device Not present byte
                            pifRAM[idx + 1] |= 0x80;
                        }
                    },
                    @enumToInt(PIFCommand.GetButtons) => {
                        info("[PIF] Get Buttons.", .{});

                        if (channel == 0) {
                            pifRAM[idx + 3 + 0] = @truncate(u8, @bitCast(u32, n64.controller));
                            pifRAM[idx + 3 + 1] = @truncate(u8, @bitCast(u32, n64.controller) >> 8);
                            pifRAM[idx + 3 + 2] = n64.controller.x;
                            pifRAM[idx + 3 + 3] = n64.controller.y;
                        } else {
                            // Write Device Not present byte
                            pifRAM[idx + 1] |= 0x80;
                        }
                    },
                    @enumToInt(PIFCommand.WriteMemcard) => {
                        warn("[PIF] Unhandled command Write To Memcard.", .{});
                    },
                    else => {
                        warn("[PIF] Unhandled command {X}h.", .{cmd});

                        @panic("unhandled PIF command");
                    }
                }

                channel += 1;

                idx += 2 + t + r;
            } else if (t == 0xFE) {
                break :pifLoop;
            } else {
                idx += 1;
            }
        }

        // pifRAM[pifStatus] = 0;

        const hexA: []const u8 = &pifRAM;
        info("[PIF] After: {}h", .{std.fmt.fmtSliceHexUpper(hexB)});
    }
}
