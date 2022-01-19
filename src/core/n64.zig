//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! n64.zig - Main emulator module. 
//!

const std = @import("std");

const bus = @import("bus.zig");
const cpu = @import("cpu.zig");

const isFastBoot = true;

/// Initializes all submodules
pub fn run(romPath: []const u8) anyerror!void {
    // Get allocator
    var alloc = std.heap.page_allocator;

    // Initialize submodules
    try bus.init(alloc, romPath, isFastBoot);
    defer bus.deinit(alloc);

    cpu.init(isFastBoot);

    while (cpu.isRunning) {
        const a = cpu.step();
    }
}
