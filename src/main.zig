//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! main.zig - Main function. 
//!

const std = @import("std");

const n64 = @import("/core/n64.zig");

const romPath = "./velaFiles/basic_simpleboot.z64";

pub fn main() anyerror!void {
    try n64.run(romPath);
}
