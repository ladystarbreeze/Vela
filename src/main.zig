//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! main.zig - Main function. 
//!

const std = @import("std");

const n64 = @import("/core/n64.zig");

// Passed:
// addiu
// andi
// basic
// daddiu
// dsll
// dsll32
// dsra32
// ori
// sll
// slti
// sltiu
// sra
// srl
// xori
const romPath = "./velaFiles/sltiu_simpleboot.z64";

pub fn main() anyerror!void {
    try n64.run(romPath);
}
