//!
//! Vela - Experimental N64 emulator written in Zig.
//! Copyright (c) 2022 Michelle-Marie Schiller
//!
//! n64.zig - Main emulator module. 
//!

const std = @import("std");
const SDL = @import("sdl2");

const bus = @import("bus.zig");
const cpu = @import("cpu.zig");
const vi  = @import("vi.zig");

const isFastBoot = true;

const cpuCyclesFrame: i64 = 93_750_000 / 60;

/// Initializes all submodules
/// TODO: this is terrible clean this up
pub fn run(romPath: []const u8) anyerror!void {
    if (SDL.SDL_Init(SDL.SDL_INIT_VIDEO) < 0) {
        sdlPanic();
    }
    defer SDL.SDL_Quit();

    var window = SDL.SDL_CreateWindow(
        "Vela",
        SDL.SDL_WINDOWPOS_CENTERED, SDL.SDL_WINDOWPOS_CENTERED,
        640, 480,
        SDL.SDL_WINDOW_SHOWN,
    ) orelse sdlPanic();
    defer _ = SDL.SDL_DestroyWindow(window);

    var renderer = SDL.SDL_CreateRenderer(window, -1, SDL.SDL_RENDERER_ACCELERATED) orelse sdlPanic();
    defer _ = SDL.SDL_DestroyRenderer(renderer);

    if (SDL.SDL_RenderSetLogicalSize(renderer, 640, 480) < 0) {
        sdlPanic();
    }

    if(SDL.SDL_SetHint(SDL.SDL_HINT_RENDER_VSYNC, "1") < 0) {
        sdlPanic();
    }

    var texture = SDL.SDL_CreateTexture(
        renderer,
        SDL.SDL_PIXELFORMAT_XBGR8888, SDL.SDL_TEXTUREACCESS_STREAMING,
        320, 240
    ) orelse sdlPanic();
    defer _ = SDL.SDL_DestroyTexture(texture);

    // Get allocator
    var alloc = std.heap.page_allocator;

    // Initialize submodules
    try bus.init(alloc, romPath, isFastBoot);
    defer bus.deinit(alloc);

    cpu.init(isFastBoot);

    SDL.SDL_ShowWindow(window);

    var cyclesRem: i64 = cpuCyclesFrame;

    mainLoop: while (true) {
        var e: SDL.SDL_Event = undefined;

        cyclesRem -= cpu.step();

        if (cyclesRem <= 0) {
            cyclesRem += cpuCyclesFrame;

            if (SDL.SDL_PollEvent(&e) != 0) {
                if (e.type == SDL.SDL_QUIT) {
                    break :mainLoop;
                }
            }

            if (SDL.SDL_UpdateTexture(texture, null, @ptrCast(*u8, &bus.ram[vi.getOrigin()]), 320 * 4) < 0) {
                sdlPanic();
            }

            if (SDL.SDL_RenderCopy(renderer, texture, null, null) < 0) {
                sdlPanic();
            }

            SDL.SDL_RenderPresent(renderer);
        }
    }
}

/// Taken from SDL.zig
fn sdlPanic() noreturn {
    const str = @as(?[*:0]const u8, SDL.SDL_GetError()) orelse "unknown error";
    @panic(std.mem.sliceTo(str, 0));
}
