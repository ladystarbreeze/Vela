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

const Screen = struct {
    width : c_int = undefined,
    height: c_int = undefined,
    stride: c_int = undefined,

    texture : ?*SDL.SDL_Texture  = null,
    renderer: ?*SDL.SDL_Renderer = null,
};

const isFastBoot = true;

const cpuCyclesFrame   : i64 = 93_750_000 / 60;
const cpuCyclesScanline: i64 = cpuCyclesFrame / 512;

var screen: Screen = Screen{};

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
        960, 720,
        SDL.SDL_WINDOW_SHOWN,
    ) orelse sdlPanic();
    defer _ = SDL.SDL_DestroyWindow(window);

    screen.renderer = SDL.SDL_CreateRenderer(window, -1, SDL.SDL_RENDERER_ACCELERATED) orelse sdlPanic();
    defer _ = SDL.SDL_DestroyRenderer(screen.renderer);

    if (SDL.SDL_RenderSetLogicalSize(screen.renderer, 960, 720) < 0) {
        sdlPanic();
    }

    if(SDL.SDL_SetHint(SDL.SDL_HINT_RENDER_VSYNC, "1") < 0) {
        sdlPanic();
    }

    // Get allocator
    var alloc = std.heap.page_allocator;

    // Initialize submodules
    try bus.init(alloc, romPath, isFastBoot);
    defer bus.deinit(alloc);

    cpu.init(isFastBoot);

    SDL.SDL_ShowWindow(window);

    changeScreen(320, 3);

    var cyclesRem: i64 = cpuCyclesFrame;
    var lineRem  : i64 = cpuCyclesScanline;

    mainLoop: while (true) {
        var e: SDL.SDL_Event = undefined;

        const c = cpu.step();

        cyclesRem -= c;
        lineRem   -= c;

        if (cyclesRem <= 0) {
            cyclesRem += cpuCyclesFrame;

            if (SDL.SDL_PollEvent(&e) != 0) {
                if (e.type == SDL.SDL_QUIT) {
                    break :mainLoop;
                }
            }

            if (SDL.SDL_UpdateTexture(screen.texture, null, @ptrCast(*u8, &bus.ram[vi.getOrigin()]), screen.width * screen.stride) < 0) {
                sdlPanic();
            }

            if (SDL.SDL_RenderCopy(screen.renderer, screen.texture, null, null) < 0) {
                sdlPanic();
            }

            SDL.SDL_RenderPresent(screen.renderer);
        }

        if (lineRem <= 0) {
            lineRem += cpuCyclesScanline;

            vi.incCurrentV();
        }
    }

    var x = SDL.SDL_DestroyTexture(screen.texture);
}

pub fn changeScreen(width: c_int, pFmt: u2) void {
    if (width == 320) {
        screen.height = 240;
    } else if (width == 640) {
        screen.height = 480;
    } else {
        screen.height = 240;
    }
    
    screen.width = width;

    if (pFmt == 2) {
        screen.stride = 2;

        screen.texture = SDL.SDL_CreateTexture(
            screen.renderer,
            SDL.SDL_PIXELFORMAT_XBGR1555, SDL.SDL_TEXTUREACCESS_STREAMING,
            screen.width, screen.height
        ) orelse sdlPanic();
    }
    else if (pFmt == 3) {
        screen.stride = 4;

        screen.texture = SDL.SDL_CreateTexture(
            screen.renderer,
            SDL.SDL_PIXELFORMAT_XBGR8888, SDL.SDL_TEXTUREACCESS_STREAMING,
            screen.width, screen.height
        ) orelse sdlPanic();
    } else {
    }
}

/// Taken from SDL.zig
fn sdlPanic() noreturn {
    const str = @as(?[*:0]const u8, SDL.SDL_GetError()) orelse "unknown error";
    @panic(std.mem.sliceTo(str, 0));
}
