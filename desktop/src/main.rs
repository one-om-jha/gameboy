use gameboy::*;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::render::Canvas;
use sdl2::video::Window;
use std::env;
use std::fs::File;
use std::io::Read;

const SCALE: u32 = 15;
const WINDOW_WIDTH: u32 = (SCREEN_WIDTH as u32) * SCALE;
const WINDOW_HEIGHT: u32 = (SCREEN_HEIGHT as u32) * SCALE;

const TICKS_PER_FRAME: usize = 10;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: cargo run /path/to/game");
        return;
    }

    // Setup SDL
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("GAMEBOY EMULATOR", WINDOW_WIDTH, WINDOW_HEIGHT)
        .position_centered()
        .opengl()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut gb = Emu::new();

    let mut rom = File::open(&args[1]).expect("Unable to open file.");
    let mut buffer = Vec::new();
    rom.read_to_end(&mut buffer).unwrap();
    gb.load(&buffer);

    'gameplay: loop {
        for evt in event_pump.poll_iter() {
            match evt {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => {
                    break 'gameplay;
                }
                Event::KeyDown {
                    keycode: Some(key), ..
                } => {
                    if let Some(k) = key2btn(key) {
                        gb.keypress(k, true);
                    }
                }
                Event::KeyUp {
                    keycode: Some(key), ..
                } => {
                    if let Some(k) = key2btn(key) {
                        gb.keypress(k, false);
                    }
                }
                _ => (),
            }
        }

        for _ in 0..TICKS_PER_FRAME {
            gb.tick();
        }
        gb.tick_timers();
        draw_screen(&gb, &mut canvas);
    }
}

fn draw_screen(emu: &Emu, canvas: &mut Canvas<Window>) {
    // Clear Canvas as black
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();

    let screen_buf = emu.get_display();

    // canvas.set_draw_color(Color::RGB(255, 255, 255));
    // for (i, pixel) in screen_buf.iter().enumerate() {
    //     if *pixel {
    //         let x = (i % SCREEN_WIDTH) as u32;
    //         let y = (i / SCREEN_WIDTH) as u32;

    //         let rect = Rect::new((x * SCALE) as i32, (y * SCALE) as i32, SCALE, SCALE);
    //         canvas.fill_rect(rect).unwrap();
    //     }
    // }
    for (i, pixel) in screen_buf.iter().enumerate() {
        let (r,g,b) = *pixel;
        let x = (i % SCREEN_WIDTH) as u32;
        let y = (i / SCREEN_WIDTH) as u32;
        canvas.set_draw_color(Color::RGB(r,g,b));
        let rect = Rect::new((x * SCALE) as i32, (y * SCALE) as i32, SCALE, SCALE);
        canvas.fill_rect(rect).unwrap();
    }
    canvas.present();
}

fn key2btn(key: Keycode) -> Option<usize> {
    match key {
        Keycode::W => Some(0),
        Keycode::D => Some(1),
        Keycode::S => Some(2),
        Keycode::A => Some(3),
        Keycode::J => Some(4),
        Keycode::K => Some(5),
        Keycode::I => Some(6),
        Keycode::O => Some(7),
        _ => None,
    }
}
