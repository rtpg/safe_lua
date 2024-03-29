#![allow(unused_must_use)]
use std::io::{stdin, stdout, Write};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};
use termion::terminal_size;

use crate::eval::LuaRunState;

// this frame will track our write bounds
struct TFrame {
    x: u16,
    y: u16,
    width: u16,
    height: u16,
}

fn fpos(frame: &TFrame, x: u16, y: u16) -> termion::cursor::Goto {
    return termion::cursor::Goto(x + frame.x, y + frame.y);
}

fn fhline<W: std::io::Write>(out: &mut RawTerminal<W>, frame: &TFrame, y: u16) {
    write!(out, "{}", fpos(frame, 0, y)).unwrap();
    for _ in 0..frame.width {
        write!(out, "-").unwrap();
    }
}

fn draw_bytecode<W: std::io::Write>(state: &LuaRunState, out: &mut RawTerminal<W>, frame: &TFrame) {
    // top line
    fhline(out, frame, 0);
    // get bytes
    let pc = state.current_frame.pc;
    let bytes = &state.current_frame.code.bytecode[pc..(pc + (frame.height as usize))];
    // frames
    for idx in 1..(frame.height - 1) {
        let byte_repr = match bytes.get((idx as usize) - 1) {
            Some(bc) => format!("{:?}", bc),
            None => "XXX".to_string(),
        };

        write!(out, "{}|Byte {}", fpos(frame, 0, idx), byte_repr,).unwrap();
    }
    // bottom line
    fhline(out, frame, frame.height - 1);
}

fn draw_frames<W: std::io::Write>(out: &mut RawTerminal<W>, frame: &TFrame) {
    // top line
    fhline(out, frame, 0);
    // frames
    for idx in 1..(frame.height - 1) {
        write!(out, "{}| Frame {}", fpos(frame, 0, idx), idx,);
    }
    // bottom line
    fhline(out, frame, frame.height - 1);
}

fn draw_stack<W: std::io::Write>(out: &mut RawTerminal<W>, frame: &TFrame) {
    // top line is drawn from frames
    // fhline(out, frame, 0);
    // frames
    for idx in 0..(frame.height - 1) {
        write!(out, "{}|Stack Elt {}", fpos(frame, 0, idx), idx,);
    }
    // bottom line
    fhline(out, frame, frame.height - 1);
}

fn draw_debugger_state<W: std::io::Write>(state: &LuaRunState, stdout: &mut RawTerminal<W>) {
    // let's figure out the terminal size
    let (term_w, term_h) = terminal_size().unwrap();

    // let's clear the screen
    write!(
        stdout,
        "{}{}",
        // Clear the screen.
        termion::clear::All,
        // Hide the cursor.
        termion::cursor::Hide
    )
    .unwrap();

    /**
     * Our UI is going to look like
     * ----------------------
     * | bytecode | frames  |
     * |          |         |
     * |          |---------|
     * |          | stack   |
     * |          |         |
     * |          |         |
     * ----------------------
     * (q to quit)
     *
     */
    let bytecode_frame = TFrame {
        x: 2,
        y: 2,
        width: (term_w - 3) / 2,
        height: term_h - 10,
    };

    let frames_frame = TFrame {
        x: bytecode_frame.width + bytecode_frame.x,
        y: 2,
        width: term_w - bytecode_frame.width - bytecode_frame.x,
        height: (term_h - 10) / 3,
    };

    let stack_frame = TFrame {
        x: frames_frame.x,
        y: frames_frame.y + frames_frame.height,
        width: frames_frame.width,
        height: frames_frame.height * 2,
    };

    draw_bytecode(state, stdout, &bytecode_frame);
    draw_frames(stdout, &frames_frame);
    draw_stack(stdout, &stack_frame);

    // help line
    write!(
        stdout,
        "{}q to quit, Hi!",
        termion::cursor::Goto(0, term_h - 8),
    );
    // flush to make stuff appear
    stdout.flush().unwrap();
}

pub enum DebugCmd {
    Quit,
    Step,
    Continue,
}

pub fn debugger_loop(state: &LuaRunState) -> DebugCmd {
    let stdin = stdin();
    let mut stdout = stdout().into_raw_mode().unwrap();

    draw_debugger_state(state, &mut stdout);
    for c in stdin.keys() {
        draw_debugger_state(state, &mut stdout);
        // Clear the current line.
        write!(
            stdout,
            "{}{}",
            termion::cursor::Goto(1, 2),
            termion::clear::CurrentLine
        )
        .unwrap();

        // Print the key we type...
        match c.unwrap() {
            // Exit.
            Key::Char('q') => return DebugCmd::Quit,
            Key::Char('s') => return DebugCmd::Step,
            Key::Char('c') => return DebugCmd::Continue,
            Key::Char(c) => println!("{}", c),
            Key::Alt(c) => println!("Alt-{}", c),
            Key::Ctrl(c) => println!("Ctrl-{}", c),
            Key::Left => println!("<left>"),
            Key::Right => println!("<right>"),
            Key::Up => println!("<up>"),
            Key::Down => println!("<down>"),
            _ => println!("Other"),
        }
        // Flush again.
        stdout.flush().unwrap();
    }

    // Show the cursor again before we exit.
    write!(stdout, "{}", termion::cursor::Show).unwrap();
    return DebugCmd::Step;
}
