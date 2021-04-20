extern crate argh;
extern crate nom;
extern crate nom_locate;
extern crate pretty_assertions;
extern crate termion;

#[macro_use]
extern crate lazy_static;

use argh::FromArgs;
use repl::do_repl;
#[macro_use]
mod macros;
mod ast;
mod compile;
mod debugger;
mod eval;
mod lex;
mod lua_stdlib;
mod natives;

pub mod numbers;
mod parse;
use std::fs::File;
use std::io::Read;
mod repl;
mod utils;

#[derive(FromArgs)]
#[argh(description = "Safe Lua")]
struct MainOpts {
    #[argh(positional, description = "the script to run")]
    script: Option<String>,
    #[argh(option, description = "a single command to run")]
    command: Option<String>,
    #[argh(switch, description = "just show me the bytecode")]
    bytecode: bool,
    #[argh(switch, description = "run the REPL")]
    repl: bool,
    #[argh(switch, description = "hook up the stepping debugger")]
    debug: bool,
}

#[allow(unused_variables)]
fn main() {
    // debugger
    // debugger::debugger_loop();
    let opts: MainOpts = argh::from_env();

    if opts.repl {
        do_repl();
        // the repl should be a loop
        panic!("We shouldn't ever reach this point");
    }

    let filepath;
    let mut contents = String::new();
    // if we got here then we're going to be running a script
    // let's get the content of what we want to run
    match (opts.script, opts.command) {
        (Some(_), Some(_)) => {
            panic!("You should only pass in the script or the command option, not both")
        }
        (None, None) => {
            panic!("Pass in either the script or the command")
        }
        (Some(passed_in_filepath), None) => {
            filepath = passed_in_filepath;
            let mut file = File::open(&filepath).unwrap();
            file.read_to_string(&mut contents).unwrap();
        }
        (None, Some(provided_command)) => {
            filepath = "from_commandline".to_string();
            contents = provided_command;
        }
    }
    println!("Starting lex");
    let contents_lex = lex::LexInput::new(&contents);
    let lex_result = lex::lex_all(contents_lex);
    // dbg!(lex_result).unwrap();

    println!("Starting parse");
    let parse_result = parse::parse(&contents);

    println!("starting compile");
    let compile_result = compile::compile(parse_result, &contents);

    if opts.bytecode {
        // we're only going to show the compilation result
        compile::display::display_code_block(&compile_result);
    } else {
        let mut run_state = eval::initial_run_state(&contents, &filepath);
        let mut debugging = opts.debug;
        // for now we're just going to loop over our yielding mechanisms
        loop {
            if debugging {
                match debugger::debugger_loop() {
                    debugger::DebugCmd::Quit => {
                        println!("Quit from the debugger");
                        break;
                    }
                    debugger::DebugCmd::Step => {
                        // let's step through one operation
                    }
                    debugger::DebugCmd::Continue => {
                        debugging = false;
                    }
                }
            } else {
                use eval::exec::ExecResult::*;
                match eval::exec::exec_to_next_yield(&mut run_state, None) {
                    Done(s) => {
                        println!("Exit code was: {}", s);
                        break;
                    }
                    Error(s) => {
                        println!("Error on execution: {}", s);
                    }
                    Yield(s) => {
                        panic!("TODO: implement yield handling. Yielded {}", s);
                    }
                }
            }
        }
    }
    println!("Done!");
}
