extern crate nom;
extern crate nom_locate;
extern crate argh;

#[macro_use]
extern crate lazy_static;

use argh::FromArgs;

mod eval;
mod ast;
mod parse;
mod lex;
mod compile;
mod natives;
mod lua_stdlib;
use std::fs::File;
use std::io::Read;
mod utils;


#[derive(FromArgs)]
#[argh(description="Safe Lua")]
struct MainOpts {
    #[argh(positional, description="the script to run")]
    script: String,
    #[argh(switch, description="just show me the bytecode")]
    bytecode: bool,
}


#[allow(unused_variables)]
fn main(){
    let opts: MainOpts = argh::from_env();
    let filepath = opts.script;
    let mut file = File::open(&filepath).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
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
	compile::display::display_code_block(compile_result);
    } else {
	let mut run_state = eval::initial_run_state(&contents, &filepath);
	// for now we're just going to loop over our yielding mechanisms
	loop {
	    use eval::exec::ExecResult::*;
	    match eval::exec::exec_to_next_yield(&mut run_state, None) {
		Done(s) => {
		    println!("Exit code was: {}", s);
		    break;
		},
		Error(s) => {
		    println!("Error on execution: {}", s);
		},
		Yield(s) => {
		    panic!("TODO: implement yield handling. Yielded {}", s);
		}
	    }
	}
    }
    println!("Done!");
}
