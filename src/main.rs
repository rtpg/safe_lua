extern crate nom;
extern crate nom_locate;

#[macro_use]
extern crate lazy_static;

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

#[allow(unused_variables)]
fn main(){
    let filepath = "lua_tests/constructs.lua";
    let mut file = File::open(filepath).unwrap();
    let mut contents = String::new();
    dbg!(&file);
    file.read_to_string(&mut contents).unwrap();
    println!("Starting lex");
    let contents_lex = lex::LexInput::new(&contents);
    let lex_result = lex::lex_all(contents_lex);
    // dbg!(lex_result).unwrap();

    println!("Starting parse");
    let parse_result = parse::parse(&contents);
    dbg!(parse_result.clone());

    println!("starting compile");
    let compile_result = compile::compile(parse_result);
    dbg!(compile_result);

    let run_state = eval::initial_run_state(filepath);
    eval::run_to_checkpoint(run_state);
}
