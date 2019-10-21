extern crate nom;

#[macro_use]
extern crate lazy_static;

mod eval;
mod ast;
mod parse;
mod lex;
mod compile;
use std::fs::File;
use std::io::Read;


#[allow(unused_variables)]
fn main(){
    let filepath = "lua_tests/constructs.lua";
    let mut file = File::open(filepath).unwrap();
    let mut contents = String::new();
    dbg!(&file);
    file.read_to_string(&mut contents).unwrap();
    println!("Starting lex");
    let lex_result = lex::lex_all(contents.as_str());
    // dbg!(lex_result).unwrap();

    println!("Starting parse");
    let parse_result = parse::parse(contents.as_str());
    dbg!(parse_result.clone());

    println!("starting compile");
    let compile_result = compile::compile(parse_result);
    dbg!(compile_result);

    let run_state = eval::initial_run_state(filepath);
    eval::run_to_checkpoint(run_state);
}
