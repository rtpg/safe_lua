#![allow(dead_code)]
extern crate nom;
extern crate nom_locate;
extern crate pretty_assertions;

#[macro_use]
extern crate lazy_static;

pub mod macros;
#[allow(dead_code)]
pub mod eval;
#[allow(dead_code)]
pub mod ast;
#[allow(dead_code)]
pub mod parse;
#[allow(dead_code)]
pub mod lex;
pub mod compile;
pub mod utils;
pub mod natives;
pub mod lua_stdlib;
pub fn main(){
    println!("Starting lex");
    println!("Starting parse");
}
