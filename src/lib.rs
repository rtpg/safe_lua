#![allow(dead_code)]
extern crate nom;
extern crate nom_locate;
extern crate pretty_assertions;

#[macro_use]
extern crate lazy_static;

#[allow(dead_code)]
pub mod ast;
pub mod compile;
#[allow(dead_code)]
pub mod eval;
#[allow(dead_code)]
pub mod lex;
pub mod lua_stdlib;
pub mod macros;
pub mod natives;
pub mod numbers;
#[allow(dead_code)]
pub mod parse;
pub mod utils;
pub fn main() {
    println!("Starting lex");
    println!("Starting parse");
}
