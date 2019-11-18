#![allow(dead_code)]
extern crate nom;

#[macro_use]
extern crate lazy_static;

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
pub fn main(){
    println!("Starting lex");
    println!("Starting parse");
}
