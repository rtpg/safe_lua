#![allow(dead_code)]
extern crate nom;

#[macro_use]
extern crate lazy_static;

#[allow(dead_code)]
mod eval;
#[allow(dead_code)]
mod ast;
#[allow(dead_code)]
mod parse;
#[allow(dead_code)]
mod lex;
mod compile;
mod utils;
fn main(){
    println!("Starting lex");
    println!("Starting parse");
}
