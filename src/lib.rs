extern crate nom;

mod eval;
mod ast;
mod parse;
mod lex;
mod compile;
use std::fs::File;
use std::io::Read;

fn main(){
    let filepath = "lua_tests/constructs.lua";
    let mut file = File::open(filepath).unwrap();
    let mut contents = String::new();
    dbg!(&file);
    file.read_to_string(&mut contents).unwrap();
    let parse_result = lex::lex_all(contents.as_str());
    dbg!(parse_result).unwrap();

    let run_state = eval::initial_run_state(filepath);
    eval::run_to_checkpoint(run_state);
}


#[cfg(test)]
#[test]
fn it_works() {
    return;
    let test_file_path = "lua-5.3.4-tests/constructs.lua";
    let run_state = eval::initial_run_state(test_file_path);
    let result = eval::run_to_checkpoint(run_state);
    assert_eq!(
        result,
        eval::RunResult::Done("OK".to_string())
    )
}
