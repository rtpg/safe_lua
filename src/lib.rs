extern crate nom;

mod eval;
mod ast;
mod parse;
mod lex;

fn main(){
}

#[cfg(test)]
#[test]
fn it_works() {
    let test_file_path = "lua-5.3.4-tests/constructs.lua";
    let run_state = eval::initial_run_state(test_file_path);
    let result = eval::run_to_checkpoint(run_state);
    assert_eq!(
        result,
        eval::RunResult::Done("OK".to_string())
    )
}
