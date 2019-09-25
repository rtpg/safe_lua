#![allow(dead_code)]
extern crate nom;

#[allow(dead_code)]
mod eval;
#[allow(dead_code)]
mod ast;
#[allow(dead_code)]
mod parse;
#[allow(dead_code)]
mod lex;
mod compile;

// fn main(){
//     let filepath = "lua_tests/constructs.lua";
//     let mut file = File::open(filepath).unwrap();
//     let mut contents = String::new();
//     dbg!(&file);
//     file.read_to_string(&mut contents).unwrap();
//     println!("Starting lex");
//     let lex_result = lex::lex_all(contents.as_str());
//     // dbg!(lex_result).unwrap();

//     println!("Starting parse");
//     let parse_result = parse::parse(contents.as_str());
//     // dbg!(parse_result.clone());
//     println!("Starting compile");
//     let compile_result = compile::compile(parse_result);
//     dbg!(compile_result);

//     let run_state = eval::initial_run_state(filepath);
//     eval::run_to_checkpoint(run_state);
// }


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
