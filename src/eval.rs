#[allow(dead_code)]

use lex;
#[allow(dead_code)]
pub struct LuaRunState {
    /*
    * store the current state of a program 
    */
    file_path: String,
    parsed_content: Vec<lex::Lex>
}

#[allow(dead_code)]
#[derive(PartialEq,Debug)]
pub enum RunResult {
    Error(String),
    Done(String)
}

#[allow(dead_code, unused_variables)]
pub fn run_to_checkpoint(state: LuaRunState) -> RunResult {
    /*
     * Do Stuff and return a result
     */
    return RunResult::Error(String::from("Yikes"));
}



pub fn initial_run_state(lua_file_path: &str) -> LuaRunState {

    let parsed_content = lex::lex_all(lua_file_path).unwrap().1;

    return LuaRunState {
        file_path: String::from(lua_file_path),
        parsed_content: parsed_content
    };
}
