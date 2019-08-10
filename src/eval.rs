pub struct LuaRunState {
    /*
    * store the current state of a program 
    */
    file_path: String,
}

#[derive(PartialEq,Debug)]
pub enum RunResult {
    Error(String),
    Done(String)
}

pub fn run_to_checkpoint(state: LuaRunState) -> RunResult {
    /*
     * Do Stuff and return a result
     */
    return RunResult::Error(String::from("Yikes"));
}



pub fn initial_run_state(lua_file_path: &str) -> LuaRunState {
    return LuaRunState {
        file_path: String::from(lua_file_path)
    };
}
