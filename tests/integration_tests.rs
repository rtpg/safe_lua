extern crate safe_lua;
use safe_lua::file_contents;
use safe_lua::{
    eval::{
	LuaRunState,
	initial_run_state,
        exec
    }
};
use std::fs::File;
use std::io::Read;

pub fn load_file<'a>(file_path: &'a str, contents: &'a str) -> LuaRunState<'a> {
    return initial_run_state(contents, file_path);
}

#[test]
fn test_lua_tests(){
    let file_name = "lua_tests/constructs.lua";
    file_contents!(file_name, contents);
    let mut run_state = load_file(&file_name, &contents);

    loop {
        exec::exec_to_next_yield(
            &mut run_state,
            None,
        );
    }
//    assert_eq!(0, 1);
}
