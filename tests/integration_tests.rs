extern crate safe_lua;

use safe_lua::{
    eval::{
	LuaRunState,
	initial_run_state,
        exec
    }
};

pub fn load_file(file_path: &str) -> LuaRunState {
    return initial_run_state(file_path);
}

#[test]
fn test_lua_tests(){
    let file_name = "lua_tests/constructs.lua";
    let mut run_state = load_file(&file_name);

    loop {
        exec::exec_to_next_yield(
            &mut run_state,
            None,
        );
    }
//    assert_eq!(0, 1);
}
