extern crate safe_lua;
use std::fs::File;
use std::io::Read;

use safe_lua::{
    file_contents,
    eval::{
        load_file,
        exec
    }
};

#[test]
fn test_lua_tests(){
    let file_name = "/Users/rtpg/proj/safe_lua/lua_tests/constructs.lua";
    let mut run_state = load_file(&file_name);

    loop {
        exec::exec_to_next_yield(
            &mut run_state,
            None,
        );
    }
//    assert_eq!(0, 1);
}