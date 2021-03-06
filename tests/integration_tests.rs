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

pub fn load_file<'a>(file_path: &'a str, contents: &'a str) -> LuaRunState {
    return initial_run_state(contents, file_path);
}


macro_rules! lua_tests {
    ($($name: ident: $file: expr,)*) => {
	$(
	    #[test]
	    fn $name(){
		let file_name = $file;
		file_contents!(file_name, contents);
		let mut run_state = load_file(&file_name, &contents);

		loop {
		    exec::exec_to_next_yield(
			&mut run_state,
			None
		    );
		}
	    }
	)*
    } 
}

lua_tests! {
    test_all: "lua_tests/all.lua",
    test_api: "lua_tests/api.lua",
    test_attrib: "lua_tests/attrib.lua",
    test_big: "lua_tests/big.lua",
    test_bitwise: "lua_tests/bitwise.lua",
    test_calls: "lua_tests/calls.lua",
    test_closure: "lua_tests/closure.lua",
    test_code: "lua_tests/code.lua",
    test_constructs: "lua_tests/constructs.lua",
    test_coroutine: "lua_tests/coroutine.lua",
    test_db: "lua_tests/db.lua",
    test_errors: "lua_tests/errors.lua",
    test_events: "lua_tests/events.lua",
    test_files: "lua_tests/files.lua",
    test_gc: "lua_tests/gc.lua",
    test_goto: "lua_tests/goto.lua",
    test_literals: "lua_tests/literals.lua",
    test_locals: "lua_tests/locals.lua",
    test_ltests: "lua_tests/ltests.lua",
    test_main: "lua_tests/main.lua",
    test_math: "lua_tests/math.lua",
    test_nextvar: "lua_tests/nextvar.lua",
    test_pm: "lua_tests/pm.lua",
    test_sort: "lua_tests/sort.lua",
    test_strings: "lua_tests/strings.lua",
    test_tpack: "lua_tests/tpack.lua",
    test_utf8: "lua_tests/utf8.lua",
    test_vararg: "lua_tests/vararg.lua",
    test_verybig: "lua_tests/verybig.lua",
}
