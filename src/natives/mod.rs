pub mod binops;


use super::eval::{
    LV,
    LuaRunState
};

fn unwrap_single_arg<'a>(args: Option<LV<'a>>) -> Option<LV<'a>> {
    // helper to unwrap a single arg from args
    match args {
	Some(LV::LuaList(req_args)) => {
	    // this means we have a list of args at least
	    match req_args.len() {
		1 => {
		    // we confirmed the single arg
		    // unwrap is safe because of size check
		    return Some(req_args.get(0).unwrap().clone());
		},
		_ => {
		    dbg!(req_args);
		    panic!("Too many args sent in");
		}
	    }
	},
	Some(other) => {
	    dbg!(other);
	    panic!("Wrong arg type sent in");
	}
	None => {
	    panic!("No args sent in");
	}
    }
}
macro_rules! vm_panic {
    ($s: expr, $err: expr) => {
	println!("!! Lua VM crash");
	// TODO unwind the entire frame stack here for more information
	let f = &$s.current_frame;
	let (line_no, lines) = f.code.sourcemap.get_lines_for_bytecode(f.pc);
	println!("!! failed on line {} of {}", line_no, $s.file_path);
	println!("--> {}", lines);
	println!("With:");
	dbg!($err);
	panic!("VM CRASH");
    }
}

pub fn lua_truthy(elt: &LV) -> bool {
    match elt {
	LV::LuaFalse => false,
	LV::LuaNil => false,
	_ => true
    }
}

pub fn lua_assert<'a, 'b>(_s: &LuaRunState, args: Option<LV<'a>>) -> LV<'a> {
    match unwrap_single_arg(args){
	Some(arg) => {
	    if lua_truthy(&arg) {
		arg.clone()
	    } else {
		dbg!(arg);
		vm_panic!(_s, "Assertion failure in Lua Execution");
	    }
	},
	None => {
	    panic!("Arity failure calling assert");
	}
    }
}

pub fn lua_fmt_for_print(arg: LV) -> String {
    match arg {
	LV::LuaS(s) => s,
	LV::Num(n) => n.to_string(),
	_ => {
	    println!("CALLED LUA PRINT ON UNSUPPORTED {}", arg);
	    "UNKNOWN_VALUE_FOR_PRINT".to_string()
	}
    }
}
pub fn lua_print<'a>(_s: &LuaRunState, args: Option<LV>) -> LV<'a> {
    match unwrap_single_arg(args){
	Some(arg) => {
	    println!("{}", lua_fmt_for_print(arg));
	    return LV::LuaNil;
	},
	None => {
	    panic!("Wrong arity for lua_print");
	}
    }
}

fn lua_type_internal<'a>(arg: LV) -> String {
    match arg {
	LV::LuaTrue => "boolean".to_string(),
	LV::LuaFalse => "boolean".to_string(),
	LV::LuaNil => "nil".to_string(),
	LV::LuaTable {..} => "table".to_string(),
	LV::NativeFunc {..} => "function".to_string(),
	LV::LuaFunc {..} => "function".to_string(),
	LV::LuaS(_) => "string".to_string(),
	LV::Num(_) => "number".to_string(),
	_ => {
	    dbg!(arg);
	    panic!("Type not implemented");
	}
    }
}
pub fn lua_type<'a>(_s: &LuaRunState, args: Option<LV>) -> LV<'a> {
    match unwrap_single_arg(args){
	Some(arg) => {
	    let type_name = lua_type_internal(arg);
	    return LV::LuaS(type_name);
	},
	None => {
	    panic!("Wrong arity");
	}
    }
}
pub fn lua_require<'a>(s: &LuaRunState<'a>, args: Option<LV>) -> LV<'a> {
    match unwrap_single_arg(args) {
	Some(arg) => {
	    match arg {
		LV::LuaS(package_name) => {
		    match s.packages.get(&package_name) {
			// TODO noclone
			Some(package) => return package.clone(),
			None => {
			    dbg!(package_name);
			    panic!("Failed package import");
			}
		    }
		},
		_ => {
		    dbg!(arg);
		    panic!("Wrong type passed into string");
		}
	    }
	},
	_ => {
	    panic!("Incorrect requirement args");
	}
    }
}
