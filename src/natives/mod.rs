pub mod binops;

use super::eval::{
    LV,
    LuaRunState
};

fn unwrap_single_arg(args: Option<LV>) -> Option<LV> {
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
pub fn lua_assert<'a>(_s: &LuaRunState, args: Option<LV>) -> LV<'a> {
    match unwrap_single_arg(args){
	Some(arg) => {
	    match arg {
		LV::LuaTrue => {return LV::LuaNil},
		_ => {
		    dbg!(arg);
		    panic!("Assertion failure in Lua Execution");
		}
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
