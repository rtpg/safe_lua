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
pub fn lua_assert(_s: &LuaRunState, args: Option<LV>) -> LV {
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
pub fn lua_print(_s: &LuaRunState, args: Option<LV>) -> LV {
    println!("CALLED LUA PRINT");
    match unwrap_single_arg(args){
	Some(arg) => {
	    dbg!(arg);
	    return LV::LuaNil;
	},
	None => {
	    panic!("Wrong arity for lua_print");
	}
    }
}

pub fn lua_require(s: &LuaRunState, args: Option<LV>) -> LV {
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
