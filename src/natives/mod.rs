pub mod binops;
pub mod package;

use super::eval::{LuaResult, LuaRunState, LV};

pub fn unwrap_single_arg<'a>(args: Option<LV>) -> Option<LV> {
    // helper to unwrap a single arg from args
    match args {
        Some(LV::LuaList(req_args)) => {
            // this means we have a list of args at least
            match req_args.len() {
                1 => {
                    // we confirmed the single arg
                    // unwrap is safe because of size check
                    return Some(req_args.get(0).unwrap().clone());
                }
                _ => {
                    dbg!(req_args);
                    panic!("Too many args sent in");
                }
            }
        }
        Some(other) => {
            dbg!(other);
            panic!("Wrong arg type sent in");
        }
        None => {
            panic!("No args sent in");
        }
    }
}

pub fn lua_truthy(elt: &LV) -> bool {
    match elt {
        LV::LuaFalse => false,
        LV::LuaNil => false,
        _ => true,
    }
}

pub fn lua_assert(_s: &LuaRunState, args: Option<LV>) -> LuaResult {
    match unwrap_single_arg(args) {
        Some(arg) => {
            if lua_truthy(&arg) {
                Ok(arg.clone())
            } else {
                return Err("Assertion failure in Lua Execution".to_string());
            }
        }
        None => {
            return Err("Arity failulre calling assert".to_string());
        }
    }
}

pub fn lua_fmt_for_print(arg: &LV) -> String {
    match arg {
        LV::LuaS(s) => s.to_string(),
        LV::Num(n) => n.to_string(),
        _ => {
            println!("CALLED LUA PRINT ON UNSUPPORTED {}", arg);
            "UNKNOWN_VALUE_FOR_PRINT".to_string()
        }
    }
}
pub fn lua_print(_s: &LuaRunState, args: Option<LV>) -> LuaResult {
    match unwrap_single_arg(args) {
        Some(arg) => {
            println!("{}", lua_fmt_for_print(&arg));
            return Ok(LV::LuaNil);
        }
        None => {
            return Err("Wrong arity for lua_print".to_string());
        }
    }
}

fn lua_type_internal(arg: LV) -> String {
    match arg {
        LV::LuaTrue => "boolean".to_string(),
        LV::LuaFalse => "boolean".to_string(),
        LV::LuaNil => "nil".to_string(),
        LV::LuaTable { .. } => "table".to_string(),
        LV::NativeFunc { .. } => "function".to_string(),
        LV::LuaFunc { .. } => "function".to_string(),
        LV::LuaS(_) => "string".to_string(),
        LV::Num(_) => "number".to_string(),
        _ => {
            dbg!(arg);
            panic!("Type not implemented");
        }
    }
}
pub fn lua_type<'a>(_s: &LuaRunState, args: Option<LV>) -> LuaResult {
    match unwrap_single_arg(args) {
        Some(arg) => {
            let type_name = lua_type_internal(arg);
            return Ok(LV::LuaS(type_name));
        }
        None => {
            return Err("Wrong arity".to_string());
        }
    }
}
pub fn lua_require<'a>(s: &LuaRunState, args: Option<LV>) -> LuaResult {
    match unwrap_single_arg(args) {
        Some(arg) => {
            match arg {
                LV::LuaS(package_name) => {
                    dbg!("TRYING TO LOOKUP");
                    dbg!(&package_name);
                    match s.packages.get(&package_name) {
                        // TODO noclone
                        Some(package) => return Ok(package.clone()),
                        None => {
                            dbg!(package_name);
                            panic!("Failed package import");
                        }
                    }
                }
                _ => {
                    dbg!(arg);
                    panic!("Wrong type passed into string");
                }
            }
        }
        _ => {
            panic!("Incorrect requirement args");
        }
    }
}
