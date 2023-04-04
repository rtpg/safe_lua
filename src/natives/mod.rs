pub mod binops;
pub mod package;

use crate::{
    compile::numeral_to_lnum,
    eval::{LNum, LuaErr},
};

use super::eval::{LuaResult, LuaRunState, LV};

/**
 * helper struct to deal with arguments from lua
 **/
pub struct LuaArgs {
    pub args: Vec<LV>,
}

pub fn lua_coerce_lnum(v: &LV) -> Result<LNum, LuaErr> {
    match v {
        LV::Num(n) => Ok(*n),
        LV::LuaS(s) => numeral_to_lnum(&s),
        _ => LuaErr::msg("Not a number"),
    }
}

impl LuaArgs {
    pub fn get_lv_arg(&self, arg: usize) -> Result<&LV, LuaErr> {
        match self.args.get(arg) {
            None => LuaErr::msg("No argument"),
            Some(v) => Ok(v),
        }
    }

    pub fn get_lv_arg_or_none(&self, arg: usize) -> Option<&LV> {
        return self.args.get(arg);
    }

    pub fn get_string_arg(&self, arg: usize) -> Result<String, LuaErr> {
        match self.args.get(arg) {
            None => LuaErr::msg("No argument"),
            Some(v) => match v {
                LV::LuaS(s) => Ok(s.to_string()),
                _ => LuaErr::msg("Non-string argument"),
            },
        }
    }

    pub fn get_arg_as_number(&self, arg: usize) -> Result<LNum, LuaErr> {
        let arg = self.get_lv_arg(arg)?;
        return lua_coerce_lnum(arg);
    }
}

pub fn unwrap_single_arg(args: &LuaArgs) -> Result<&LV, LuaErr> {
    // helper to unwrap a single arg from args
    return args.get_lv_arg(0);
}

pub fn lua_truthy(elt: &LV) -> bool {
    match elt {
        LV::LuaFalse => false,
        LV::LuaNil => false,
        _ => true,
    }
}

pub fn lua_noop(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    // this is a placeholder for a lot of stuff
    eprintln!("Called noop");
    return Ok(LV::LuaNil);
}

pub fn lua_pcall(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    todo!()
}

pub fn lua_assert(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let arg = args.get_lv_arg(0)?;
    if lua_truthy(&arg) {
        Ok(arg.clone())
    } else {
        return LuaErr::msg("Assertion failure in Lua Execution");
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
pub fn lua_print(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let arg = args.get_lv_arg(0)?;
    println!("{}", lua_fmt_for_print(arg));
    return Ok(LV::LuaNil);
}

fn lua_type_internal(arg: &LV) -> String {
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
pub fn lua_type<'a>(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let arg = unwrap_single_arg(args)?;
    let type_name = lua_type_internal(arg);
    return Ok(LV::LuaS(type_name));
}
pub fn lua_require<'a>(s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let package_name = args.get_string_arg(0)?;
    match s.packages.get(&package_name) {
        // TODO noclone
        Some(package) => return Ok(package.clone()),
        None => {
            dbg!(package_name);
            panic!("Failed package import");
        }
    }
}
