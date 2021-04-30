use eval::LuaResult;
use eval::LuaRunState;
use eval::LV;
use eval::LV::*;
use natives::unwrap_single_arg;

use crate::eval::LNum;

pub fn unwrap_num_or_stringed_num(l: &LV) -> Result<LNum, String> {
    // takes a number or a string and tries to unwrap it
    // used by tonumber, among other things
    match l {
        Num(n) => Ok(*n),
        LuaS(s) => match s.parse::<f64>() {
            Ok(result) => Ok(LNum::Float(result)),
            Err(_) => Err("not a number".to_string()),
        },
        _ => Err("not a number".to_string()),
    }
}

pub fn lua_tonumber<'a>(_s: &LuaRunState, args: Option<LV>) -> LuaResult {
    match unwrap_single_arg(args) {
        Some(arg) => match unwrap_num_or_stringed_num(&arg) {
            Ok(value) => Ok(Num(value)),
            Err(err) => Err(err),
        },
        _ => Err("wrong arity".to_string()),
    }
}
