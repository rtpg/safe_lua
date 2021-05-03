use eval::LuaResult;
use eval::LuaRunState;
use eval::LV;
use eval::LV::*;

use crate::{
    eval::{LNum, LuaErr},
    natives::LuaArgs,
};

pub fn unwrap_num_or_stringed_num(l: &LV) -> Result<LNum, LuaErr> {
    // takes a number or a string and tries to unwrap it
    // used by tonumber, among other things
    match l {
        Num(n) => Ok(*n),
        LuaS(s) => match s.parse::<f64>() {
            Ok(result) => Ok(LNum::Float(result)),
            Err(_) => LuaErr::msg("not a number"),
        },
        _ => LuaErr::msg("not a number"),
    }
}

pub fn lua_tonumber<'a>(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let arg = args.get_lv_arg(0)?;
    let value = unwrap_num_or_stringed_num(arg)?;
    return Ok(Num(value));
}
