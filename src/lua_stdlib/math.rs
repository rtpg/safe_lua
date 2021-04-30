use datastructures::{lua_set_native, lua_ssetattr};
use eval::{LuaResult, LuaRunState, LV};

use eval::LuaErr;

use crate::eval::LuaAllocator;

fn lua_coerce_number(v: &LV) -> Result<f64, LuaErr> {
    match v {
        LV::Num(n) => Ok(*n),
        _ => Err("Not a number".to_string()),
    }
}
pub fn lua_log(_s: &LuaRunState, args: Option<LV>) -> LuaResult {
    let (x_num, maybe_base_num) = match &args {
        Some(LV::LuaList(v)) => {
            if v.len() == 0 {
                return Err("Not enough arguments".to_string());
            }
            let first_result = lua_coerce_number(&v[0])?;
            let second_result = if v.len() > 1 {
                Some(lua_coerce_number(&v[1])?)
            } else {
                None
            };
            (first_result, second_result)
        }
        _ => return Err("Invalid shape".to_string()),
        None => return Err("Not enough arguments".to_string()),
    };

    let result = match maybe_base_num {
        None => x_num.ln(),
        Some(base_num) => x_num.log(base_num),
    };
    return Ok(LV::Num(result));
}

pub fn math_pkg(s: &mut LuaAllocator) -> LV {
    let mut pkg = s.allocate_tbl();
    lua_set_native(&mut pkg, "log", lua_log);
    lua_ssetattr(&mut pkg, "maxinteger", LV::Num(i64::MAX as f64));
    return pkg;
}
