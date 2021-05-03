use datastructures::{lua_set_native, lua_ssetattr};
use eval::{LuaResult, LuaRunState, LV};

use eval::LNum;
use eval::LuaErr;

use crate::{
    eval::LuaAllocator,
    natives::{lua_coerce_lnum, LuaArgs},
};

pub fn lfloat(f: f64) -> LV {
    return LV::Num(LNum::Float(f));
}

pub fn lua_coerce_number(v: &LV) -> Result<f64, LuaErr> {
    match v {
        LV::Num(n) => match n {
            LNum::Float(v) => Ok(*v),
            LNum::Int(v) => Ok(*v as f64),
        },
        LV::LuaS(_) => todo!("coerce string to number"),
        _ => Err("Not a number".to_string()),
    }
}

pub fn lua_coerce_int(v: &LV) -> Result<i64, LuaErr> {
    match v {
        LV::Num(n) => match n {
            LNum::Float(v) => Ok(*v as i64),
            LNum::Int(v) => Ok(*v),
        },
        LV::LuaS(_) => todo!("coerce string to int"),
        _ => Err("Not a number".to_string()),
    }
}
pub fn lua_log(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let x_num = args.get_arg_as_number(0)?;
    let maybe_base_num = match args.get_lv_arg_or_none(1) {
        None => None,
        Some(v) => Some(lua_coerce_lnum(v)?),
    };

    let result = match maybe_base_num {
        None => x_num.as_float().ln(),
        Some(base_num) => x_num.as_float().log(base_num.as_float()),
    };
    return Ok(lfloat(result));
}

pub fn lua_floor(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let arg = args.get_lv_arg(0)?;
    match arg {
        LV::Num(n) => match n {
            LNum::Float(f) => Ok(LV::Num(LNum::Float(f.floor()))),
            LNum::Int(_) => Ok(LV::Num(*n)),
        },
        _ => not_number(),
    }
}

fn not_number() -> LuaResult {
    return Err("Provided value wasn't a number".to_string());
}

#[test]
fn test_overflow() {
    assert_eq!(i64::MAX, i64::MIN.overflowing_sub(1).0);
}

pub fn math_pkg(s: &mut LuaAllocator) -> LV {
    let mut pkg = s.allocate_tbl();
    lua_set_native(&mut pkg, "log", lua_log).unwrap();
    lua_set_native(&mut pkg, "floor", lua_floor).unwrap();
    lua_ssetattr(&mut pkg, "maxinteger", LV::Num(LNum::Int(i64::MAX))).unwrap();
    lua_ssetattr(&mut pkg, "mininteger", LV::Num(LNum::Int(i64::MIN))).unwrap();
    return pkg;
}
