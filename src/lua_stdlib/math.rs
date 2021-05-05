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

pub fn lua_coerce_float(v: &LV) -> Result<f64, LuaErr> {
    let lnum_val = lua_coerce_lnum(v)?;
    match lnum_val {
        LNum::Float(w) => Ok(w),
        LNum::Int(w) => Ok(w as f64),
    }
}

pub fn lua_coerce_int(v: &LV) -> Result<i64, LuaErr> {
    match v {
        LV::Num(n) => match n {
            LNum::Float(v) => Ok(*v as i64),
            LNum::Int(v) => Ok(*v),
        },
        LV::LuaS(_) => todo!("coerce string to int"),
        _ => LuaErr::msg("Not a number"),
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

pub fn lua_math_modf(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let arg = args.get_arg_as_number(0)?;
    let integral_part = match arg {
        LNum::Int(i) => LNum::Int(i),
        LNum::Float(f) => {
            if f.is_finite() {
                LNum::Float(f.trunc())
            } else {
                LNum::Float(f)
            }
        }
    };

    let fractional_part = match arg {
        LNum::Int(_i) => LNum::Float(0.0),
        LNum::Float(f) => {
            if f.is_finite() {
                LNum::Float(f - f.trunc())
            } else if f.is_nan() {
                LNum::Float(f)
            } else {
                LNum::Float(0.0)
            }
        }
    };

    return Ok(LV::LuaList(vec![
        LV::Num(integral_part),
        LV::Num(fractional_part),
    ]));
}
pub fn lua_math_type(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    match args.get_lv_arg(0)? {
        LV::Num(n) => match n {
            LNum::Float(_) => Ok(LV::LuaS("float".to_string())),
            LNum::Int(_) => Ok(LV::LuaS("integer".to_string())),
        },
        _ => Ok(LV::LuaNil),
    }
}
fn not_number() -> LuaResult {
    return LuaErr::msg("Provided value wasn't a number");
}

#[test]
fn test_overflow() {
    assert_eq!(i64::MAX, i64::MIN.overflowing_sub(1).0);
}

pub fn math_pkg(s: &mut LuaAllocator) -> LV {
    let mut pkg = s.allocate_tbl();
    lua_set_native(&mut pkg, "log", lua_log).unwrap();
    lua_set_native(&mut pkg, "floor", lua_floor).unwrap();
    lua_set_native(&mut pkg, "type", lua_math_type).unwrap();
    lua_set_native(&mut pkg, "modf", lua_math_modf).unwrap();
    lua_ssetattr(&mut pkg, "huge", LV::Num(LNum::Float(f64::INFINITY))).unwrap();
    lua_ssetattr(&mut pkg, "maxinteger", LV::Num(LNum::Int(i64::MAX))).unwrap();
    lua_ssetattr(&mut pkg, "mininteger", LV::Num(LNum::Int(i64::MIN))).unwrap();
    return pkg;
}
