use eval::LuaResult;
use eval::LV;
use eval::LV::*;
use lua_stdlib::math::lfloat;
use lua_stdlib::math::{lua_coerce_float, lua_coerce_int};
use natives::lua_coerce_lnum;
use natives::lua_fmt_for_print;
use natives::lua_truthy;

pub fn lua_binop_eq<'a>(l: &LV, r: &LV) -> LV {
    if lua_binop_eq_impl(l, r) {
        return LuaTrue;
    } else {
        return LuaFalse;
    }
}
pub fn lua_binop_neq<'a>(l: &LV, r: &LV) -> LV {
    if !lua_binop_eq_impl(l, r) {
        return LuaTrue;
    } else {
        return LuaFalse;
    }
}
pub fn lua_binop_eq_impl(l: &LV, r: &LV) -> bool {
    // println!("RUNNING EQ ON");
    // dbg!(&l);
    // dbg!(&r);
    match l {
        LuaNil => {
            matches!(r, LuaNil)
        }
        Num(n) => match r {
            Num(m) => n == m,
            _ => false,
        },
        LuaS(n) => match r {
            LuaS(m) => n == m,
            _ => false,
        },
        LuaTrue => matches!(r, LuaTrue),
        LuaFalse => matches!(r, LuaFalse),
        LuaList(_) => todo!(),
        LuaTable { id, .. } => match r {
            LuaTable { id: id2, .. } => {
                dbg!(l);
                dbg!(r);
                id == id2
            }
            _ => false,
        },
        LuaFunc { id, .. } => match r {
            LuaFunc { id: id2, .. } => id == id2,
            _ => false,
        },
        CodeIndex(_) => todo!(),
        Code(_) => todo!(),
        NameList(_, _) => todo!(),
        NativeFunc { .. } => todo!(),
        BytecodeFunc { .. } => todo!(),
        PCall { .. } => todo!(),
    }
}

pub fn lua_binop_leq<'a>(l: &LV, r: &LV) -> LV {
    if lua_binop_leq_impl(l, r) {
        return LuaTrue;
    } else {
        return LuaFalse;
    }
}
pub fn lua_binop_leq_impl(l: &LV, r: &LV) -> bool {
    match l {
        LuaNil => {
            matches!(r, LuaNil)
        }
        Num(n) => match r {
            Num(m) => n <= m,
            _ => false,
        },
        _ => {
            dbg!(l);
            dbg!(r);
            panic!("need binop impl");
        }
    }
}
pub fn lua_exponent_eq<'a>(l: &LV, r: &LV) -> LuaResult {
    match (lua_coerce_float(l), lua_coerce_float(r)) {
        (Ok(ll), Ok(rr)) => Ok(lfloat(ll.powf(rr))),
        _ => {
            let msg = format!("Type mismatch on exponent. Received {0} and {1}", l, r);
            return LuaErr::msg(msg);
        }
    }
}

// pub fn lua_binop_minus<'a>(l: &LV, r: &LV) -> LV {
//     match l {
//         Num(n) => match r {
//             Num(m) => LV::Num(n - m),
//             _ => panic!("FAILURE (need to implement lua-bubbling failure)"),
//         },
//         _ => {
//             dbg!(l);
//             dbg!(r);
//             panic!("need binop impl");
//         }
//     }
// }

macro_rules! binop {
    ($impl_name: ident, $external_name: ident, $left: ident, $right: ident, $number_op: expr) => {
        pub fn $impl_name(l: &LV, r: &LV) -> LuaResult {
            match (lua_coerce_lnum(l), lua_coerce_lnum(r)) {
                (Ok($left), Ok($right)) => Ok(LV::Num($number_op)),
                (_, _) => {
                    // TODO add metamethod mechanisms here
                    // dbg!(lua_coerce_lnum(l));
                    // dbg!(lua_coerce_lnum(r));
                    return LuaErr::msg(format!("{0} and {1} can't be added ($impl_name)", l, r));
                }
            }
        }
        pub fn $external_name(l: &LV, r: &LV) -> LV {
            $impl_name(l, r).unwrap()
        }
    };
}

binop!(lua_binop_plus_int, lua_binop_plus, l, r, &l + &r);
binop!(lua_binop_minus_int, lua_binop_minus, l, r, &l - &r);
binop!(lua_binop_times_int, lua_binop_times, l, r, &l * &r);

binop!(lua_binop_div, lua_binop_div_panic, l, r, &l / &r);
binop!(
    lua_binop_floordiv,
    lua_binop_floordiv_panic,
    l,
    r,
    (&l / &r).floor()
);

binop!(
    lua_binop_mod_int,
    lua_binop_mod,
    l,
    r,
    l - &((&l / &r).floor()) * &r
);

pub fn lua_binop_and<'a>(l: &LV, r: &LV) -> LV {
    // lua considers false and nil to be falsy
    // everything else is truthy
    // and returns first operator if it is falsy, second otherwise
    if lua_truthy(&l) {
        return r.clone();
    } else {
        return l.clone();
    }
}

pub fn lua_binop_or<'a>(l: &LV, r: &LV) -> LV {
    // The result of the or operator is its first operand if it is not false;
    // otherwise, the result is its second operand
    if lua_truthy(&l) {
        return l.clone();
    } else {
        return r.clone();
    }
}

pub fn lua_binop_less<'a>(l: &LV, r: &LV) -> LV {
    match l {
        Num(n) => match r {
            Num(m) => {
                if n < m {
                    LV::LuaTrue
                } else {
                    LV::LuaFalse
                }
            }
            _ => panic!("FAILURE (need to implement lua-bubbling failure)"),
        },
        _ => {
            dbg!(l);
            dbg!(r);
            panic!("need binop impl");
        }
    }
}

pub fn lua_binop_greater<'a>(l: &LV, r: &LV) -> LuaResult {
    match (l, r) {
        (Num(n), Num(m)) => Ok(if n > m { LV::LuaTrue } else { LV::LuaFalse }),
        (LuaS(s), LuaS(t)) => Ok(if s > t { LV::LuaTrue } else { LV::LuaFalse }),
        _ => LuaErr::msg(format!("Attempt to compare {} and {}", l, r).as_str()),
    }
}

fn try_convert_i32(fv: LNum) -> Result<i32, String> {
    let f = match fv {
        LNum::Float(x) => x,
        LNum::Int(x) => x as f64,
    };

    let cast_result = f as i32;
    if f64::from(cast_result) != f {
        Err("Cannot cast to integer".to_string())
    } else {
        Ok(cast_result)
    }
}

use eval::LNum;
use numbers::unwrap_num_or_stringed_num;

use crate::eval::LuaErr;

/**
 * shifting logic to match lua
 * l << r when shift_left, l >> r when not shift_left
 **/

fn lua_shift_logic(l: i64, r: i64, shift_left: bool) -> Result<i64, LuaErr> {
    if r < 0 {
        // reverse the direction
        return lua_shift_logic(l, -r, !shift_left);
    }
    let downgraded_r = r as u32;
    let shift_result = if shift_left {
        // legit unsure of how this is supposed to work
        l.overflowing_shl(downgraded_r)
    } else {
        l.overflowing_shr(downgraded_r)
    };

    if shift_result.1 {
        // overflow
        return Ok(0);
    } else {
        return Ok(shift_result.0);
    }
}
pub fn lua_binop_lshift<'a>(l: &LV, r: &LV) -> LuaResult {
    match (unwrap_num_or_stringed_num(l), unwrap_num_or_stringed_num(r)) {
        (Ok(n), Ok(m)) => {
            // we need to confirm if we have integer representations
            let int_n = lua_coerce_int(&LV::Num(n))?;
            let int_m = lua_coerce_int(&LV::Num(m))?;
            let result = lua_shift_logic(int_n, int_m, true)?;
            return Ok(Num(LNum::Int(result)));
        }
        _ => LuaErr::msg(format!("Bitshift attempt for {} and {}", l, r).as_str()),
    }
}

pub fn lua_binop_rshift<'a>(l: &LV, r: &LV) -> LuaResult {
    match (unwrap_num_or_stringed_num(l), unwrap_num_or_stringed_num(r)) {
        (Ok(n), Ok(m)) => {
            // we need to confirm if we have integer representations
            let int_n = try_convert_i32(n);
            let int_m = try_convert_i32(m);
            match (int_n, int_m) {
                (Ok(x), Ok(y)) => Ok(Num((x >> y).into())),
                _ => LuaErr::msg(format!("No integer conversions for {} and {}", n, m).as_str()),
            }
        }
        _ => LuaErr::msg(format!("Bitshift attempt for {} and {}", l, r).as_str()),
    }
}

pub fn lua_binop_binor<'a>(l: &LV, r: &LV) -> LuaResult {
    match (unwrap_num_or_stringed_num(l), unwrap_num_or_stringed_num(r)) {
        (Ok(n), Ok(m)) => {
            // we need to confirm if we have integer representations
            let int_n = try_convert_i32(n);
            let int_m = try_convert_i32(m);
            match (int_n, int_m) {
                (Ok(x), Ok(y)) => Ok(Num((x | y).into())),
                _ => LuaErr::msg(format!("No integer conversions for {} and {}", n, m).as_str()),
            }
        }
        _ => LuaErr::msg(format!("Bitshift attempt for {} and {}", l, r).as_str()),
    }
}

pub fn lua_binop_binand<'a>(l: &LV, r: &LV) -> LuaResult {
    match (unwrap_num_or_stringed_num(l), unwrap_num_or_stringed_num(r)) {
        (Ok(n), Ok(m)) => {
            // we need to confirm if we have integer representations
            let int_n = try_convert_i32(n);
            let int_m = try_convert_i32(m);
            match (int_n, int_m) {
                (Ok(x), Ok(y)) => Ok(Num((x & y).into())),
                _ => LuaErr::msg(format!("No integer conversions for {} and {}", n, m).as_str()),
            }
        }
        _ => LuaErr::msg(format!("Bitshift attempt for {} and {}", l, r).as_str()),
    }
}

pub fn lua_binop_binxor<'a>(l: &LV, r: &LV) -> LuaResult {
    match (unwrap_num_or_stringed_num(l), unwrap_num_or_stringed_num(r)) {
        (Ok(n), Ok(m)) => {
            // we need to confirm if we have integer representations
            let int_n = try_convert_i32(n);
            let int_m = try_convert_i32(m);
            match (int_n, int_m) {
                (Ok(x), Ok(y)) => Ok(Num((x ^ y).into())),
                _ => LuaErr::msg(format!("No integer conversions for {} and {}", n, m).as_str()),
            }
        }
        _ => LuaErr::msg(format!("Bitshift attempt for {} and {}", l, r).as_str()),
    }
}

pub fn lua_binop_concat<'a>(l: &LV, r: &LV) -> LV {
    // We can concatenate two strings with the concatenation operator .. (two dots).
    // If any operand is a number, Lua converts this number to a string:
    let left_value = match &l {
        LuaS(s) => s.to_string(),
        Num(_n) => lua_fmt_for_print(&l),
        _ => {
            dbg!(l);
            panic!("invalid operand for concat");
        }
    };
    let right_value = match &r {
        LuaS(s) => s.to_string(),
        Num(_n) => lua_fmt_for_print(&r),
        _ => {
            dbg!(r);
            panic!("invalid operand for concat");
        }
    };
    return LuaS(left_value + &right_value);
}
