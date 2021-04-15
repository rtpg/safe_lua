use eval::LuaExc;
use eval::LuaResult;
use eval::LV;
use eval::LV::*;
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
        LuaTable { .. } => todo!(),
        LuaFunc { .. } => todo!(),
        CodeIndex(_) => todo!(),
        Code(_) => todo!(),
        NameList(_, _) => todo!(),
        NativeFunc { .. } => todo!(),
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
pub fn lua_exponent_eq<'a>(l: &LV, r: &LV) -> Result<LV, LuaExc> {
    match l {
        Num(ll) => match r {
            Num(rr) => Ok(Num(ll.powf(*rr))),
            _ => {
                let msg = format!("Type mismatch on exponent. Received {0} and {1}", l, r);
                Err(LuaExc { msg: msg })
            }
        },
        _ => {
            let msg = format!("Type mismatch on exponent. Received {0} and {1}", l, r);
            Err(LuaExc { msg: msg })
        }
    }
}

pub fn lua_binop_minus<'a>(l: &LV, r: &LV) -> LV {
    match l {
        Num(n) => match r {
            Num(m) => LV::Num(n - m),
            _ => panic!("FAILURE (need to implement lua-bubbling failure)"),
        },
        _ => {
            dbg!(l);
            dbg!(r);
            panic!("need binop impl");
        }
    }
}

pub fn lua_binop_plus<'a>(l: &LV, r: &LV) -> LV {
    match l {
        Num(n) => match r {
            Num(m) => LV::Num(n + m),
            _ => panic!("FAILURE (need to implement lua-bubbling failure)"),
        },
        _ => {
            dbg!(l);
            dbg!(r);
            panic!("need binop impl");
        }
    }
}

pub fn lua_binop_times<'a>(l: &LV, r: &LV) -> LV {
    match l {
        Num(n) => match r {
            Num(m) => LV::Num(n * m),
            _ => panic!("FAILURE (need to implement lua-bubbling failure)"),
        },
        _ => {
            dbg!(l);
            dbg!(r);
            panic!("need binop impl");
        }
    }
}

pub fn lua_binop_div<'a>(l: &LV, r: &LV) -> LuaResult {
    match (l, r) {
        (Num(n), Num(m)) => Ok(LV::Num(n / m)),
        _ => Err(format!("Cant divide {} and {}", l, r)),
    }
}

pub fn lua_binop_floordiv<'a>(l: &LV, r: &LV) -> LuaResult {
    match (l, r) {
        (Num(n), Num(m)) => {
            let result = n / m;
            Ok(LV::Num(result.floor()))
        }
        _ => Err(format!("Cant divide {} and {}", l, r)),
    }
}

pub fn lua_binop_mod<'a>(l: &LV, r: &LV) -> LV {
    match l {
        Num(n) => {
            match r {
                Num(m) => {
                    // from "Programming in Lua"
                    // The following equation defines the modulo operator:
                    // a % b == a - ((a // b) * b)
                    // (floor division is //)

                    let floor_div = (n / m).floor();
                    LV::Num(n - ((floor_div) * m))
                }
                _ => panic!("FAILURE (need to implement lua-bubbling failure)"),
            }
        }
        _ => {
            dbg!(l);
            dbg!(r);
            panic!("need binop impl");
        }
    }
}

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
        _ => Err(format!("Attempt to compare {} and {}", l, r)),
    }
}

fn try_convert_i32(f: f64) -> Result<i32, String> {
    let cast_result = f as i32;
    if f64::from(cast_result) != f {
        Err("Cannot cast to integer".to_string())
    } else {
        Ok(cast_result)
    }
}

use numbers::unwrap_num_or_stringed_num;

pub fn lua_binop_lshift<'a>(l: &LV, r: &LV) -> LuaResult {
    match (unwrap_num_or_stringed_num(l), unwrap_num_or_stringed_num(r)) {
        (Ok(n), Ok(m)) => {
            // we need to confirm if we have integer representations
            let int_n = try_convert_i32(n);
            let int_m = try_convert_i32(m);
            match (int_n, int_m) {
                (Ok(x), Ok(y)) => Ok(Num((x << y).into())),
                _ => Err(format!("No integer conversions for {} and {}", n, m)),
            }
        }
        _ => Err(format!("Bitshift attempt for {} and {}", l, r)),
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
                _ => Err(format!("No integer conversions for {} and {}", n, m)),
            }
        }
        _ => Err(format!("Bitshift attempt for {} and {}", l, r)),
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
                _ => Err(format!("No integer conversions for {} and {}", n, m)),
            }
        }
        _ => Err(format!("Bitshift attempt for {} and {}", l, r)),
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
                _ => Err(format!("No integer conversions for {} and {}", n, m)),
            }
        }
        _ => Err(format!("Bitshift attempt for {} and {}", l, r)),
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
                _ => Err(format!("No integer conversions for {} and {}", n, m)),
            }
        }
        _ => Err(format!("Bitshift attempt for {} and {}", l, r)),
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
