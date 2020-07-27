use eval::LuaExc;
use eval::LV;
use eval::LV::*;

pub fn lua_binop_eq(l: &LV, r: &LV) -> LV {
    if lua_binop_eq_impl(l, r) {
	return LuaTrue
    } else {
	return LuaFalse
    }
}
pub fn lua_binop_eq_impl(l: &LV, r: &LV) -> bool {

    match l {
	LuaNil => {
	    matches!(r, LuaNil)
	},
	Num(n) => {
	    match r {
		Num(m) => {
		    n == m
		},
		_ => false
	    } 
	},
	_ => {
	    dbg!(l);
	    dbg!(r);
	    panic!("need binop impl");
	}
    }
}

pub fn lua_exponent_eq<'a>(l: &LV, r: &LV) -> Result<LV, LuaExc<'a>> {
    match l {
	Num(ll) => {
	    match r {
		Num(rr) => Ok(Num(ll.powf(*rr))),
		_ => {
		    Err(
			LuaExc {
			    msg: "Type mismatch on exponent"
			}
		    )
		}
	    }
	},
	_ => {
	    Err(
		LuaExc {
		    msg: "Type mismatch on exponent"
		}
	    )
	}
    }
}
