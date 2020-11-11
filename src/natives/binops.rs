use eval::LuaExc;
use eval::LV;
use eval::LV::*;

pub fn lua_binop_eq<'a>(l: &LV, r: &LV) -> LV<'a> {
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

pub fn lua_binop_leq<'a>(l: &LV, r: &LV) -> LV<'a> {
    if lua_binop_leq_impl(l, r) {
	return LuaTrue
    } else {
	return LuaFalse
    }
}
pub fn lua_binop_leq_impl(l: &LV, r: &LV) -> bool {

    match l {
	LuaNil => {
	    matches!(r, LuaNil)
	},
	Num(n) => {
	    match r {
		Num(m) => {
		    n <= m
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
pub fn lua_exponent_eq<'a>(l: &LV, r: &LV) -> Result<LV<'a>, LuaExc> {
    match l {
	Num(ll) => {
	    match r {
		Num(rr) => Ok(Num(ll.powf(*rr))),
		_ => {
		    let msg = format!(
			"Type mismatch on exponent. Received {0} and {1}",
			l,
			r
		    );
		    Err(
			LuaExc {
			    msg: msg
			}
		    )
		}
	    }
	},
	_ => {
	    let msg = format!(
		"Type mismatch on exponent. Received {0} and {1}",
		l,
		r
	    );
	    Err(
		LuaExc {
		    msg: msg 
		}
	    )
	}
    }
}
