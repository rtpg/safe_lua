use natives::lua_fmt_for_print;
use natives::lua_truthy;
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
	LuaS(n) => {
	    match r {
		LuaS(m) => {
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

pub fn lua_binop_minus<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    match l {
	Num(n) => {
	    match r {
		Num(m) => {
		    LV::Num(n - m)
		},
		_ => panic!("FAILURE (need to implement lua-bubbling failure)")
	    } 
	},
	_ => {
	    dbg!(l);
	    dbg!(r);
	    panic!("need binop impl");
	}
    }
}

pub fn lua_binop_plus<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    match l {
	Num(n) => {
	    match r {
		Num(m) => {
		    LV::Num(n + m)
		},
		_ => panic!("FAILURE (need to implement lua-bubbling failure)")
	    } 
	},
	_ => {
	    dbg!(l);
	    dbg!(r);
	    panic!("need binop impl");
	}
    }
}


pub fn lua_binop_times<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    match l {
	Num(n) => {
	    match r {
		Num(m) => {
		    LV::Num(n * m)
		},
		_ => panic!("FAILURE (need to implement lua-bubbling failure)")
	    } 
	},
	_ => {
	    dbg!(l);
	    dbg!(r);
	    panic!("need binop impl");
	}
    }
}

pub fn lua_binop_div<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    match l {
	Num(n) => {
	    match r {
		Num(m) => {
		    LV::Num(n / m)
		},
		_ => panic!("FAILURE (need to implement lua-bubbling failure)")
	    } 
	},
	_ => {
	    dbg!(l);
	    dbg!(r);
	    panic!("need binop impl");
	}
    }
}

pub fn lua_binop_mod<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    match l {
	Num(n) => {
	    match r {
		Num(m) => {
		    // from "Programming in Lua"
		    // The following equation defines the modulo operator:
                    // a % b == a - ((a // b) * b)
		    // (floor division is //)

		    let floor_div = (n/m).floor();
		    LV::Num(n - ((floor_div) * m))
		},
		_ => panic!("FAILURE (need to implement lua-bubbling failure)")
	    } 
	},
	_ => {
	    dbg!(l);
	    dbg!(r);
	    panic!("need binop impl");
	}
    }
}

pub fn lua_binop_and<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    // lua considers false and nil to be falsy
    // everything else is truthy
    // and returns first operator if it is falsy, second otherwise
    if lua_truthy(&l) {
	return r.clone();
    } else {
	return l.clone();
    }
}

pub fn lua_binop_or<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    // The result of the or operator is its first operand if it is not false;
    // otherwise, the result is its second operand
    if lua_truthy(&l){
	return l.clone();
    } else {
	return r.clone();
    }
}

pub fn lua_binop_less<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    match l {
	Num(n) => {
	    match r {
		Num(m) => {
		    if n < m {
			LV::LuaTrue
		    } else {
			LV::LuaFalse
		    }
		},
		_ => panic!("FAILURE (need to implement lua-bubbling failure)")
	    } 
	},
	_ => {
	    dbg!(l);
	    dbg!(r);
	    panic!("need binop impl");
	}
    }
}

pub fn lua_binop_greater<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    match l {
	Num(n) => {
	    match r {
		Num(m) => {
		    if n > m {
			LV::LuaTrue
		    } else {
			LV::LuaFalse
		    }
		},
		_ => panic!("FAILURE (need to implement lua-bubbling failure)")
	    } 
	},
	_ => {
	    dbg!(l);
	    dbg!(r);
	    panic!("need binop impl");
	}
    }
}

pub fn lua_binop_concat<'a>(l: &LV<'a>, r: &LV<'a>) -> LV<'a> {
    // We can concatenate two strings with the concatenation operator .. (two dots).
    // If any operand is a number, Lua converts this number to a string:
    let left_value = match &l {
	LuaS(s) => s.to_string(),
	Num(n) => lua_fmt_for_print(&l),
	_ => {
	    dbg!(l);
	    panic!("invalid operand for concat");
	}
    };
    let right_value = match &r {
	LuaS(s) => s.to_string(),
	Num(n) => lua_fmt_for_print(&r),
	_ => {
	    dbg!(r);
	    panic!("invalid operand for concat");
	}
    };
    return LuaS(left_value + &right_value);
}
