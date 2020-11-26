use eval::LuaResult;
use natives::unwrap_single_arg;
use eval::LuaRunState;
use eval::LV::*;
use eval::LV;

pub fn unwrap_num_or_stringed_num(l: &LV) -> Result<f64, String> {
    // takes a number or a string and tries to unwrap it
    // used by tonumber, among other things
    match l {
	Num(n) => Ok(*n),
	LuaS(s) => {
	    match s.parse::<f64>() {
		Ok(result) => Ok(result),
		Err(_) => Err("not a number".to_string())
	    }
	},
	_ => Err("not a number".to_string())
    }
}

pub fn lua_tonumber<'a>(_s: &LuaRunState<'a>, args: Option<LV<'a>>) -> LuaResult<'a> {
    match unwrap_single_arg(args) {
	Some(arg) => {
	    match unwrap_num_or_stringed_num(&arg) {
		Ok(value) => Ok(Num(value)),
		Err(err) => Err(err),
	    }
	},
	_ => Err("wrong arity".to_string())
    }
}
