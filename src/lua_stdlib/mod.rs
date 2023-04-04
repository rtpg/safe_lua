use super::eval::LV;
use eval::LuaAllocator;
use natives::LuaArgs;
use std::collections::HashMap;
pub mod debug;
pub mod math;
pub mod string;
use eval::{LuaResult, LuaRunState};

pub fn lua_next(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    // this receives a table, and an index
    // this returns a "next index", and the value associated
    // the order is _undefined_, tho of course it needs to be "consistent"
    let _table = args.get_lv_arg(0)?;
    let _index = args.get_lv_arg_or_none(1);
    // TODO need to actually
    todo!();
}

pub fn lua_pairs(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let _table = args.get_lv_arg(0)?;
    // TODO support __pairs
    // returns the next function, the table t, and nil
    todo!();
    // return LV::LuaList(vec![
    //     // TODO avoid rebuilding the native func... somehow
    //     LV::NativeFunc { func: todo!() },
    //     t,
    //     LV::LuaNil,
    // ]);
}

pub fn stdlib<'a>(alloc: &mut LuaAllocator) -> HashMap<String, LV> {
    // all the standard lib stuff
    let mut lib = HashMap::new();
    lib.insert("debug".to_string(), debug_mod(alloc));
    lib.insert("string".to_string(), _mod(alloc));
    lib.insert("math".to_string(), math::math_pkg(alloc));
    lib.insert("string".to_string(), string::string_pkg(alloc));
    lib.insert("table".to_string(), _mod(alloc));
    lib.insert("io".to_string(), _mod(alloc));
    lib.insert("os".to_string(), _mod(alloc));
    lib.insert("coroutine".to_string(), _mod(alloc));
    lib.insert("debug".to_string(), _mod(alloc));
    lib.insert("utf8".to_string(), _mod(alloc));
    return lib;
}

fn _mod(s: &mut LuaAllocator) -> LV {
    return s.allocate_tbl();
}

pub fn debug_mod<'a>(s: &mut LuaAllocator) -> LV {
    return s.allocate_tbl();
}
