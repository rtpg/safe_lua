use super::eval::LV;
use std::collections::HashMap;

pub fn stdlib<'a>() -> HashMap<String, LV> {
    // all the standard lib stuff
    let mut lib = HashMap::new();
    lib.insert("debug".to_string(), debug_mod());
    lib.insert("string".to_string(), _mod());
    lib.insert("math".to_string(), _mod());
    lib.insert("table".to_string(), _mod());
    lib.insert("io".to_string(), _mod());
    lib.insert("os".to_string(), _mod());
    lib.insert("coroutine".to_string(), _mod());
    return lib;
}

fn _mod() -> LV {
    return LV::LuaTable { v: HashMap::new() };
}

pub fn debug_mod<'a>() -> LV {
    return LV::LuaTable { v: HashMap::new() };
}
