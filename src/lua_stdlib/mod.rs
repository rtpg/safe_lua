use super::eval::LV;
use eval::LuaAllocator;
use std::collections::HashMap;

pub fn stdlib<'a>(alloc: &mut LuaAllocator) -> HashMap<String, LV> {
    // all the standard lib stuff
    let mut lib = HashMap::new();
    lib.insert("debug".to_string(), debug_mod(alloc));
    lib.insert("string".to_string(), _mod(alloc));
    lib.insert("math".to_string(), _mod(alloc));
    lib.insert("table".to_string(), _mod(alloc));
    lib.insert("io".to_string(), _mod(alloc));
    lib.insert("os".to_string(), _mod(alloc));
    lib.insert("coroutine".to_string(), _mod(alloc));
    return lib;
}

fn _mod(s: &mut LuaAllocator) -> LV {
    return s.allocate_tbl();
}

pub fn debug_mod<'a>(s: &mut LuaAllocator) -> LV {
    return s.allocate_tbl();
}
