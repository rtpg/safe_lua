use super::eval::LV;
use std::collections::HashMap;




pub fn stdlib<'a>() -> HashMap<String, LV<'a>> {
    // all the standard lib stuff
    let mut lib = HashMap::new();
    lib.insert(
	"debug".to_string(),
	debug_mod(),
    );
    return lib;
	
}


pub fn debug_mod<'a>() -> LV<'a> {
    return LV::LuaTable {
	v: HashMap::new()
    };
}
