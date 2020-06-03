use super::eval::LV;
use std::collections::HashMap;




pub fn stdlib() -> HashMap<String, LV> {
    // all the standard lib stuff
    let mut lib = HashMap::new();
    lib.insert(
	"debug".to_string(),
	debug_mod(),
    );
    return lib;
	
}


pub fn debug_mod() -> LV {
    return LV::LuaTable {
	v: HashMap::new()
    };
}
