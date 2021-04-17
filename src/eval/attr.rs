use eval::{LuaResult, LV};

/**
 * Look up a property on a lua object
**/
pub fn getattr(object: &LV, property: &str) -> LuaResult {
    match object {
        LV::LuaNil => return Err("Attempted to index a nil value".to_string()),
        _ => {
            dbg!(object);
            panic!("Unhandled getattr case")
        }
    }
}
