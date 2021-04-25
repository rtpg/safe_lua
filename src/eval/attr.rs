use eval::lua_hash;
use eval::{LuaResult, LV};

/**
 * Look up a property on a lua object
**/
pub fn getattr(object: &LV, property: &str) -> LuaResult {
    match object {
        LV::LuaNil => return Err("Attempted to index a nil value".to_string()),
        LV::LuaTable { v, .. } => {
            match v.borrow().get(&lua_hash(&LV::LuaS(property.to_string()))) {
                Some(elt) => return Ok(elt.clone()),
                None => return Ok(LV::LuaNil),
            }
        }
        _ => {
            dbg!(object);
            panic!("Unhandled getattr case")
        }
    }
}
