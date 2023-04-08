use eval::lua_hash;
use eval::{LuaResult, LV};

use super::LuaErr;

/**
 * Look up a property on a lua object
**/
pub fn getattr(object: &LV, property: &str) -> LuaResult {
    match object {
        LV::LuaNil => return LuaErr::msg("Attempted to index a nil value"),
        LV::LuaTable { v, .. } => {
            match v.borrow().get(&lua_hash(&LV::LuaS(property.to_string()))) {
                Some(elt) => return Ok(elt.clone()),
                None => return Ok(LV::LuaNil),
            }
        }
        _ => {
            return LuaErr::msg(format!("Do not know how to getattr {}", object));
            // dbg!(object);
            // return LuaErr::msg()
            // panic!("Unhandled getattr case")
        }
    }
}
