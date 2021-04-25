use datastructures::lua_ssetattr;
use eval::LuaAllocator;
use natives::LV;

pub fn package_mod(alloc: &mut LuaAllocator) -> LV {
    let mut result = alloc.allocate_tbl();

    // TODO properly implement package.path
    lua_ssetattr(&mut result, "path", LV::LuaS("NOT_IMPLEMENTED".to_string())).unwrap();
    lua_ssetattr(
        &mut result,
        "cpath",
        LV::LuaS("NOT_IMPLEMENTED".to_string()),
    )
    .unwrap();
    lua_ssetattr(
        &mut result,
        "cpath",
        LV::LuaS("NOT_IMPLEMENTED".to_string()),
    )
    .unwrap();
    return result;
}
