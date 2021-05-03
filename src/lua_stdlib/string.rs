use crate::{
    datastructures::lua_set_native,
    eval::{LuaAllocator, LuaResult, LuaRunState, LV},
    natives::LuaArgs,
};

pub fn lua_string_format(_s: &LuaRunState, args: &LuaArgs) -> LuaResult {
    let base_string = args.get_string_arg(0)?;
    // TODO implement string format actually
    print!("CALLED FORMAT WITH {0} and {1:?}", base_string, args.args);
    return Ok(LV::LuaS(base_string.to_string()));
}

pub fn string_pkg(s: &mut LuaAllocator) -> LV {
    let mut pkg = s.allocate_tbl();
    lua_set_native(&mut pkg, "format", lua_string_format).unwrap();
    return pkg;
}
