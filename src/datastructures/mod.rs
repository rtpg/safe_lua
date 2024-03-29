use eval::lua_hash;
use eval::LuaErr;
use eval::LuaNative;
use eval::LV;

// pub fn lua_getattr(tlb: &LV, key: &LV) -> Result<LV, LuaErr> {}

pub fn lua_ssetattr(tbl: &mut LV, key: &str, value: LV) -> Result<(), LuaErr> {
    return lua_setattr(tbl, &LV::LuaS(key.to_string()), value);
}

pub fn lua_setattr(tbl: &mut LV, key: &LV, value: LV) -> Result<(), LuaErr> {
    let table_inner = match tbl {
        LV::LuaTable { v, .. } => v,
        _ => return LuaErr::msg("Called lua_setattr on a non-table"),
    };

    let mut table_data = table_inner.borrow_mut();
    table_data.insert(lua_hash(key), value);

    return Ok(());
}

pub fn lua_set_native(tbl: &mut LV, key: &str, func: LuaNative) -> Result<(), LuaErr> {
    let lua_func = LV::NativeFunc {
        name: key.to_string(),
        f: func,
        returns_multiple: false,
    };

    return lua_ssetattr(tbl, key, lua_func);
}

pub fn lua_getattr(tbl: &LV, key: &LV) -> Result<LV, LuaErr> {
    // try to do tbl[key], basically
    let hashed_key = lua_hash(key);
    let tbl_inner = match tbl {
        LV::LuaTable { v, .. } => v,
        _ => return LuaErr::msg("Called lua_getattr on a non-table"),
    };

    match tbl_inner.borrow().get(&hashed_key) {
        Some(v) => Ok(v.clone()),
        None => Ok(LV::LuaNil),
    }
}
