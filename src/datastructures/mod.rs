use eval::lua_hash;
use eval::LuaErr;
use eval::LV;

// pub fn lua_getattr(tlb: &LV, key: &LV) -> Result<LV, LuaErr> {}

pub fn lua_ssetattr(tbl: &mut LV, key: &str, value: LV) -> Result<(), LuaErr> {
    return lua_setattr(tbl, &LV::LuaS(key.to_string()), value);
}

pub fn lua_setattr(tbl: &mut LV, key: &LV, value: LV) -> Result<(), LuaErr> {
    let table_inner = match tbl {
        LV::LuaTable { v, .. } => v,
        _ => return Err("Called lua_setattr on a non-table".to_string()),
    };

    let mut table_data = table_inner.borrow_mut();
    table_data.insert(lua_hash(key), value);

    return Ok(());
}
