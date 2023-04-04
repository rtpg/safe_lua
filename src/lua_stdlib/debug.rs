use eval::{LuaAllocator, LV};

pub fn debug_pkg(s: &mut LuaAllocator) -> LV {
    let mut pkg = s.allocate_tbl();
    return pkg;
}
