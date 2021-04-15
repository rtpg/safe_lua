extern crate nom_locate;
extern crate safe_lua;
use nom_locate::LocatedSpan;
use safe_lua::compile::Sourcemap;
use safe_lua::compile::SourcemapLoc;

#[test]
fn test_sourcemaps() {
    let loc_0 = LocatedSpan::new("Location Zero");
    let mut sm = Sourcemap::new("loc");

    sm.write_map(0, loc_0);

    assert_eq!(sm.get_location(0).unwrap(), SourcemapLoc::new(1));
}
