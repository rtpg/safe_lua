extern crate safe_lua;
extern crate nom_locate;
use safe_lua::compile::Sourcemap;
use safe_lua::compile::SourcemapLoc;
use nom_locate::LocatedSpan;

#[test]
fn test_sourcemaps(){

    let loc_0 = LocatedSpan::new("Location Zero");
    let mut sm = Sourcemap::new("loc");

    sm.write_map(0, loc_0);

    assert_eq!(sm.get_location(0).unwrap(), SourcemapLoc::new(1));
}
