use compile::CodeObj;
use std::convert::TryInto;

pub fn display_code_block<'a>(obj: &CodeObj){
    // improvement possibilities
    // display the source file
    // display inner function stuff (like when building up)
    println!("START<<<<<");
    println!("MAIN BLOCK");
    // this value is to keep track of which line ew have printed so far
    let mut prev_line = 0;
    for (bc_idx, elt) in obj.bytecode.iter().enumerate() {
	let loc = obj.sourcemap.get_location(bc_idx);

	
	let loc_line: usize = match loc {
	    Some(l) => l.location_line().try_into().unwrap(),
	    None => 0 // 0 should make the line not show up
	};
	let display_line = loc_line > prev_line;
	if display_line {
	    // this is a new line so we'll show it
	    // (this doesn't handle multi-line expressions yet)
	    prev_line = loc_line;
	    // THERE'S SOME OFF BY ONE SOMEWHERE
	    let line = obj.sourcemap.get_line(loc_line - 1);
	    println!(" {:?} -- {}", elt, line);
	} else {
	    // we just print out the element
	    println!(" {:?}", elt);
	}
    }
    for (idx, inner) in obj.inner_code.iter().enumerate() {
	println!("INNER BLOCK {}", idx);
	display_code_block(&inner);
    }
    println!(">>>>>END");
}
