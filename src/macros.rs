// cross module macros go here
#[macro_export]
macro_rules! vm_panic {
    ($s: expr, $err: expr) => {
	println!("!! Lua VM crash");
	// TODO unwind the entire frame stack here for more information
	let f = &$s.current_frame;
	let (line_no, lines) = f.code.sourcemap.get_lines_for_bytecode(f.pc);
	println!("!! failed on line {} of {}", line_no, $s.file_path);
	println!("--> {}", lines);
	println!("With:");
	dbg!($err);
	panic!("VM CRASH");
    }
}
