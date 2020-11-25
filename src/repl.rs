
use compile::utils::try_compile_block;
use eval;
use eval::exec::{
    exec_until_done
};

use std::io;
use std::io::Write;

const starting_snippet: &'static str = "
print 'Starting REPL';
";
pub fn do_repl() {
    let mut run_state = eval::initial_run_state(
	starting_snippet,
	"<repl>",
    );

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    
    let input = &mut String::new();

    loop {
	// run the state until it's done
	exec_until_done(&mut run_state);
	// put the prompt
	stdout.write(b">>> ");
	// we flush to make sure it shows up
	stdout.flush();

	// capture the input
	input.clear();
	stdin.read_line(input);

	// now we'll need to compile the input
	let compile_result = try_compile_block(input);
	match compile_result {
	    Ok(code_block) => {
		// load the code, it will run at the top of the loop
		run_state.load_code(code_block);
	    },
	    Err(err_msg) => {
		println!("compilation failed with {}", err_msg);
	    }
	}
    }
}
