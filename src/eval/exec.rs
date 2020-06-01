use eval::LuaRunState;
use eval::LV;
use compile::BC;

pub enum ExecResult {
    Error(String),
    Yield(String),
}

pub fn exec_to_next_yield(s: &mut LuaRunState, yield_result: Option<u8>) -> ExecResult {
    // Move the machine forward until we hit the next yield
    loop {
        // let's get the next bytecode instruction to run
        let bc = s.current_frame.pc;
        let next_instruction = &s.current_frame.code.bytecode[bc];

	// we use this to confirm we're doing the right thing in the code
	// by default we expect the pc to move forward one instruction at a time
	//
	// But if we actually were trying to have the same value we'll allow it
	// by setting a new one here
	let mut intended_next_pc: Option<usize> = None;
        match next_instruction {
            BC::NOOP => {panic!("")},
            BC::PUSH_NIL => {panic!("")},
            BC::PUSH_FALSE => {panic!("")},
	    BC::PUSH_VAL_BY_NAME(val) => {
		match s.current_frame.env.values.get(val) {
		    Some(v) => s.current_frame.stack.values.push(v.clone()),
		    None => {
			dbg!(val);
			panic!("Key not found!")
		    }
		}
		
	    },
	    BC::PUSH_STRING(val) => {
		s.current_frame.stack.values.push(LV::LuaS(val.clone()))
	    },
	    BC::BUILD_LIST(len) => {
		// when we build a list we collect all the values
		// and then we will put it into a list
		let mut final_list: Vec<LV> = vec![];
		for _ in 0..*len {
		    final_list.push(
			s.current_frame.stack.values.pop().unwrap()
		    )
		};

		s.current_frame.stack.values.push(
		    LV::LuaList(final_list)
		)
	    },
	    BC::CALL_FUNCTION => {
		// TODO add a thing here to pop values cleanly
		// TODO all of it
		// When calling a function, we need to
		// get the arguments, then the function
		// TODO add a macro for pops
		let args = s.current_frame.stack.values.pop().unwrap();
		let m_func = s.current_frame.stack.values.pop().unwrap();
		// let's actually call the function
		match m_func {
		    LV::NativeFunc(f) => {
			let return_value = f(Some(args));
			s.current_frame.stack.values.push(return_value);
		    },
		    _ => {
			dbg!(m_func);
			panic!("Got a non-func to call!");
		    }
		}
	    },
            _ => {
                dbg!(next_instruction);
                panic!("Unhandled Bytecode");
            }
        }
	match intended_next_pc {
	    Some(pc) => {
		s.current_frame.pc = pc;
	    },
	    None => {
		// if we have not provided anything, just increment by 1
		s.current_frame.pc += 1;
	    }
	}
    }
}
