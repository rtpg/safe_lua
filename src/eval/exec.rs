use eval::LuaRunState;
use eval::LV;
use eval::LuaValueStack;
use compile::{
    BC,
    JumpTarget
};
use natives::binops::*;

pub enum ExecResult {
    Error(String),
    Yield(String),
}


fn print_and_push(stack: &mut LuaValueStack, val: LV) {
    println!("PUSH => {}", val);
    stack.values.push(val);
}

macro_rules! vm_panic {
    ($s: expr, $err: expr) => {
	println!("Lua VM crashed at");
	dbg!($s.current_frame.code.sourcemap.get_location($s.current_frame.pc));
	println!("With:");
	dbg!($err);
	panic!("VM CRASH");
    }
}

pub fn exec_to_next_yield(s: &mut LuaRunState, _yield_result: Option<u8>) -> ExecResult {
    // Move the machine forward until we hit the next yield
    macro_rules! pop {
	() => {
	    match s.current_frame.stack.values.pop() {
		Some(v) => {
		    println!("POP => {}", v);
		    v
		}
		None => {panic!("Popping an empty stack")}
	    }
	}
    }

    fn push(s: &mut LuaRunState, v: LV) {
	print_and_push(&mut s.current_frame.stack, v);
    }
    
    fn peek<'a>(s: &'a LuaRunState<'_>) -> &'a LV {
	let len = s.current_frame.stack.values.len() - 1;
	match &s.current_frame.stack.values.get(len) {
	    Some(v) => v, // TODO noclone
	    None => {panic!("Peeking an empty stack")}
	}
    }

    loop {
        // let's get the next bytecode instruction to run
        let bc = s.current_frame.pc;
        let next_instruction = &s.current_frame.code.bytecode[bc].clone();

	// we use this to confirm we're doing the right thing in the code
	// by default we expect the pc to move forward one instruction at a time
	//
	// But if we actually were trying to have the same value we'll allow it
	// by setting a new one here
	let mut intended_next_pc: Option<JumpTarget> = None;
	dbg!(next_instruction);
        match next_instruction {
            BC::NOOP => {
		// noop, just do nothing
	    },
	    BC::JUMP_FALSE(tgt) => {
		let check = pop!();
		match check {
		    LV::LuaFalse => {
			// jump to the table
			intended_next_pc = Some(tgt.clone());
		    },
		    _ => {
			// we will not jump here and just let the system go to the next spot
		    }
		}
	    }
            BC::PUSH_NIL => {
		push(s, LV::LuaNil);
	    },
            BC::PUSH_FALSE => {
		push(s, LV::LuaFalse);
	    },
	    BC::PUSH_VAL_BY_NAME(val) => {
		let v1 = s.current_frame.env.values.get(val);
		match v1 {
		    Some(v) => {
			let v_clone = v.clone();
			push(s, v_clone);
		    },
		    None => {
			dbg!(val);
			panic!("Key not found!")
		    }
		}
		
	    },
	    BC::PUSH_NUMBER(n) => {
		push(s, LV::Num(*n))
	    },
	    BC::PUSH_CODE_INDEX(n) => {
		push(s, LV::CodeIndex(*n))
	    },
	    BC::ASSIGN_NAME(n) => {
		let val = pop!();
		s.current_frame.env.values.insert(
		    n.to_string(),
		    val
		);
	    },
	    BC::POP => {
		pop!();
	    },
	    BC::PUSH_NAMELIST(namelist, ellipsis) => {
		push(s, LV::NameList(namelist.to_vec(), *ellipsis))
	    },
	    BC::PUSH_STRING(val) => {
		push(s, LV::LuaS(val.clone()))
	    },
	    BC::BUILD_LIST(len) => {
		// when we build a list we collect all the values
		// and then we will put it into a list
		let mut final_list: Vec<LV> = vec![];
		for _ in 0..*len {
		    final_list.push(
			pop!()
		    )
		};

		push(
		    s,
		    LV::LuaList(final_list)
		)
	    },
	    BC::BINOP(binop) => {
                let r = pop!();
                let l = pop!();
		let result = match binop.as_ref() {
		    "==" => lua_binop_eq(&l, &r),
		    "^" => {
			match lua_exponent_eq(&l, &r){
			    Ok(v) => v,
			    Err(err) => {
				// get the location
				
				vm_panic!(s, err);
			    }
			}
		    },
		    _ => {
			dbg!(binop);
			panic!("Unknown binop");
		    }
		};
		push(s, result);
	    },
	    BC::ASSIGN_LOCAL_FROM_EXPRLIST(name, sz) => {
		match peek(s) {
		    LV::LuaList(l) => {
			match l.get(*sz) {
			    Some(v) => {
				let v_clone = v.clone();
				s.current_frame.env.values.insert(
				    name.clone(),
				    v_clone
				);
			    },
			    None => {
				dbg!(l);
				dbg!(sz);
				panic!("Failed getting local from exprlist");
			    }
			}
		    },
		    other => {
			dbg!(other);
			panic!("Non-exprlist found");
		    }
		}
	    },
	    BC::BUILD_FUNCTION => {
		let namelist = pop!();
		let code_idx = pop!();
		match (&namelist, &code_idx) {
		    (LV::NameList(nl, ellipsis), LV::CodeIndex(idx)) => {
			push(
			    s,
			    LV::LuaFunc {
				code_idx: *idx,
				args: nl.to_vec(),
				ellipsis: *ellipsis
			    }
			)
		    },
		    _ => {
			dbg!(namelist);
			dbg!(code_idx);
			panic!("Could not build function, inconsistent stack");
		    }
		}
	    },
	    BC::ASSIGN_LOCAL_FROM_TOP_OF_STACK(name) => {
		let val = pop!();
		s.current_frame.env.values.insert(
		    name.to_string(),
		    val
		);
	    },
	    BC::EXTRACT_FROM_EXPRLIST(idx) => {
		// ASSUME: exprlist top of stack
		// ASSUME: size exprlist >= idx
		let peeked_value = peek(s);
		match peeked_value {
		    LV::LuaList(vec) => {
			match vec.get(*idx) {
			    Some(val) => {
				let val_clone = val.clone();
				push(s, val_clone);
			    },
			    None => {
				panic!("exprlist too small");
			    }
			}
		    },
		    other => {
			dbg!(other);
			panic!("Wrong kind of object on the stack")
		    }
		}
	    },
	    BC::CALL_FUNCTION => {
		// TODO add a thing here to pop values cleanly
		// TODO all of it
		// When calling a function, we need to
		// get the arguments, then the function
		// TODO add a macro for pops
		let args = pop!();
		let m_func = pop!();
		// let's actually call the function
		match m_func {
		    LV::NativeFunc {name: _name, f} => {
			let return_value = f(s, Some(args));
			push(s, return_value);
		    },
		    _ => {
			dbg!(m_func);
			panic!("Got a non-func to call!");
		    }
		}
	    },
	    BC::PANIC(err_msg) => {
		dbg!(err_msg);
		panic!("Lua panic opcode");
	    },
            _ => {
                dbg!(next_instruction);
                panic!("Unhandled Bytecode");
            }
        }
	match intended_next_pc {
	    Some(tgt) => {
		match tgt {
		    JumpTarget::CodeLoc(pc) => {
			s.current_frame.pc = pc;
		    },
		    JumpTarget::JumpTable(loc) => {
			match s.current_frame.code.jump_target.get(loc) {
			    Some(Some(n)) => {
				s.current_frame.pc = *n;
			    },
			    _ => {
				panic!("Failed jump table lookup");
			    }
			}
		    }
		}
	    },
	    None => {
		// if we have not provided anything, just increment by 1
		s.current_frame.pc += 1;
	    }
	}
    }
}
