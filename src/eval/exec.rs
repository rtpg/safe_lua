use compile::{JumpTarget, BC};
use eval::attr::getattr;
use eval::LuaNative;
use eval::LuaResult;
use eval::LuaRunState;
use eval::LuaValueStack;
use eval::DBG_POP_PUSH;
use eval::DBG_PRINT_INSTRUCTIONS;
use eval::LV;
use natives::binops::*;
use natives::lua_truthy;
use std::collections::HashMap;

#[derive(Debug)]
pub enum ExecResult {
    // there was some error in the process
    Error(String),
    // we yielded some result to be unwrapped
    Yield(String),
    // we are actually done and ther is nothing else to do
    Done(String),
}

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
    };
}

fn unwrap_or_vm_panic(s: &LuaRunState, val: LuaResult) -> LV {
    // open up a result, but panic if it failed
    match val {
        Err(err) => {
            vm_panic!(s, err);
        }
        Ok(result) => result,
    }
}

fn print_and_push(stack: &mut LuaValueStack, val: LV) {
    if DBG_POP_PUSH {
        println!("PUSH => {}", val);
    }
    stack.values.push(val);
}

pub fn exec_until_done<'b>(s: &'b mut LuaRunState) -> ExecResult {
    // run the state until we hit a done state
    // this will error out on yields etc
    loop {
        let result = exec_to_next_yield(s, None);
        if matches!(result, ExecResult::Done(_)) {
            return result;
        } else {
            dbg!(result);
            panic!("Not handled");
        }
    }
}

fn push<'a>(s: &mut LuaRunState, v: LV) {
    print_and_push(&mut s.current_frame.stack, v);
}

pub fn exec_step(s: &mut LuaRunState) -> Option<ExecResult> {
    // Move the machine forward until we hit the next yield
    macro_rules! pop {
        () => {
            match s.current_frame.stack.values.pop() {
                Some(v) => {
                    if DBG_POP_PUSH {
                        println!("POP => {}", v);
                    }
                    v
                }
                None => {
                    panic!("Popping an empty stack")
                }
            }
        };
    }

    // fail the system
    fn lua_fail(s: &mut LuaRunState, msg: &'static str) {
        println!("!! failed with message {}", msg);
        let f = &s.current_frame;
        let (line_no, lines) = f.code.sourcemap.get_lines_for_bytecode(f.pc);
        println!("!! failed on line {}", line_no);
        println!("--> {}", lines);
        panic!(msg);
    }

    fn peek(s: &LuaRunState) -> &LV {
        let len = s.current_frame.stack.values.len() - 1;
        match &s.current_frame.stack.values.get(len) {
            Some(v) => v, // TODO noclone
            None => {
                panic!("Peeking an empty stack")
            }
        }
    }

    let bytecode_length = s.current_frame.code.bytecode.len();
    // let's get the next bytecode instruction to run
    let bc = s.current_frame.pc;
    if bc >= bytecode_length {
        if bc == bytecode_length {
            return Some(ExecResult::Done("Execution finished".to_string()));
        } else {
            // if the execution gets here something has massively gone wrong
            return Some(ExecResult::Error("Bytecode out of bounds".to_string()));
        }
    }
    let next_instruction = &s.current_frame.code.bytecode[bc].clone();

    // we use this to confirm we're doing the right thing in the code
    // by default we expect the pc to move forward one instruction at a time
    //
    // But if we actually were trying to have the same value we'll allow it
    // by setting a new one here
    let mut intended_next_pc: Option<JumpTarget> = None;
    if DBG_PRINT_INSTRUCTIONS {
        dbg!(next_instruction);
    }
    match next_instruction {
        BC::NOOP => {
            // noop, just do nothing
        }
        BC::JUMP_FALSE(tgt) => {
            let check = pop!();
            if !lua_truthy(&check) {
                // jump to the target
                intended_next_pc = Some(tgt.clone());
            }
        }
        BC::ASSIGN_LOCAL_NIL(_) => {
            // for now, do nothing
            // basically later we'll want to implement scoping correctly here
        }
        BC::PUSH_NIL => {
            push(s, LV::LuaNil);
        }
        BC::PUSH_TRUE => {
            push(s, LV::LuaTrue);
        }
        BC::PUSH_FALSE => {
            push(s, LV::LuaFalse);
        }
        BC::PUSH_VAL_BY_NAME(val) => {
            let env = &s.current_frame.env;
            let v1 = env.get(val);
            match v1 {
                Some(v) => {
                    let v_clone = v.clone();
                    push(s, v_clone);
                }
                None => {
                    // unknown names are nil
                    push(s, LV::LuaNil);
                }
            }
        }
        BC::PUSH_NUMBER(n) => push(s, LV::Num(*n)),
        BC::PUSH_CODE_INDEX(n) => {
            let code_obj = s.current_frame.code.lookup_code_by_idx(*n);
            push(s, LV::Code(code_obj));
            // probably don't need this...
            push(s, LV::CodeIndex(*n));
        }
        BC::ASSIGN_NAME(n) => {
            let val = pop!();
            s.current_frame.env.set(n.to_string(), val);
        }
        BC::PUSH_NEW_TBL => push(s, LV::LuaTable { v: HashMap::new() }),
        BC::POP => {
            pop!();
        }
        BC::PUSH_NAMELIST(namelist, ellipsis) => {
            push(s, LV::NameList(namelist.to_vec(), *ellipsis))
        }
        BC::PUSH_STRING(val) => push(s, LV::LuaS(val.clone())),
        BC::BUILD_LIST(len) => {
            // when we build a list we collect all the values
            // and then we will put it into a list
            let mut final_list: Vec<LV> = vec![];
            for _ in 0..*len {
                final_list.push(pop!())
            }

            push(s, LV::LuaList(final_list))
        }
        BC::BINOP(binop) => {
            let r = pop!();
            let l = pop!();
            let result = match binop.as_ref() {
                "==" => lua_binop_eq(&l, &r),
                "~=" => lua_binop_neq(&l, &r),
                "^" => {
                    match lua_exponent_eq(&l, &r) {
                        Ok(v) => v,
                        Err(err) => {
                            // get the location
                            vm_panic!(s, err);
                        }
                    }
                }
                "<=" => lua_binop_leq(&l, &r),
                "-" => lua_binop_minus(&l, &r),
                "+" => lua_binop_plus(&l, &r),
                "*" => lua_binop_times(&l, &r),
                "/" => unwrap_or_vm_panic(s, lua_binop_div(&l, &r)),
                "//" => unwrap_or_vm_panic(s, lua_binop_floordiv(&l, &r)),
                "<" => lua_binop_less(&l, &r),
                ">" => unwrap_or_vm_panic(s, lua_binop_greater(&l, &r)),
                "<<" => unwrap_or_vm_panic(s, lua_binop_lshift(&l, &r)),
                ">>" => unwrap_or_vm_panic(s, lua_binop_rshift(&l, &r)),
                "|" => unwrap_or_vm_panic(s, lua_binop_binor(&l, &r)),
                "&" => unwrap_or_vm_panic(s, lua_binop_binand(&l, &r)),
                "~" => unwrap_or_vm_panic(s, lua_binop_binxor(&l, &r)),
                "and" => lua_binop_and(&l, &r),
                "or" => lua_binop_or(&l, &r),
                "%" => lua_binop_mod(&l, &r),
                ".." => lua_binop_concat(&l, &r),
                _ => {
                    dbg!(binop);
                    vm_panic!(s, "unknown binop");
                }
            };
            push(s, result);
        }
        BC::ASSIGN_LOCAL_FROM_EXPRLIST(name, sz) => match peek(s) {
            LV::LuaList(l) => match l.get(*sz) {
                Some(v) => {
                    let v_clone = v.clone();
                    s.current_frame.env.set(name.clone(), v_clone);
                }
                None => {
                    dbg!(l);
                    dbg!(sz);
                    panic!("Failed getting local from exprlist");
                }
            },
            other => {
                dbg!(other);
                panic!("Non-exprlist found");
            }
        },
        BC::BUILD_FUNCTION => {
            let namelist = pop!();
            let code_idx = pop!();
            let code = pop!();
            match (&namelist, &code_idx, &code) {
                (LV::NameList(nl, ellipsis), LV::CodeIndex(idx), LV::Code(code)) => push(
                    s,
                    LV::LuaFunc {
                        code_idx: *idx,
                        code: code.clone(),
                        args: nl.to_vec(),
                        ellipsis: *ellipsis,
                        parent_env: s.current_frame.env.clone(),
                    },
                ),
                _ => {
                    dbg!(namelist);
                    dbg!(code_idx);
                    panic!("Could not build function, inconsistent stack");
                }
            }
        }
        BC::ASSIGN_LOCAL_FROM_TOP_OF_STACK(name) => {
            let val = pop!();

            s.current_frame.env.set(name.to_string(), val);
        }
        BC::EXTRACT_FROM_EXPRLIST(idx) => {
            // ASSUME: exprlist top of stack
            // ASSUME: size exprlist >= idx
            let peeked_value = peek(s);
            match peeked_value {
                LV::LuaList(vec) => match vec.get(*idx) {
                    Some(val) => {
                        let val_clone = val.clone();
                        push(s, val_clone);
                    }
                    None => {
                        panic!("exprlist too small");
                    }
                },
                other => {
                    dbg!(other);
                    panic!("Wrong kind of object on the stack")
                }
            }
        }
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
                LV::NativeFunc { name: _name, f } => {
                    handle_native_call(s, f, args);
                }
                LV::LuaFunc { .. } => {
                    // we queue up the new frame to continue executing
                    // we also want to indicate that we want to shift the pc forward one
                    // the return will set the return value on the satck of the frame later
                    s.enter_function_call(m_func, args);
                    intended_next_pc = Some(JumpTarget::InnerFuncCall());
                }
                _ => {
                    dbg!(m_func);
                    lua_fail(s, "Got a non-func to call");
                }
            }
        }
        BC::PANIC(err_msg) => {
            dbg!(err_msg);
            vm_panic!(s, "Lua panic opcode");
        }
        BC::JUMP(new_location) => {
            intended_next_pc = Some(new_location.clone());
        }
        BC::RETURN_VALUE => {
            let return_value = pop!();
            s.return_from_funccall(return_value);
        }
        BC::UNOP(code) => {
            let value = pop!();
            match code.as_str() {
                "-" => match value {
                    LV::Num(n) => push(s, LV::Num(-n)),
                    _ => {
                        dbg!(value);
                        vm_panic!(s, "Attempted arithmetic on a non-number");
                    }
                },
                "not" => {
                    if lua_truthy(&value) {
                        push(s, LV::LuaFalse);
                    } else {
                        push(s, LV::LuaTrue);
                    }
                }
                _ => {
                    dbg!(code);
                    vm_panic!(s, "Unknown op-code");
                }
            }
        }
        BC::DOT_ACCESS => {
            let property_obj = pop!();
            let object = pop!();
            let property;
            match property_obj {
                LV::LuaS(s) => {
                    property = s;
                }
                _ => {
                    dbg!(property_obj);
                    vm_panic!(s, "Tried to use a non-string on a dot access");
                }
            };
            match getattr(&object, &property) {
                Ok(v) => push(s, v),
                Err(e) => {
                    dbg!(object);
                    dbg!(property);
                    vm_panic!(s, "Tried to lookup a property but failed");
                }
            }
        }
        _ => {
            dbg!(next_instruction);
            vm_panic!(s, "Unhandled Bytecode");
        }
    }
    match intended_next_pc {
        Some(tgt) => match tgt {
            JumpTarget::CodeLoc(pc) => {
                s.current_frame.pc = pc;
            }
            JumpTarget::JumpTable(loc) => match s.current_frame.code.jump_target.get(loc) {
                Some(Some(n)) => {
                    s.current_frame.pc = *n;
                }
                _ => {
                    panic!("Failed jump table lookup");
                }
            },
            JumpTarget::InnerFuncCall() => {}
        },
        None => {
            // if we have not provided anything, just increment by 1
            s.current_frame.pc += 1;
        }
    }
    // "base case" when we haven't finished stuff up
    return None;
}
pub fn exec_to_next_yield<'a, 'b>(s: &'b mut LuaRunState, _yield_result: Option<u8>) -> ExecResult {
    loop {
        let maybe_result = exec_step(s);
        match maybe_result {
            Some(result) => return result,
            None => {}
        }
    }
}

fn handle_native_call<'a, 'b>(s: &'b mut LuaRunState, f: LuaNative, args: LV) {
    // call a native function, and do all the proper error handling from it
    let return_value = f(s, Some(args));
    match return_value {
        Ok(result) => push(s, result),
        Err(err) => {
            vm_panic!(s, err);
        }
    }
}
