use compile::{JumpTarget, BC};
use datastructures::lua_getattr;
use eval::attr::getattr;
use eval::lua_hash;
use eval::LuaNative;
use eval::LuaResult;
use eval::LuaRunState;
use eval::LuaValueStack;
use eval::DBG_POP_PUSH;
use eval::DBG_PRINT_INSTRUCTIONS;
use eval::LV;
use natives::binops::*;
use natives::lua_coerce_lnum;
use natives::lua_truthy;
use natives::LuaArgs;

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

    /**
     * Print the current stack on the top frame (for debugging)
     **/
    fn lua_dbg_stack(s: &LuaRunState) {
        println!("STACK NOW IS");
        for value in &s.current_frame.stack.values {
            println!("{:?}", value);
        }
        println!("======");
    }

    // fail the system
    fn lua_fail(s: &mut LuaRunState, msg: &'static str) {
        println!("!! failed with message {}", msg);
        let f = &s.current_frame;
        let (line_no, lines) = f.code.sourcemap.get_lines_for_bytecode(f.pc);
        println!("!! failed on line {}", line_no);
        println!("--> {}", lines);
        panic!("{}", msg);
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
        BC::PUSH_NEW_TBL => {
            let new_tbl = s.allocate_tbl();
            push(s, new_tbl);
        }
        BC::ASSIGN_TABLE_VALUE => {
            // print!("ASSIGN TABLE VALUE");
            // lua_dbg_stack(s);
            let value = pop!();
            let key = pop!();
            let tbl_wrapped = pop!();
            let (tbl, id) = match tbl_wrapped {
                LV::LuaTable { v, id } => (v, id),
                _ => {
                    dbg!(tbl_wrapped);
                    vm_panic!(s, "Tried to assign table value to non-table");
                }
            };
            // hash the key to insert into the table
            let hash = lua_hash(&key);
            tbl.borrow_mut().insert(hash, value);
            // TODO probably better way of doing this than rebuilding this object
            // all the time
            // this is inheriting the same id s it's the "same" table
            push(s, LV::LuaTable { v: tbl, id: id });
            // vm_panic!(s, "TODO: implement assign table value");
        }
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
            let result: LuaResult = match binop.as_ref() {
                "==" => Ok(lua_binop_eq(&l, &r)),
                "~=" => Ok(lua_binop_neq(&l, &r)),
                "^" => lua_exponent_eq(&l, &r),
                "<=" => Ok(lua_binop_leq(&l, &r)),
                "-" => Ok(lua_binop_minus(&l, &r)),
                "+" => lua_binop_plus_int(&l, &r),
                "*" => Ok(lua_binop_times(&l, &r)),
                "/" => lua_binop_div(&l, &r),
                "//" => lua_binop_floordiv(&l, &r),
                "<" => Ok(lua_binop_less(&l, &r)),
                ">" => lua_binop_greater(&l, &r),
                "<<" => lua_binop_lshift(&l, &r),
                ">>" => lua_binop_rshift(&l, &r),
                "|" => lua_binop_binor(&l, &r),
                "&" => lua_binop_binand(&l, &r),
                "~" => lua_binop_binxor(&l, &r),
                "and" => Ok(lua_binop_and(&l, &r)),
                "or" => Ok(lua_binop_or(&l, &r)),
                "%" => Ok(lua_binop_mod(&l, &r)),
                ".." => Ok(lua_binop_concat(&l, &r)),
                _ => {
                    dbg!(binop);
                    vm_panic!(s, "unknown binop");
                }
            };
            match result {
                Ok(value) => push(s, value),
                Err(err) => s.raise_error(err),
            }
        }
        BC::ASSIGN_LOCAL_FROM_EXPRLIST(name, sz) => {
            let to_assign = match peek(s) {
                // if we have an exprlist we use that
                LV::LuaList(l) => match l.get(*sz) {
                    Some(v) => v,
                    None => {
                        dbg!(l);
                        dbg!(sz);
                        panic!("Failed getting local from exprlist");
                    }
                },
                other => {
                    // if the sz is 0 then we can handle that "directly"
                    // (since we're getting the right kind of object here)
                    if *sz == 0 {
                        other
                    } else {
                        lua_dbg_stack(s);
                        dbg!(other);
                        panic!("Non-exprlist found");
                    }
                }
            };

            let v_clone = to_assign.clone();
            s.current_frame.env.set(name.clone(), v_clone);
        }
        BC::BUILD_FUNCTION => {
            let namelist = pop!();
            let code_idx = pop!();
            let code = pop!();
            let new_id = s.alloc.get_id();
            match (&namelist, &code_idx, &code) {
                (LV::NameList(nl, ellipsis), LV::CodeIndex(idx), LV::Code(code)) => push(
                    s,
                    LV::LuaFunc {
                        id: new_id,
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
            let extracted_value = match peeked_value {
                LV::LuaList(vec) => match vec.get(*idx) {
                    Some(val) => val,
                    None => {
                        vm_panic!(s, "exprlist too small");
                    }
                },
                other => {
                    if *idx == 0 {
                        other
                    } else {
                        dbg!(other);
                        vm_panic!(s, "Wrong kind of object on the stack");
                    }
                }
            };
            let ev_clone = extracted_value.clone();
            push(s, ev_clone);
        }
        BC::RUN_NATIVE_FUNC => {
            let args = pop!();
            let m_func = pop!();
            let lua_args = match args {
                LV::LuaList(params) => LuaArgs { args: params },
                _ => panic!("Received non-list as param in native call"),
            };

            match m_func {
                LV::NativeFunc { f, .. } => {
                    let return_value = f(s, &lua_args);
                    match return_value {
                        Ok(result) => match result {
                            LV::LuaList(elts) => s.return_multi_from_funccall(elts),
                            _ => s.return_from_funccall(result),
                        },
                        Err(err) => s.raise_error(err),
                    }
                }
                _ => {
                    dbg!(m_func);
                    lua_fail(s, "Got a non-func to call");
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
            s.enter_function_call(m_func, args, false);
            intended_next_pc = Some(JumpTarget::InnerFuncCall());
        }
        BC::PANIC(err_msg) => {
            dbg!(err_msg);
            vm_panic!(s, "Lua panic opcode");
        }
        BC::JUMP(new_location) => {
            intended_next_pc = Some(new_location.clone());
        }
        BC::UNWRAP_RETURN => {
            // when we get a list return, unwrap it cleanly
            let elt = pop!();
            let to_push = match elt {
                LV::LuaList(v) => {
                    // unwrap
                    v[0].clone()
                }
                other => other,
            };
            push(s, to_push);
        }
        BC::RETURN_NONE => {
            s.return_from_funccall(LV::LuaNil);
        }
        BC::RETURN_VALUE => {
            let return_value = pop!();
            s.return_from_funccall(return_value);
        }
        BC::RETURN_MULTI(args) => {
            let mut to_return: Vec<LV> = vec![];
            for _ in 0..*args {
                to_return.push(pop!());
            }
            to_return.reverse();
            s.return_multi_from_funccall(to_return);
        }
        BC::UNOP(code) => {
            let value = pop!();
            match code.as_str() {
                "-" => match lua_coerce_lnum(&value) {
                    Ok(v) => push(s, LV::Num(-v)),
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
                    dbg!(e);
                    vm_panic!(s, "Tried to lookup a property but failed");
                }
            }
        }

        BC::PUSH_ELLIPSIS => {
            todo!()
        }
        BC::PUSH_NUMERAL(_) => {
            todo!()
        }
        BC::SET_LOCAL => {
            todo!()
        }
        BC::ARRAY_ACCESS => {
            // a[b]
            // stack is in [a b] order
            let key = pop!();
            let arr = pop!();
            match lua_getattr(&arr, &key) {
                Ok(result) => push(s, result),
                Err(err) => {
                    dbg!(err);
                    vm_panic!(s, "Failed on array access");
                }
            }
        }
        BC::GOTO(_) => {
            todo!()
        }
        BC::ASSIGN_ARR_ACCESS() => {
            todo!()
        }
        BC::ASSIGN_DOT_ACCESS(_) => {
            todo!()
        }
        BC::CALL_METHOD => {
            todo!()
        }
        BC::JUMP_TRUE(_) => {
            todo!()
        }
        BC::FOR_LOOP_INIT => {
            todo!()
        }
        BC::FOR_LOOP_CHECK_CONDITION(_) => {
            todo!()
        }
        BC::FOR_IN_LOOP_INIT => {
            todo!()
        }
        BC::BREAK => {
            todo!()
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

fn handle_native_call<'a, 'b>(
    s: &'b mut LuaRunState,
    f: LuaNative,
    args: LV,
    returns_multiple: bool,
) {
    // call a native function, and do all the proper error handling from it
    let lua_args = match args {
        LV::LuaList(params) => LuaArgs { args: params },
        _ => panic!("Received non-list as param in native call"),
    };

    let return_value = f(s, &lua_args);
    match return_value {
        Ok(result) => {
            if returns_multiple && !matches!(result, LV::LuaList(_)) {
                panic!("a native function returning multiple results must return a LuaList")
            }
            push(s, result);
        }
        Err(err) => {
            vm_panic!(s, err);
        }
    }
}
