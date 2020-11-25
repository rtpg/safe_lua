/**
 * This file contains the tooling to turn
 * a bunch of lex tokens into a list of bytecode
 * operations
 */
pub mod display;

use std::convert::TryInto;
use std::rc::Rc;
use nom_locate::LocatedSpan;
use ast;
use lex;
use std::collections::HashMap;
use ast::Name;

// jump target shape
#[derive(Clone, Debug)]
#[must_use="Consume any jump targets"]
pub enum JumpTarget {
    // this is a jump to another part of the frame
    CodeLoc(usize),
    // this is a forward reference to some other location
    JumpTable(usize),
    // this is indicating that we have an inner function call
    // as the next pc targe
    InnerFuncCall(),
}


// bytecode commands
#[allow(non_camel_case_types, dead_code)]
#[derive(Debug, Clone)]
pub enum BC {
    PUSH_NIL,
    PUSH_FALSE, PUSH_TRUE,
    PUSH_ELLIPSIS, 
    PUSH_NUMERAL(String), PUSH_STRING(String),
    PUSH_NUMBER(f64),
    PUSH_CODE_INDEX(usize),
    PUSH_NAMELIST(ast::Namelist, bool),
    PUSH_NEW_TBL,

    // this looks up a name in the env, and then puts it on the stack
    PUSH_VAL_BY_NAME(String),

    // with the stack being [value][name], set a local
    SET_LOCAL,

    ARRAY_ACCESS,
    DOT_ACCESS,

    POP,

    NOOP,

    GOTO(String),

    // operators
    BINOP(String), UNOP(String),

    // table operator
    ASSIGN_TABLE_VALUE,

    // foo = bar
    // n = $1
    ASSIGN_NAME(Name),
    // $1[$2]  = $3
    ASSIGN_ARR_ACCESS(),
    // $1.n = $2
    ASSIGN_DOT_ACCESS(Name),
    // call function (normal)
    CALL_FUNCTION,
    // call function (method)
    CALL_METHOD,

    // return without a value
    RETURN_NONE,
    // return the top of the stack
    RETURN_VALUE,
    BUILD_LIST(usize),
    BUILD_FUNCTION,

    // extract from exprlist puts a value from an exprlist
    // onto the top of the stack
    EXTRACT_FROM_EXPRLIST(usize),
    // assign something to a local
    // takes the top of the stack's array value
    // and puts it in the local env under name
    ASSIGN_LOCAL_FROM_EXPRLIST(Name, usize),
    // give a local value nil
    ASSIGN_LOCAL_NIL(Name),
    ASSIGN_LOCAL_FROM_TOP_OF_STACK(Name),
    // jump commands
    JUMP(JumpTarget),
    // pop the stack and jump if the value is truthy
    JUMP_TRUE(JumpTarget),
    JUMP_FALSE(JumpTarget),

    // for loop instructions
    // this is to simplify some easy operations
    // INIT: decremenent start value by the step
    FOR_LOOP_INIT,
    // CHECK_CONDITION:
    // do increment as needed
    // push a boolean to jump if needed
    // also edit the local with the current value
    FOR_LOOP_CHECK_CONDITION(Name),

    // for in loop instructions
    // this also simplifies stuff
    // INIT: set up init conditions
    FOR_IN_LOOP_INIT,

    // BREAK: find the latest for loop and break out into it
    BREAK,
    // PANIC: just blow up
    PANIC(String),
}

#[derive(Debug)]
pub struct Sourcemap<'a> {
    // this struct stores sourcemap information "nicely" (at least in theory)
    // later: make this more performant/space friendly
    map_data: Vec<LocatedSpan<&'a str>>,
    original_source: &'a str,
    // this lets us quickly get a line
    source_by_line: Vec<&'a str>,
}

impl<'a> Sourcemap<'a> {
    pub fn new(original_source: &'a str) -> Sourcemap<'a>{
	return Sourcemap {
	    map_data: vec![],
	    original_source: original_source,
	    source_by_line: original_source.split("\n").collect(),
	}
    }
    pub fn write_map(&mut self, bytecode_position: usize, location: LocatedSpan<&'a str>){
	// write the mapping for the bytecode position
	if self.map_data.len() < bytecode_position + 1 {
	    // copy the last known location N times
	    let default_location = match self.map_data.len() {
		0 => LocatedSpan::new("unknown location"),
		_ => self.map_data[self.map_data.len()-1]
	    };
	    self.map_data.resize(
		bytecode_position + 1,
		default_location,
	    );
	}
	self.map_data[bytecode_position] = location
    }

    pub fn get_location(&self, bytecode_position: usize) -> LocatedSpan<&str> {
	// take the bytecode position and get
	match self.map_data.get(bytecode_position) {
	    Some(loc) => *loc,
	    None => {
		// basically this gets called on the lines at the end of a trace, so just
		// return the last one
		// (this is hacky and dumb)
		self.map_data[self.map_data.len()-1]
	    }
	}
    }
    pub fn get_lines_for_bytecode(&self, bytecode_position: usize) -> (usize, &'a str) {
	let loc = self.get_location(bytecode_position);
	let loc_line: usize = loc.location_line().try_into().unwrap();
	return (loc_line, self.get_line(loc_line));
    }
    
    pub fn get_line(&self, source_line: usize) -> &'a str{
	match self.source_by_line.get(source_line-1) {
	    Some(txt) => txt,
	    None => "OUT OF BOUNDS SOURCEMAP",
	}
    }
}
// code objects
// they can include references to other code objects
#[derive(Debug)]
pub struct CodeObj<'a> {
    // the actual commands to run when we want to run this code block
    pub bytecode: Vec<BC>,
    // the mapping from bytecode to code location
    pub sourcemap: Sourcemap<'a>,
    // references to other code blocks
    // (for example code blocks for funciton definitions)
    inner_code: Vec<Rc<CodeObj<'a>>>,
    // positions in the bytecode for jump labels
    labels: HashMap<String, usize>,
    // jump targets, used for looping etc
    // jump_target[i] stores the code location to jump to
    pub jump_target: Vec<Option<usize>>,
}

impl<'a> CodeObj<'a> {
    pub fn lookup_code_by_idx<'b>(&'b self, idx: usize) -> Rc<CodeObj<'a>> {
	return self.inner_code[idx].clone();
    }
}

pub trait Code<'a> {
    // emit bytecode
    // return 
    fn emit(&mut self, elt: BC, location: Option<LocatedSpan<&'a str>>);

    // emit a noop and provide a jump target for later
    fn emit_jump_location(&mut self) -> JumpTarget;
    // save an inner code object, and return its index
    fn write_inner_code(&mut self, CodeObj<'a>) -> usize;
    // add a label
    fn add_label(&mut self, String);
    // prep a forward jump
    // this adds a jump command to a future line in bytecode
    // and then closes it out with set forward jump target
    fn prep_fwd_jump(&mut self) -> JumpTarget;
    // emit a NOOP and register the jump location
    fn emit_fwd_jump_location(&mut self, JumpTarget);
}

impl<'a> Code<'a> for CodeObj<'a> {
    fn emit(&mut self, elt: BC, location: Option<LocatedSpan<&'a str>>){
        self.bytecode.push(elt);
	if let Some(loc) = location {
	    self.sourcemap.write_map(
		self.bytecode.len() - 1,
		loc
	    );
	}
    }

    fn emit_jump_location(&mut self) -> JumpTarget {
        self.emit(BC::NOOP, None);
        let loc = self.bytecode.len() - 1;
        return JumpTarget::CodeLoc(loc);
    }

    fn prep_fwd_jump(&mut self) -> JumpTarget {
        self.jump_target.push(None);
        let jmp_idx = self.jump_target.len() - 1;
        return JumpTarget::JumpTable(jmp_idx);
    }

    fn emit_fwd_jump_location(&mut self, target: JumpTarget){
        match target {
            JumpTarget::CodeLoc(_) => {
                panic!("Forward jumps are for forward jumps!")
            },
            JumpTarget::JumpTable(i) => {
                // here we need to register the location in our jump table
                self.emit(BC::NOOP, None);
                let loc = self.bytecode.len() - 1;
                self.jump_target[i] = Some(loc);
            }
	    _ => {
		panic!("Unsupported jump emit");
	    }
        }
    }
    fn write_inner_code(&mut self, code: CodeObj<'a>) -> usize {
        // TODO is this safe?
        self.inner_code.push(Rc::new(code));
        // TODO not sure if this is the right thing to return either
        return self.inner_code.len() - 1;
    }

    fn add_label(&mut self, name: String) {
        // we'll add a noop command here to avoid any issues
        self.emit(BC::NOOP, None);
        let jmp_position = self.bytecode.len() - 1;
        self.labels.insert(name, jmp_position);
    }
}


pub fn compile_stat<'a>(stat: ast::Stat<'a>, code: &mut impl Code<'a>){
    use ast::StatV::*;

    match stat.v {
        Semicol => {},
        RawExpr(e) => {
            // we'll push the value, then pop it
	    {
		push_expr(e, code);
		code.emit(BC::POP, None);
	    }
        },
        Label(name) => {
            code.add_label(name.to_string());
        },
        Goto(name) => {
            code.emit(
                BC::GOTO(name.to_string()),
		None
            )
        },
        Do(block) => {
            compile_block(block, code);
        },
        While(expr, block) => {
            // evaluate an expression
            // if it's true, evaluate the block then jump backwards
            // if it's false, skip over the block
            let start_position = code.emit_jump_location();
            // first, we evaluate the expression
            push_expr(expr, code);
            // if it's false we jump to the end
            let jump_to_end = code.prep_fwd_jump();
            code.emit(
		BC::JUMP_FALSE(jump_to_end.clone(),),
		    None
	    );
            // else it's true, so we can evaluate the block
            compile_block(block, code);
            // if it was true, we jump back to the start position
            code.emit(BC::JUMP(start_position), None);
            // after jump location...
            code.emit_fwd_jump_location(jump_to_end);
        },
        LocalNames(namelist, maybe_exprlist) => {
            //http://www.lua.org/manual/5.3/manual.html#3.3.3
            // TODO last element of a function getting expanded
            match maybe_exprlist {
                Some(exprlist) => {
                    // first we push the expression list
                    push_exprlist(exprlist, code);
                    // then we will create assignment instructions for
                    // all the names
                    let namelist_size = namelist.len();
                    for i in 0..namelist_size{
                        // create assignment
                        code.emit(
                            BC::ASSIGN_LOCAL_FROM_EXPRLIST(
                                namelist[i].clone(),
                                i,
                            ),
			    None
                        )
                    }
		    code.emit(BC::POP, None);
                },
                None => {
                    // let's just make everything nil
                    for name in namelist {
                        code.emit(
                            BC::ASSIGN_LOCAL_NIL(name.to_string()),
			    None
                        )
                    }
                }
            }
        },
        LocalFuncDecl(name, funcbody) => {
            push_func(funcbody, false, code);
            code.emit(
                BC::ASSIGN_LOCAL_FROM_TOP_OF_STACK(name.to_string()),
		None
            );
        },
        Eql(varlist, exprlist) => {
            // TODO right-most function expansion
            push_exprlist(exprlist, code);
            // once we have the exprlist on the top of the stack
            // we'll try to assign to each variable
            let varlist_len = varlist.vars.len();
            for (i, var) in (0..varlist_len).zip(varlist.vars.into_iter()) {
                // first we extract out the value to assign...
                code.emit(BC::EXTRACT_FROM_EXPRLIST(i), None);
                // then we do the assignment
                push_var_assignment(var, code);
            }
        },
        If {predicate, then_block, elif_list, else_block} => {
            // in an if statement, we need to evaluate the predicate
            // then jump to the right places
            // first, let's evaluate the predicate
            push_expr(predicate, code);
            let after_then_block = code.prep_fwd_jump();
            // this second statement is used to jump over elifs and else
            // blocks
            let very_end_of_if_statement = code.prep_fwd_jump();
            // if the predicate is _false_ we go after the then block
            code.emit(BC::JUMP_FALSE(after_then_block.clone()), None);
            // now let's emit the then block;
            compile_block(then_block, code);
            // and then jump to the end
            code.emit(BC::JUMP(very_end_of_if_statement.clone()), None);
            code.emit_fwd_jump_location(after_then_block);
            //next up we see if there's any elif to deal with
            for (pred, block) in elif_list {
                // elifs work basically the same way
                push_expr(pred, code);
                let after_block = code.prep_fwd_jump();
                code.emit(BC::JUMP_FALSE(after_block.clone()), None);
                compile_block(block, code);
                code.emit(BC::JUMP(very_end_of_if_statement.clone()), None);
                code.emit_fwd_jump_location(after_block);
            }
            // finally we have the else block
            match else_block {
                Some(else_bl) => {
                    // if there's an else block and we get here, then
                    // we didn't execute any other block, so no condition
                    // to check here
                    // just execute
                    compile_block(else_bl, code);
                },
                None => {}
            }
            code.emit_fwd_jump_location(very_end_of_if_statement);
        },
        Repeat(block, expr) => {
            // repeat block until expr
            let start_of_repeat = code.emit_jump_location();
            compile_block(block, code);
            // let's check the expression now
            push_expr(expr, code);
            // then jump back to the top if needed
            code.emit(
                BC::JUMP_FALSE(start_of_repeat),
		None
            );
        },
        FuncDecl(funcname, funcbody) => {
            push_func(funcbody, funcname.method_component.is_some(), code);
	    // if this is a method (like "function elt:x(...)") then we need to assign the function to elt
	    // if not we just assign to the base name
	    let assignment_target = match funcname.other_name_components.len() {
		0 => match funcname.method_component {
		    // function f
		    None => ast::Var::N(funcname.first_name_component, funcname.loc),
		    // function f:attrname
		    Some(attrname) => ast::Var::DotAccess(
			ast::Prefixexpr {
			    prefix: ast::Prefix::Varname(funcname.first_name_component, funcname.loc),
			    suffixes: vec![],
			},
			attrname
		    )
		},
		_ => {
		    // function a.b.c.f
		    
		    let last_elt = funcname.other_name_components[funcname.other_name_components.len() - 1].clone();
		    let other_elts = funcname.other_name_components[0..funcname.other_name_components.len() - 1].iter().map(|name| ast::Suffix::DotAccess(name.to_string()) ).collect();
		    
		    ast::Var::DotAccess(
			ast::Prefixexpr {
			    prefix: ast::Prefix::Varname(funcname.first_name_component, funcname.loc),
			    suffixes: other_elts,
			},
			last_elt
		    )
		}
	    }; 

	    push_var_assignment(assignment_target, code);
        },
        For(name, start_value, limit, maybe_step, block) => {
            push_expr(start_value, code);
            push_expr(limit, code);
            match maybe_step {
                Some(step) => {
                    push_expr(step, code);
                },
                None => {
                    // default step of 1
                    code.emit(BC::PUSH_NUMBER(1.0), None);
                }
            }
            // here we use specialized op codes for for loops
            // first command is used to decrement start value for the loop
            code.emit(BC::FOR_LOOP_INIT, None);
            let for_loop_block_start = code.emit_jump_location();
            let for_loop_end = code.prep_fwd_jump();
            code.emit(BC::FOR_LOOP_CHECK_CONDITION(name.to_string()), None);
            // if the condition is not valid we jump out
            code.emit(BC::JUMP_FALSE(for_loop_end.clone()), None);
            // here we actually run the loop
            compile_block(block, code);
            // then we jump back up
            code.emit(BC::JUMP(for_loop_block_start), None);
            // END
            code.emit_fwd_jump_location(for_loop_end);
        },
        ForIn(_namelist, exprlist, _block) => {
            // for x,y,z,a in explist do block end
            // here the way this works is by evaluating explist
            // and then getting some data to be used as an iterator
            push_exprlist(exprlist, code);

            // so now we have the expression list, we then move to
            // starting up our for in loop
            code.emit(BC::FOR_IN_LOOP_INIT, None);
            // we then begin the actual loop
//            let for_loop_begin = code.emit_jump_location();
//            let end_of_for_loop
            code.emit(BC::PANIC("For-in loops are not implemented yet".to_string()), None);
        },
        Break => {
            code.emit(BC::BREAK, None);
        },
    }
}

pub fn push_var_assignment<'a>(var: ast::Var<'a>, code: &mut impl Code<'a>){
    use super::ast::Var::*;
    match var {
	N(name, loc) => {
	    // just assign directly;
	    code.emit(BC::ASSIGN_NAME(name.to_string()), Some(loc));
	},
	ArrAccess(prefix_expr, expr) => {
	    push_expr(expr, code);
	    push_prefixexpr(prefix_expr, code);
	    code.emit(BC::ASSIGN_ARR_ACCESS(), None);
	},
	DotAccess(prefix_expr, name) => {
	    push_prefixexpr(prefix_expr, code);
	    code.emit(BC::ASSIGN_DOT_ACCESS(name.to_string()), None);
	}
    }
}

pub fn push_numeral<'a>(n: &String, code: &mut impl Code<'a>){

    // we will parse out the value
    if n.starts_with("0x") {
	// hex
	let hex_wo_pfx = n.trim_start_matches("0x");
	match i64::from_str_radix(hex_wo_pfx, 16) {
	    Ok(v) => {
		// TODO get rid of rounding
		code.emit(BC::PUSH_NUMBER(v as f64), None)
	    },
	    Err(_) => {
		dbg!(n);
		panic!("PARSE FAILURE ON HEX NUMERAL");
	    }
	}
    } else {
	// not hex
	match n.parse::<f64>() {
	    Ok(v) => {
		code.emit(BC::PUSH_NUMBER(v), None)
	    },
	    Err(_) => {
		dbg!(n);
		panic!("PARSE FAILURE ON NUMERAL");
	    }
	}
    }
}
pub fn push_expr<'a>(expr: ast::Expr<'a>, code: &mut impl Code<'a>){

    use ast::Expr::*;
    
    match expr {
        Nil(loc) => code.emit(BC::PUSH_NIL, Some(loc)),
        False(loc) => code.emit(BC::PUSH_FALSE, Some(loc)),
        True(loc) => code.emit(BC::PUSH_TRUE, Some(loc)),
        Numeral(n, _loc) => {
	    push_numeral(&n, code);
	},
        LiteralString(s, loc) => code.emit(
            BC::PUSH_STRING(s.to_string(), ),
	    Some(loc),
        ),
        Ellipsis(loc) => code.emit(
	    BC::PUSH_ELLIPSIS,
	    Some(loc)
	),
        BinOp(left, op, right) => {
            // here we need to push the left and right expressions
            // then evaluate the operator
            push_expr(*left, code);
            push_expr(*right, code);
            // this should panic
            if let lex::LexValue::Keyword(raw_op) = op {
                code.emit(BC::BINOP(raw_op.to_string()), None);
            } else {
                panic!("Invalid binary operator");
            }
        },
        UnOp(op, left) => {
            // here just have to process one operator
            push_expr(*left, code);
            code.emit(BC::UNOP(op.to_string()), None);
        },
        Pref(prefixed_expr) => {
            push_prefixexpr(*prefixed_expr, code);
        },
        Tbl(ctr, _loc) => {
            push_table(ctr, code);
        },
        Functiondef(funcbody) => {
            push_func(funcbody, false, code);
        }
    }
}

pub fn new_code_obj<'a>(original_source: &'a str) -> CodeObj<'a>{
    return CodeObj {
        bytecode: Vec::new(),
	sourcemap: Sourcemap::new(original_source),
        inner_code: Vec::new(),
        labels: HashMap::new(),
        jump_target: Vec::new(),
    }
}

pub fn push_func<'a>(body: ast::Funcbody<'a>, has_colon: bool, code: &mut impl Code<'a>){
    // take a function body and register the op codes to push it to the stack
    // we'll need to build up a code object for this body to represent in the
    // bytecode as well
    //
    // (if a colon is provided via the has_colon bit, we add a self parameter to the parameters)
    let mut inner_code = new_code_obj("CONTENTS GOTTEN IN PUSH FUNC AND ARE WRONG");
    // this inner code object will hold our body 
    compile_block(body.body, &mut inner_code);

    let code_index = code.write_inner_code(inner_code);
    code.emit(BC::PUSH_CODE_INDEX(code_index), None);
    push_parlist(&body.parlist, has_colon, code);
    // this takes the parlist and the code index 
    // and builds a function from it
    code.emit(BC::BUILD_FUNCTION, None);
}

pub fn push_parlist<'a>(maybe_parlist: &Option<ast::Parlist>, with_self: bool, code: &mut impl Code<'a>){
    // push up a parameter list
    // with_self indicates whethert to add an implicit self
    match maybe_parlist {
        None => {
	    match with_self {
		true => code.emit(
		    BC::PUSH_NAMELIST(
			vec!["self".to_string()],
			false,
		    ),
		    None
		),
		false => code.emit(BC::PUSH_NIL, None)
	    }
	},
        Some(parlist) => {
	    let mut final_namelist = parlist.namelist.clone();
	    if with_self {
		// put self in the front, basically
		final_namelist.push("self".to_string());
		final_namelist.rotate_right(1);
	    }
            code.emit(
                BC::PUSH_NAMELIST(
		    final_namelist,
                    parlist.has_ellipsis,
                ),
		None
            )
        }
    }
}
pub fn push_table<'a>(ctr: std::option::Option<std::vec::Vec<ast::Field<'a>>>, code: &mut impl Code<'a>){
    use ast::Field::*;

    // push a table based on its constructor
    code.emit(BC::PUSH_NEW_TBL, None);
    // we will build tables by iterating over field listings
    match ctr {
        // if the table is just {} we just needed to emit the 
        // new table push
        None => {},
        Some(flds) => {
            // track the index for raw expressions
            let mut raw_idx = 1;
            for fld in flds {
                // first we want to push the key to the stack
                // then we'll push the value
                match fld {
                    Bracketed(k, v) => {
                        push_expr(k, code);
                        push_expr(v, code);
                    },
                    Named(n, v) => {
                        // here we want to emit the string
                        code.emit(
                            BC::PUSH_STRING(n.to_string()),
			    None,
                        );
                        push_expr(v, code);
                    },
                    Raw(e) => {
                        code.emit(
			    BC::PUSH_CODE_INDEX(raw_idx),
			    None
//                            BC::PUSH_NUMERAL(raw_idx.to_string()),
                        );
                        push_expr(e, code);
                        raw_idx += 1;
                    }
                }
                // at the end we want to assign to the table
                code.emit(
                    BC::ASSIGN_TABLE_VALUE,
		    None
                );
            }
        }
    }
}

pub fn push_prefixexpr<'a>(pexpr: ast::Prefixexpr<'a>, code: &mut impl Code<'a>){
    use ast::Prefix::*;
    use ast::Suffix::*;

    match pexpr.prefix {
        ParenedExpr(e) => push_expr(e, code),
        Varname(n, loc) => push_variable(
            ast::Var::N(n.to_string(), loc), code)
    }

    for suffix in pexpr.suffixes { 
        match suffix {
            ArrAccess(expr) => {
                // a[b]
                // a is already on the stack
                // push b
                push_expr(expr, code);
                code.emit(BC::ARRAY_ACCESS, None);
            },
            DotAccess(name) => {
                // a.b
                // a is already on the satck
                // push b
                code.emit(BC::PUSH_STRING(name.to_string()), None);
                code.emit(BC::DOT_ACCESS, None);
            },
            MethodCall(_name, _args) => {
                // a(:name)(args)
                // a is already on the stack
                match &_name {
                    Some(n) => {
                        code.emit(BC::PUSH_STRING(n.to_string()), None)
                    },
                    None => {}
                }

                // also push the arguments on the stack
                push_args(_args, code);

                match _name {
                    Some(_n) => {
                        code.emit(BC::CALL_METHOD, None)
                    },
                    None => {
                        code.emit(BC::CALL_FUNCTION, None)
                    }
                }
            }
        }
    }
}

pub fn push_variable<'a>(v: ast::Var<'a>, code: &mut impl Code<'a>){
    use ast::Var::*;

    match v {
        N(name, loc) => {
            // name access
            code.emit(
                BC::PUSH_VAL_BY_NAME(name.to_string()),
		Some(loc)
            )
        },
        ArrAccess(prefix_expr, expr) => {
            // a[b]
            // push a
            push_prefixexpr(prefix_expr, code);
            // push b
            push_expr(expr, code);
            code.emit(BC::ARRAY_ACCESS, None);
        },
        DotAccess(prefix_expr, name) => {
            // a.b
            // push a
            push_prefixexpr(prefix_expr, code);
            // push b
            code.emit(BC::PUSH_STRING(name.to_string()), None);
            code.emit(BC::DOT_ACCESS, None);
        }
    }
}

pub fn push_exprlist<'a>(exprs: std::vec::Vec<ast::Expr<'a>>, code: &mut impl Code<'a>){
    // take a list of expressions, and build the list from the values
    // being in the stack
    // [] -> [list_of_expressions]
    let expr_count = exprs.len();
    for expr in exprs {
        push_expr(expr, code);
    }
    code.emit(BC::BUILD_LIST(expr_count), None);
}


pub fn push_args<'a>(args: ast::Args<'a>, code: &mut impl Code<'a>){
    // build up the argument list for passing to a function
    // at the end we should have on top of the stack one list element
    // holding our arguments
    use ast::Args::*;

    match args {
        Literal(s) => {
            // just build the string and make a list
            code.emit(BC::PUSH_STRING(s.to_string()), None);
            code.emit(BC::BUILD_LIST(1), None);
        },
        Table(ctr) => {
            push_table(ctr, code);
            code.emit(BC::BUILD_LIST(1), None);
        },
        List(maybe_exprs) => {
            match maybe_exprs {
                None => {
                    // f()
                    // here we'll just push None to the stack
                    code.emit(BC::PUSH_NIL, None);
                },
                Some(exprs) => {
                    // f(a, b, c)
                    let expr_count = exprs.len();
                    for expr in exprs {
                        push_expr(expr, code);
                    }
                    code.emit(BC::BUILD_LIST(expr_count), None);
                }
            }
        }
    }
}

// pub fn push_funccall(call: ast::Funccall, code: &mut impl Code){
//     // write the bytecode to call a function and push the result onto the stack

//     // there are two modes, based on whether we are in method calling mode or not
//     match call.command_name {
//         None => {
//             // raw function mode
//             // push the function and then the arguments
//             // then trigger a call
//             push_prefixexpr(call.expr, code);
//             push_args(call.args, code);
//             code.emit(BC::CALL_FUNCTION);
//         },
//         Some(name) => {
//             // method mode 
//             // here we push the prefixed expression, name, and arguments
//             push_prefixexpr(call.expr, code);
//             code.emit(BC::PUSH_STRING(name));
//             push_args(call.args, code);
//             code.emit(BC::CALL_METHOD);
//         }
//     }
// }
pub fn compile_block<'a>(b: ast::Block<'a>, code: &mut impl Code<'a>) {

    let stats = b.stats;
    let retstat = b.retstat;
    for stat in stats {
        compile_stat(stat, code);
    }

    match retstat {
        None => {},
        Some(retstat) => {
            match retstat.return_expr {
                None => code.emit(BC::RETURN_NONE, None),
                Some(exprlist) => {
		    let l = exprlist.len();
                    if l == 0 {
                        // here we're also in a return none case
                        code.emit(BC::RETURN_NONE, None);
                    } else {
                        // we first need to get all the values
                        // and then build up a list
                        // and then return
                        
                        // this pushes exprlist.len() elements to stack
                        let exprlist_len = exprlist.len();
			if exprlist.len() == 1 {
			    // we'll do a simple return here
			    push_expr(exprlist[0].clone(), code);
			    
			} else {
			    // if we have multiple values we'll need to build up a
			    // list on returning
			    for expr in exprlist.into_iter() {
				push_expr(expr, code);
			    }
			    // this pops exprlist.len() elements to the stack
			    // then adds one
			    code.emit(
				BC::BUILD_LIST(exprlist_len),
				None
			    );
			}
                        // this removes the last element
                        code.emit(BC::RETURN_VALUE, None);
                    }
                }
            }
        }
    }
}
pub fn compile<'a>(parsed_block: ast::Block<'a>, contents: &'a str) -> CodeObj<'a> {
    let mut code = new_code_obj(contents);
    compile_block(parsed_block, &mut code);
    return code;
}
