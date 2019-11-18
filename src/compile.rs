/**
 * This file contains the tooling to turn
 * a bunch of lex tokens into a list of bytecode
 * operations
 */
use ast;
use lex;
use std::collections::HashMap;

// jump target shape
#[derive(Clone, Debug)]
pub enum JumpTarget {
    CodeLoc(usize),
    JumpTable(usize),
}

// bytecode commands
#[allow(non_camel_case_types, dead_code)]
#[derive(Debug)]
pub enum BC {
    PUSH_NIL,
    PUSH_FALSE, PUSH_TRUE,
    PUSH_NUMERAL(String), PUSH_STRING(String),
    PUSH_NUMBER(usize),
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

    // jump commands
    JUMP(JumpTarget),
    // pop the stack and jump if the value is truthy
    JUMP_TRUE(JumpTarget),
    JUMP_FALSE(JumpTarget),
}

// code objects
// they can include references to other code objects
#[derive(Debug)]
pub struct CodeObj {
    // the actual commands to run when we want to run this code block
    pub bytecode: Vec<BC>,
    // references to other code blocks
    // (for example code blocks for funciton definitions)
    inner_code: Vec<CodeObj>,
    // positions in the bytecode for jump labels
    labels: HashMap<String, usize>,
    // jump targets, used for looping etc
    // jump_target[i] stores the code location to jump to
    jump_target: Vec<Option<usize>>,
}


pub trait Code {
    // emit bytecode
    // return 
    fn emit(&mut self, elt: BC);

    // emit a noop and provide a jump target for later
    fn emit_jump_location(&mut self) -> JumpTarget;
    // save an inner code object, and return its index
    fn write_inner_code(&mut self, CodeObj) -> usize;
    // add a label
    fn add_label(&mut self, String);
    // prep a forward jump
    // this adds a jump command to a future line in bytecode
    // and then closes it out with set forward jump target
    fn prep_fwd_jump(&mut self) -> JumpTarget;
    // emit a NOOP and register the jump location
    fn emit_fwd_jump_location(&mut self, JumpTarget);
}

impl Code for CodeObj {
    fn emit(&mut self, elt: BC){
        self.bytecode.push(elt);
    }

    fn emit_jump_location(&mut self) -> JumpTarget {
        self.emit(BC::NOOP);
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
                self.emit(BC::NOOP);
                let loc = self.bytecode.len() - 1;
                self.jump_target[i] = Some(loc);
            }
        }
    }
    fn write_inner_code(&mut self, code: CodeObj) -> usize {
        // TODO is this safe?
        self.inner_code.push(code);
        // TODO not sure if this is the right thing to return either
        return self.inner_code.len() - 1;
    }

    fn add_label(&mut self, name: String) {
        // we'll add a noop command here to avoid any issues
        self.emit(BC::NOOP);
        let jmp_position = self.bytecode.len() - 1;
        self.labels.insert(name, jmp_position);
    }
}


pub fn compile_stat(stat: ast::Stat, code: &mut impl Code){
    use ast::Stat::*;

    match stat {
        Semicol => {},
        RawExpr(e) => {
            // we'll push the value, then pop it
            push_expr(e, code);
            code.emit(BC::POP);
        },
        Label(name) => {
            code.add_label(name);
        },
        Goto(name) => {
            code.emit(
                BC::GOTO(name)
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
            code.emit(BC::JUMP_FALSE(jump_to_end.clone()));
            // else it's true, so we can evaluate the block
            compile_block(block, code);
            // if it was true, we jump back to the start position
            code.emit(BC::JUMP(start_position));
            // after jump location...
            code.emit_fwd_jump_location(jump_to_end);
        },
        LocalNames(namelist, maybe_exprlist) => {
            //http://www.lua.org/manual/5.3/manual.html#3.3.3
            panic!("Local names not implemented yet")
        },
        _ => {
            dbg!(stat);
            panic!("Not implemented yet");
        }
    }
}

pub fn push_expr(expr: ast::Expr, code: &mut impl Code){
    use ast::Expr::*;

    match expr {
        Nil => code.emit(BC::PUSH_NIL),
        False => code.emit(BC::PUSH_FALSE),
        True => code.emit(BC::PUSH_TRUE),
        Numeral(n) => code.emit(
            BC::PUSH_NUMERAL(n),
        ),
        LiteralString(s) => code.emit(
            BC::PUSH_STRING(s),
        ),
        Ellipsis => panic!("No ellipsis support yet"),
        BinOp(left, op, right) => {
            // here we need to push the left and right expressions
            // then evaluate the operator
            push_expr(*left, code);
            push_expr(*right, code);
            // this should panic
            if let lex::Lex::Keyword(raw_op) = op {
                code.emit(BC::BINOP(raw_op));
            } else {
                panic!("Invalid binary operator");
            }
        },
        UnOp(op, left) => {
            // here just have to process one operator
            push_expr(*left, code);
            code.emit(BC::UNOP(op));
        },
        Pref(prefixed_expr) => {
            push_prefixexpr(*prefixed_expr, code);
        },
        Tbl(ctr) => {
            push_table(ctr, code);
        },
        Functiondef(funcbody) => {
            push_func(funcbody, code);
        }
    }
}

pub fn new_code_obj() -> CodeObj {
    return CodeObj {
        bytecode: Vec::new(),
        inner_code: Vec::new(),
        labels: HashMap::new(),
        jump_target: Vec::new(),
    }
}

pub fn push_func(body: ast::Funcbody, code: &mut impl Code){
    // take a function body and register the op codes to push it to the stack
    // we'll need to build up a code object for this body to represent in the
    // bytecode as well
    let mut inner_code = new_code_obj();
    // this inner code object will hold our body 
    compile_block(body.body, &mut inner_code);

    let code_index = code.write_inner_code(inner_code);
    code.emit(BC::PUSH_NUMBER(code_index));
    push_parlist(body.parlist, code);
    // this takes the parlist and the code index 
    // and builds a function from it
    code.emit(BC::BUILD_FUNCTION);
}

pub fn push_parlist(maybe_parlist: Option<ast::Parlist>, code: &mut impl Code){
    match maybe_parlist {
        None => code.emit(BC::PUSH_NIL),
        Some(parlist) => {
            code.emit(
                BC::PUSH_NAMELIST(
                    parlist.namelist,
                    parlist.has_ellipsis,
                )
            )
        }
    }
}
pub fn push_table(ctr: ast::Tableconstructor, code: &mut impl Code){
    use ast::Field::*;

    // push a table based on its constructor
    code.emit(BC::PUSH_NEW_TBL);
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
                            BC::PUSH_STRING(n),
                        );
                        push_expr(v, code);
                    },
                    Raw(e) => {
                        code.emit(
                            BC::PUSH_NUMERAL(raw_idx.to_string()),
                        );
                        push_expr(e, code);
                        raw_idx += 1;
                    }
                }
                // at the end we want to assign to the table
                code.emit(
                    BC::ASSIGN_TABLE_VALUE,
                );
            }
        }
    }
}

pub fn push_prefixexpr(pexpr: ast::Prefixexpr, code: &mut impl Code){
    use ast::Prefix::*;
    use ast::Suffix::*;

    match pexpr.prefix {
        ParenedExpr(e) => push_expr(e, code),
        Varname(n) => push_variable(
            ast::Var::N(n), code)
    }

    for suffix in pexpr.suffixes { 
        match suffix {
            ArrAccess(expr) => {
                // a[b]
                // a is already on the stack
                // push b
                push_expr(expr, code);
                code.emit(BC::ARRAY_ACCESS);
            },
            DotAccess(name) => {
                // a.b
                // a is already on the satck
                // push b
                code.emit(BC::PUSH_STRING(name));
                code.emit(BC::DOT_ACCESS);
            },
            MethodCall(_name, _args) => {
                // a(:name)(args)
                // a is already on the stack
                let is_method_call = _name.is_some();

                match &_name {
                    Some(n) => {
                        code.emit(BC::PUSH_STRING(n.to_string()))
                    },
                    None => {}
                }

                // also push the arguments on the stack
                push_args(_args, code);

                match _name {
                    Some(_n) => {
                        code.emit(BC::CALL_METHOD)
                    },
                    None => {
                        code.emit(BC::CALL_FUNCTION)
                    }
                }
            }
        }
    }
}

pub fn push_variable(v: ast::Var, code: &mut impl Code){
    use ast::Var::*;

    match v {
        N(name) => {
            // name access
            code.emit(
                BC::PUSH_VAL_BY_NAME(name)
            )
        },
        ArrAccess(prefix_expr, expr) => {
            // a[b]
            // push a
            push_prefixexpr(prefix_expr, code);
            // push b
            push_expr(expr, code);
            code.emit(BC::ARRAY_ACCESS);
        },
        DotAccess(prefix_expr, name) => {
            // a.b
            // push a
            push_prefixexpr(prefix_expr, code);
            // push b
            code.emit(BC::PUSH_STRING(name));
            code.emit(BC::DOT_ACCESS);
        }
    }
}

pub fn push_args(args: ast::Args, code: &mut impl Code){
    // build up the argument list for passing to a function
    // at the end we should have on top of the stack one list element
    // holding our arguments
    use ast::Args::*;

    match args {
        Literal(s) => {
            // just build the string and make a list
            code.emit(BC::PUSH_STRING(s));
            code.emit(BC::BUILD_LIST(1));
        },
        Table(ctr) => {
            push_table(ctr, code);
            code.emit(BC::BUILD_LIST(1));
        },
        List(maybe_exprs) => {
            match maybe_exprs {
                None => {
                    // f()
                    // here we'll just push None to the stack
                    code.emit(BC::PUSH_NIL);
                },
                Some(exprs) => {
                    // f(a, b, c)
                    let expr_count = exprs.len();
                    for expr in exprs {
                        push_expr(expr, code);
                    }
                    code.emit(BC::BUILD_LIST(expr_count));
                }
            }
        }
    }
}

pub fn push_funccall(call: ast::Funccall, code: &mut impl Code){
    // write the bytecode to call a function and push the result onto the stack

    // there are two modes, based on whether we are in method calling mode or not
    match call.command_name {
        None => {
            // raw function mode
            // push the function and then the arguments
            // then trigger a call
            push_prefixexpr(call.expr, code);
            push_args(call.args, code);
            code.emit(BC::CALL_FUNCTION);
        },
        Some(name) => {
            // method mode 
            // here we push the prefixed expression, name, and arguments
            push_prefixexpr(call.expr, code);
            code.emit(BC::PUSH_STRING(name));
            push_args(call.args, code);
            code.emit(BC::CALL_METHOD);
        }
    }
}
pub fn compile_block(b: ast::Block, code: &mut impl Code) {
    for stat in b.stats {
        compile_stat(stat, code);
    }

    match b.retstat {
        None => {},
        Some(retstat) => {
            match retstat.return_expr {
                None => code.emit(BC::RETURN_NONE),
                Some(exprlist) => {
                    if exprlist.len() == 0 {
                        // here we're also in a return none case
                        code.emit(BC::RETURN_NONE);
                    } else {
                        // we first need to get all the values
                        // and then build up a list
                        // and then return
                        
                        // this pushes exprlist.len() elements to stack
                        let exprlist_len = exprlist.len();
                        for expr in exprlist {
                            push_expr(expr, code);
                        }
                        // this pops exprlist.len() elements to the stack
                        // then adds one
                        code.emit(
                            BC::BUILD_LIST(exprlist_len),
                        );
                        // this removes the last element
                        code.emit(BC::RETURN_VALUE);
                    }
                }
            }
        }
    }
}
pub fn compile(parsed_block: ast::Block) -> CodeObj {
    let mut code = new_code_obj();
    compile_block(parsed_block, &mut code);
    return code;
}
