/**
 * This file contains the tooling to turn
 * a bunch of lex tokens into a list of bytecode
 * operations
 */
use ast;
use lex;

// bytecode commands
pub enum BC {
    PUSH_NIL,
    PUSH_FALSE, PUSH_TRUE,
    PUSH_NUMERAL(String), PUSH_STRING(String),
    PUSH_NUMBER(usize),
    PUSH_NAMELIST(ast::Namelist, bool),
    PUSH_NEW_TBL,

    PUSH_VAL_BY_NAME(String),
    ARRAY_ACCESS,
    DOT_ACCESS,

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
}

// code objects
// they can include references to other code objects
pub struct CodeObj {
    // the actual commands to run when we want to run this code block
    bytecode: Vec<BC>,
    // references to other code blocks
    // (for example code blocks for funciton definitions)
    inner_code: Vec<CodeObj>,
}


pub trait Code {
    // emit bytecode
    fn emit(&mut self, elt: BC);
    // save an inner code object, and return its index
    fn write_inner_code(&mut self, CodeObj) -> usize;
}

impl Code for CodeObj {
    fn emit(&mut self, elt: BC){
        self.bytecode.push(elt);
    }

    fn write_inner_code(&mut self, code: CodeObj) -> usize {
        // TODO is this safe?
        self.inner_code.push(code);
        // TODO not sure if this is the right thing to return either
        return self.inner_code.len() - 1;
    }
}

pub fn compile_stat(stat: ast::Stat, code: &mut impl Code){

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
    use ast::Prefixexpr::*;

    match pexpr {
        ParendExpr(e) => push_expr(e, code),
        Call(funccall) => push_funccall(*funccall, code),
        V(boxed_variable) => push_variable(*boxed_variable, code),
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
pub fn compile(parsed_block: ast::Block) -> Vec<BC>{
    return Vec::new();
}
