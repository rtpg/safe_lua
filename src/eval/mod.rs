#[macro_use]
#[allow(dead_code)]
pub mod exec;
use std::collections::HashMap;
use parse::parse;
use super::ast;
use super::compile::{
    CodeObj,
    compile,
};
use std::rc::Rc;
use super::natives::{
    lua_assert,
    lua_print,
    lua_require
};
use super::lua_stdlib::stdlib;

// debug toggles
const DBG_PRINT_INSTRUCTIONS: bool = false;
const DBG_POP_PUSH: bool = false;

// our Lua values
#[derive(Clone)]
pub enum LV<'a> {
    Num(f64),
    LuaS(String),
    LuaList(Vec<LV<'a>>),
    LuaTable {
	v: HashMap<String, LV<'a>>
    },
    NativeFunc {
	name: String,
	f: fn(&LuaRunState<'a>, Option<LV>) -> LV<'a>
    },
    LuaFunc {
	code_idx: usize,
	code: Rc<CodeObj<'a>>,
	args: ast::Namelist,
	ellipsis: bool,
    },
    // INTERNAL VALUES 
    // This code index value is just becauze I have usize
    // and I think I need to be careful here
    // this value shouldn't leak normally
    CodeIndex(usize),
    // this is the actual code object
    Code(Rc<CodeObj<'a>>),
    // namelist. lazy
    NameList(ast::Namelist, bool),
    LuaNil,
    LuaTrue,
    LuaFalse,
}

#[derive(Debug)]
pub struct LuaExc {
    // a Lua Exception
    pub msg: String 
}

fn lv_fmt(lv: &LV, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use self::LV::*;
    match lv {
	Num(n) => {
	    f.debug_struct("LuaNum").field("v", n).finish()
	},
	LuaS(s) => {
	    f.debug_struct("LuaString").field("v", s).finish()
	},
	LuaList(l) => {
	    f.debug_struct("LuaList").field("v", l).finish()
	},
	LuaTable{v:t} => {
	    f.debug_struct("LuaTable").field("v", t).finish()
	},
	LuaFunc {code_idx, args, ..} => {
	  f.debug_struct("LuaFunc").field("code_idx", code_idx).field("args", args).finish()  
	},
	NativeFunc {name, f: _} => {
	    f.debug_struct("NativeFunc").field("name", name).finish()
	},
	CodeIndex(n) => {
	    f.debug_struct("CodeIndex").field("v", n).finish()
	},
	NameList(namelist, ellipsis) => {
	    f.debug_struct("NameList").field("args", namelist).field("ellipsis", ellipsis).finish()
	},
	LuaNil => {
	    f.debug_struct("Nil").finish()
	},
	LuaTrue => {
	    f.debug_struct("True").finish()
	},
	LuaFalse => {
	    f.debug_struct("False").finish()
	},
	Code(_) => {
	    f.debug_struct("<Code Object>").finish()
	}
    }
}
impl<'a> std::fmt::Debug for LV<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	lv_fmt(self, f)
    }
}
impl<'a> std::fmt::Display for LV<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	lv_fmt(self, f)
    }
}

pub struct LuaValueStack<'a> {
    values: Vec<LV<'a>>,
}

pub struct LuaEnv<'a> {
    values: HashMap<String, LV<'a>>
}

pub struct LuaFrame<'a>{
    // This is a single frame, that includes the environment etc
    // calling into another function will build a frame that will
    // execute whatever needs to be executed
    code: Rc<CodeObj<'a>>, // the code itself
    pc: usize, // program counter
    stack: LuaValueStack<'a>, // value stack
    env: LuaEnv<'a>
}

impl<'a> LuaFrame<'a> {
    fn assign_args(&mut self, arglist: Vec<String>, args: LV){
	// we want to take every argument in the arglist and set it into the environment
	// as locals
	panic!("Implement assigning args");
    }
}

#[allow(dead_code)]
pub struct LuaRunState<'a> {
    /*
    * store the current state of a program 
    */
    file_path: String,
    compiled_code: Rc<CodeObj<'a>>,
    // frame we are executing on
    current_frame: LuaFrame<'a>,
    // stack of frames (doesn't include existing frame)
    frame_stack: Vec<LuaFrame<'a>>,
    pub packages: HashMap<String, LV<'a>>,
}

impl<'a> LuaRunState<'a> {
    fn enter_function_call(&mut self, func: LV, provided_args: LV) {
	// set up a new lua frame as the top level func and work from there
	match func {
	    LV::LuaFunc {..} => {
		
		panic!("TODO: implement function call entering");
	    },
	    _ => {
		panic!("Received a non-func to enter");
	    }
	}
    }

    fn return_from_funccall(&mut self) {
	// set up the return of the func call in the above frame and then execute further
	panic!("TODO: implement func returns");
    }
}
#[allow(dead_code)]
#[derive(PartialEq,Debug)]
pub enum RunResult {
    Error(String),
    Done(String)
}

#[allow(dead_code, unused_variables)]
pub fn run_to_checkpoint(state: LuaRunState) -> RunResult {
    return RunResult::Error(String::from("Yikes"));
}

pub fn global_env<'a>() -> LuaEnv<'a> {
    let globals: Vec<(String, LV<'a>)> = vec![
	("print".to_string(), LV::NativeFunc {
	    name: "print".to_string(),
	    f: lua_print
	}),
	("require".to_string(), LV::NativeFunc {
	    name: "require".to_string(),
	    f: lua_require
	}),
	("assert".to_string(), LV::NativeFunc {
	    name: "assert".to_string(),
	    f: lua_assert
	})
    ];

    return LuaEnv {
	values: globals.iter().cloned().collect()
    }

    
}
pub fn frame_from_code(code:Rc<CodeObj>) -> LuaFrame {
    return LuaFrame {
        code: code,
        pc: 0,
        stack: LuaValueStack {
            values: vec![]
        },
	env: global_env()
    }
}


pub fn initial_run_state<'a>(contents: &'a str, lua_file_path: &'a str) -> LuaRunState<'a> {

    let parsed_content = parse(contents);
    let compiled_code = compile(parsed_content);
    let boxed_code = Rc::new(compiled_code);
    return LuaRunState {
        file_path: String::from(lua_file_path),
        compiled_code: boxed_code.clone(),
        current_frame: frame_from_code(boxed_code.clone()),
        frame_stack: vec![],
	packages: stdlib()
    };
}
