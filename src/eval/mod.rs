#[macro_use]
#[allow(dead_code)]
pub mod exec;
use natives::lua_type;
use std::cell::RefCell;
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
	f: fn(&LuaRunState<'a>, Option<LV<'a>>) -> LV<'a>
    },
    LuaFunc {
	code_idx: usize,
	code: Rc<CodeObj>,
	args: ast::Namelist,
	ellipsis: bool,
	parent_env: LuaEnv<'a>,
    },
    // INTERNAL VALUES 
    // This code index value is just becauze I have usize
    // and I think I need to be careful here
    // this value shouldn't leak normally
    CodeIndex(usize),
    // this is the actual code object
    Code(Rc<CodeObj>),
    // namelist. lazy
    NameList(ast::Namelist, bool),
    LuaNil,
    LuaTrue,
    LuaFalse,
}

pub type LuaErr = String;

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

impl<'a> PartialEq for LV<'a> {
    
    fn eq(&self, other: &LV<'a>) -> bool {
	match self {
	    LV::Num(s) => match other {
		LV::Num(o) => s == o,
		_ => {
		    dbg!(self, other);
		    panic!("NEED TO LOOK UP EQ SEMANTICS FOR LUA");
		}
	    },
	    _ => {
		dbg!(self, other);
		todo!();
	    }
	}
    }
}
#[derive(Clone)]
pub struct LuaValueStack<'a> {
    values: Vec<LV<'a>>,
}

impl<'a> LuaValueStack<'a> {
    fn push(&mut self, val: LV<'a>){
	self.values.push(val);
    }
    #[allow(dead_code)]
    fn pop(&mut self) -> Option<LV<'a>> {
	return self.values.pop();
    }
}

#[derive(Debug, Clone)]
struct LuaEnvData<'a> {
    // pretty sure there's a way to set this up without requiring Rc's here as well...
    _values: Rc<RefCell<HashMap<String, LV<'a>>>>,
    parent: Option<Rc<RefCell<LuaEnvData<'a>>>>,
}

impl <'a> LuaEnvData<'a> {
    fn get(&self, name: &str) -> Option<LV<'a>> {
	// either the value is in the top sourcemap or it's in the parent somewhere
	let v = self._values.borrow();
	let maybe_val = v.get(name);
	match maybe_val {
	    // if the top level hashmap contains the value, return that
	    Some(val) => Some((*val).clone()),
	    // if not, look in the parent
	    None => {
		match &self.parent {
		    Some(env) => env.borrow().get(name),
		    // if there's no parent, then the value really just doesn't exist
		    None =>  None
		}
	    }
	}
    }
}
#[derive(Debug, Clone)]
pub struct LuaEnv<'a> {
    data: Rc<RefCell<LuaEnvData<'a>>>,
}

impl<'a> LuaEnv<'a> {
    fn new(values: HashMap<String, LV<'a>>, parent: Option<LuaEnv<'a>>) -> LuaEnv<'a>{
	let new_parent = match parent {
	    None => None,
	    Some(env) => Some(env.data.clone())
	};

	
	return LuaEnv {
	    data: Rc::new(RefCell::new(
		LuaEnvData {
		    _values: Rc::new(RefCell::new(values)),
		    parent: new_parent,
		}
	    ))
	}
    }
    fn set(&mut self, name: String, val: LV<'a>){
	self.data.borrow_mut()._values.borrow_mut().insert(name, val);
    }
    fn get(&self, name: &str) -> Option<LV<'a>> {
	return self.data.borrow().get(name);
    }

    fn make_child_env(&self) -> LuaEnv<'a> {
	// this makes a new environment that has self as a parent
	// so lookups will go through it (basically local scope from global)
	return LuaEnv {
	    data: Rc::new(RefCell::new(LuaEnvData {
		_values: Rc::new(RefCell::new(HashMap::new())),
		parent: Some(self.data.clone())
	    }))
	}
    }
}

#[derive(Clone)]
pub struct LuaFrame<'a>{
    // This is a single frame, that includes the environment etc
    // calling into another function will build a frame that will
    // execute whatever needs to be executed
    pub code: Rc<CodeObj>, // the code itself
    pub pc: usize, // program counter
    stack: LuaValueStack<'a>, // value stack
    env: LuaEnv<'a>,
}

impl<'a> LuaFrame<'a> {
    fn assign_args(&mut self, arglist: Vec<String>, args: LV<'a>){
	// we want to take every argument in the arglist and set it into the environment
	// as locals
	match args {
	    LV::LuaList(list) => {
		if arglist.len() != list.len() {
		    panic!("TODO implement different argument arity on function pass");
		}
		// assign the value to each name into the stack
		for (name, val) in arglist.iter().zip(list) {
		    self.env.set(name.to_string(), val);
		}
	    },
	    _ => {
		dbg!(args);
		panic!("Received wrong args the other day");
	    }
	}
    }
}

#[allow(dead_code)]
pub struct LuaRunState<'a> {
    /*
    * store the current state of a program 
    */
    pub file_path: String,
    compiled_code: Rc<CodeObj>,
    pub global_env: LuaEnv<'a>,
    // frame we are executing on
    pub current_frame: LuaFrame<'a>,
    // stack of frames (doesn't include existing frame)
    frame_stack: Vec<LuaFrame<'a>>,
    pub packages: HashMap<String, LV<'a>>,
}

impl<'a> LuaRunState<'a> {
    fn enter_function_call(&mut self, func: LV<'a>, provided_args: LV<'a>) {
	// set up a new lua frame as the top level func and work from there
	match func {
	    LV::LuaFunc {code, args, ellipsis, parent_env, ..} => {
		if ellipsis {
		    panic!("TODO implement ellipsis function calls");
		}
		let mut new_frame = frame_from_code(code, parent_env.make_child_env());
		new_frame.assign_args(args, provided_args);
		// let's get this new frame set up
		// TODO noclone
		self.frame_stack.push(self.current_frame.clone());
		self.current_frame = new_frame;
	    },
	    _ => {
		panic!("Received a non-func to enter");
	    }
	}
    }

    fn return_from_funccall(&mut self, return_value: LV<'a>) {
	// set up the return of the func call in the above frame and then execute further
	// we'll first prep the frame
	let mut previous_frame = self.frame_stack.pop().unwrap();
	previous_frame.stack.push(return_value);
	// then go to it
	self.current_frame = previous_frame;
    }

    pub fn load_code(&mut self, new_code_obj: CodeObj){
	// load in a code object to run
	let boxed_code = Rc::new(new_code_obj);
	let new_frame = frame_from_code(boxed_code.clone(), self.global_env.clone());
	self.current_frame = new_frame;
	self.frame_stack = vec![];
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
    macro_rules! global {
	($name: expr, $func: expr) => {
	    ($name.to_string(), LV::NativeFunc {
		name: $name.to_string(),
		f: $func
	    })
	}
    }
    
    let globals: Vec<(String, LV<'a>)> = vec![
	global!("print", lua_print),
	global!("require", lua_require),
	global!("assert", lua_assert),
	global!("type", lua_type),
    ];

    return LuaEnv::new(globals.iter().cloned().collect(), None);
}
pub fn frame_from_code<'a>(code:Rc<CodeObj>, env: LuaEnv<'a>) -> LuaFrame<'a> {
    return LuaFrame {
        code: code,
        pc: 0,
        stack: LuaValueStack {
            values: vec![]
        },
	env: env
    }
}


pub fn initial_run_state<'a>(contents: &'a str, lua_file_path: &'a str) -> LuaRunState<'a> {

    let parsed_content = parse(contents);
    let compiled_code = compile(parsed_content, contents);
    let boxed_code = Rc::new(compiled_code);
    let env = global_env();
    return LuaRunState {
        file_path: String::from(lua_file_path),
        compiled_code: boxed_code.clone(),
        current_frame: frame_from_code(boxed_code.clone(), env.clone()),
	global_env: env,
        frame_stack: vec![],
	packages: stdlib()
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env_inheritence(){
	let mut base_env = LuaEnv::new(
	    HashMap::new(),
	    None
	);
	assert_eq!(base_env.get("foo"), None);
	base_env.set("foo".to_string(), LV::Num(1.0));
	assert_eq!(base_env.get("foo"), Some(LV::Num(1.0)));
	let mut child_env = base_env.make_child_env();
	assert_eq!(child_env.get("foo"), Some(LV::Num(1.0)));
	child_env.set("foo".to_string(), LV::Num(2.0));
	assert_eq!(child_env.get("foo"), Some(LV::Num(2.0)));
	assert_eq!(base_env.get("foo"), Some(LV::Num(1.0)));
    }
}
