#[macro_use]
#[allow(dead_code)]
pub mod exec;
use ::file_contents;
use std::io::Read;
use std::collections::HashMap;
use ast::Block;
use parse::parse;
use super::compile::{
    CodeObj,
    compile,
};
use std::fs::File;
use lex;
use std::rc::Rc;

// our Lua values
#[derive(Debug, Clone)]
pub enum LV {
    Num(f64),
    LuaS(String),
    LuaList(Vec<LV>),
    NativeFunc(fn(Option<LV>) -> LV)
}

pub struct LuaValueStack {
    values: Vec<LV>,
}

pub struct LuaEnv{
    values: HashMap<String, LV>
}

pub struct LuaFrame{
    // This is a single frame, that includes the environment etc
    // calling into another function will build a frame that will
    // execute whatever needs to be executed
    code: Rc<CodeObj>, // the code itself
    pc: usize, // program counter
    stack: LuaValueStack, // value stack
    env: LuaEnv 
}

#[allow(dead_code)]
pub struct LuaRunState {
    /*
    * store the current state of a program 
    */
    file_path: String,
    compiled_code: Rc<CodeObj>,
    // frame we are executing on
    current_frame: LuaFrame,
    // stack of frames (doesn't include existing frame)
    frame_stack: Vec<LuaFrame>,
}

#[allow(dead_code)]
#[derive(PartialEq,Debug)]
pub enum RunResult {
    Error(String),
    Done(String)
}

#[allow(dead_code, unused_variables)]
pub fn run_to_checkpoint(state: LuaRunState) -> RunResult {
    /*
     * Do Stuff and return a result
     */
    return RunResult::Error(String::from("Yikes"));
}

pub fn lua_print(args: Option<LV>) -> LV {
    println!("CALLED LUA PRINT");
    return LV::Num(0.0);
}

pub fn frame_from_code(code:Rc<CodeObj>) -> LuaFrame {
    return LuaFrame {
        code: code,
        pc: 0,
        stack: LuaValueStack {
            values: vec![]
        },
	env: LuaEnv {
	    values: [("print".to_string(), LV::NativeFunc(lua_print))].iter().cloned().collect()
	}
    }
}
pub fn initial_run_state<'a>(lua_file_path: &'a str) -> LuaRunState {

    file_contents!(lua_file_path, contents);
    let parsed_content = parse(&contents);
    let compiled_code = compile(parsed_content);
    let boxed_code = Rc::new(compiled_code);
    return LuaRunState {
        file_path: String::from(lua_file_path),
        compiled_code: boxed_code.clone(),
        current_frame: frame_from_code(boxed_code.clone()),
        frame_stack: vec![],
    };
}

pub fn load_file(file_path: &str) -> LuaRunState {
    return initial_run_state(file_path);
}
