#[macro_use]
#[allow(dead_code)]
pub mod exec;
pub mod attr;
use crate::natives::{lua_noop, LuaArgs};

use super::ast;
use super::compile::{compile, CodeObj};
use super::lua_stdlib::stdlib;
use super::natives::{lua_assert, lua_print, lua_require};
use crate::compile::BC;
use natives::lua_type;
use numbers::lua_tonumber;
use parse::parse;
use std::rc::Rc;
use std::{cell::RefCell, ops::Neg};

// debug toggles
const DBG_PRINT_INSTRUCTIONS: bool = true;
const DBG_POP_PUSH: bool = true;

// the type for native Lua functions
#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct LuaErr {
    _msg: String,
}

impl LuaErr {
    /**
     * Build an error that is just an error message
     * (wrap in result cuz that's the usual use case)
     **/
    pub fn msg<T, S: Into<String>>(s: S) -> Result<T, LuaErr> {
        return Err(LuaErr { _msg: s.into() });
    }
}

pub type LuaResult = Result<LV, LuaErr>;
pub type LuaNative = fn(&LuaRunState, &LuaArgs) -> LuaResult;

// TODO move to datastructs
#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum LuaHash {
    SHash(String),
    NHash(LNum),
}

pub fn lua_hash(v: &LV) -> LuaHash {
    match v {
        LV::LuaS(s) => return LuaHash::SHash(s.to_string()),
        LV::Num(n) => return LuaHash::NHash(*n),
        _ => {
            dbg!(v);
            todo!();
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LNum {
    Int(i64),
    Float(f64),
}

impl LNum {
    pub fn floor(self) -> LNum {
        match self {
            LNum::Int(_) => self,
            LNum::Float(v) => LNum::Float(v.floor()),
        }
    }

    pub fn as_float(self) -> f64 {
        match self {
            LNum::Int(i) => i as f64,
            LNum::Float(v) => v,
        }
    }
}
fn lnum_int(v: &LNum) -> i64 {
    use eval::LNum::*;
    match v {
        Int(i) => *i,
        Float(f) => *f as i64,
    }
}

fn lnum_float(v: &LNum) -> f64 {
    match v {
        LNum::Int(v) => *v as f64,
        LNum::Float(f) => *f,
    }
}

impl<T> From<T> for LNum
where
    T: Into<f64>,
{
    fn from(t: T) -> Self {
        LNum::Float(t.into())
    }
}
impl std::fmt::Display for LNum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LNum::Int(v) => f.debug_struct("Int").field("v", v).finish(),
            LNum::Float(v) => f.debug_struct("Float").field("v", v).finish(),
        }
    }
}

impl std::ops::Sub<&LNum> for &LNum {
    type Output = LNum;
    fn sub(self, rhs: &LNum) -> LNum {
        return *self - *rhs;
    }
}

impl std::ops::Sub<LNum> for LNum {
    type Output = LNum;

    fn sub(self, rhs: LNum) -> LNum {
        use self::LNum::*;
        // all usual operations convert integer to float
        match self {
            Int(v) => match rhs {
                Int(w) => Int(v.overflowing_sub(w).0),
                Float(w) => Float((v as f64) - w),
            },
            Float(v) => match rhs {
                Int(w) => Float(v - (w as f64)),
                Float(w) => Float(v - w),
            },
        }
    }
}

impl std::ops::Div<&LNum> for &LNum {
    type Output = LNum;
    fn div(self, rhs: &LNum) -> LNum {
        return *self / *rhs;
    }
}
impl std::ops::Div<LNum> for LNum {
    type Output = LNum;

    fn div(self, rhs: LNum) -> LNum {
        use self::LNum::*;

        match (self, rhs) {
            (Int(v), Int(w)) => {
                if w == 0 {
                    // implement divide by zero semantics even if the original
                    // values where ints
                    Float((v as f64) / (w as f64))
                } else {
                    Int(v / w)
                }
            }
            (_, _) => Float(lnum_float(&self) / lnum_float(&rhs)),
        }
    }
}

impl Neg for LNum {
    type Output = LNum;

    fn neg(self) -> LNum {
        use self::LNum::*;
        match self {
            Int(v) => Int(-v),
            Float(v) => Float(-v),
        }
    }
}
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

impl std::ops::Mul<&LNum> for &LNum {
    type Output = LNum;

    fn mul(self, rhs: &LNum) -> LNum {
        use self::LNum::*;
        match (self, rhs) {
            (Int(v), Int(w)) => Int(i64::wrapping_mul(*v, *w)),
            (_, _) => Float(lnum_float(self) * lnum_float(rhs)),
        }
    }
}
impl PartialOrd for LNum {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use self::LNum::*;
        match self {
            Int(v) => match other {
                Int(w) => return v.partial_cmp(w),
                Float(w) => return (&(*v as f64)).partial_cmp(w),
            },
            Float(v) => match *other {
                Int(w) => return v.partial_cmp(&(w as f64)),
                Float(w) => return v.partial_cmp(&w),
            },
        }
    }
}
impl std::ops::Add<&LNum> for &LNum {
    type Output = LNum;

    fn add(self, rhs: &LNum) -> LNum {
        use self::LNum::*;
        // all usual operations convert integer to float
        match self {
            Int(v) => match rhs {
                Int(w) => Int(v + w),
                Float(w) => Float(*v as f64 + w),
            },
            Float(v) => match rhs {
                Int(w) => Float(v + *w as f64),
                Float(w) => Float(v + w),
            },
        }
    }
}

impl std::ops::BitAnd for LNum {
    type Output = LNum;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (lnum_int(&self), lnum_int(&rhs)) {
            (x, y) => LNum::Int(x & y),
        }
    }
}

impl std::ops::BitOr for LNum {
    type Output = LNum;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (lnum_int(&self), lnum_int(&rhs)) {
            (x, y) => LNum::Int(x | y),
        }
    }
}
impl Eq for LNum {}
impl PartialEq for LNum {
    fn eq(&self, other: &LNum) -> bool {
        match self {
            LNum::Int(i) => match other {
                LNum::Int(v) => i == v,
                LNum::Float(v) => (*i as f64) == *v,
            },
            LNum::Float(i) => match other {
                LNum::Int(v) => *i == (*v as f64),
                LNum::Float(v) => i == v,
            },
        }
    }
}

impl Hash for LNum {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let byte_repr: u64 = u64::from_ne_bytes(match self {
            LNum::Int(i) => i.to_ne_bytes(),
            LNum::Float(f) => f.to_ne_bytes(),
        });
        // we have the bytes then the type tag
        let hash: u128 = ((byte_repr as u128) << 1)
            + (match self {
                LNum::Int(_) => 0,
                LNum::Float(_) => 1,
            });

        state.write_u128(hash);
    }
}
// our Lua values
#[derive(Clone)]
pub enum LV {
    Num(LNum),
    LuaS(String),
    // this is actually an internal structure meant for multiple returns
    LuaList(Vec<LV>),
    LuaTable {
        // this id is for identity purposes
        id: usize,
        v: Rc<RefCell<HashMap<LuaHash, LV>>>,
    },
    NativeFunc {
        name: String,
        f: LuaNative,
        // XXX returns_multiple does nothing! returning LuaList signals that
        returns_multiple: bool,
    },
    // Hack! to deal with the specialness of pcall
    PCall,
    BytecodeFunc {
        name: String,
        bytecode: Vec<BC>,
    },
    LuaFunc {
        id: usize,
        code_idx: usize,
        code: Rc<CodeObj>,
        args: ast::Namelist,
        ellipsis: bool,
        parent_env: LuaEnv,
    },
    // Not great!
    //
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

#[derive(Debug)]
pub struct LuaExc {
    // a Lua Exception
    pub msg: String,
}

fn lv_fmt(lv: &LV, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use self::LV::*;
    match lv {
        Num(n) => f.debug_struct("LuaNum").field("v", n).finish(),
        LuaS(s) => f.debug_struct("LuaString").field("v", s).finish(),
        LuaList(l) => f.debug_struct("LuaList").field("v", l).finish(),
        LuaTable { v: t, .. } => f.debug_struct("LuaTable").field("v", t).finish(),
        LuaFunc { code_idx, args, .. } => f
            .debug_struct("LuaFunc")
            .field("code_idx", code_idx)
            .field("args", args)
            .finish(),
        NativeFunc { name, f: _, .. } => f.debug_struct("NativeFunc").field("name", name).finish(),
        BytecodeFunc { name, .. } => f.debug_struct("BytecodeFunc").field("name", name).finish(),
        PCall => f.debug_struct("PCall").finish(),
        CodeIndex(n) => f.debug_struct("CodeIndex").field("v", n).finish(),
        NameList(namelist, ellipsis) => f
            .debug_struct("NameList")
            .field("args", namelist)
            .field("ellipsis", ellipsis)
            .finish(),
        LuaNil => f.debug_struct("Nil").finish(),
        LuaTrue => f.debug_struct("True").finish(),
        LuaFalse => f.debug_struct("False").finish(),
        Code(_) => f.debug_struct("<Code Object>").finish(),
    }
}
impl<'a> std::fmt::Debug for LV {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        lv_fmt(self, f)
    }
}
impl<'a> std::fmt::Display for LV {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        lv_fmt(self, f)
    }
}

impl<'a> PartialEq for LV {
    fn eq(&self, other: &LV) -> bool {
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
pub struct LuaValueStack {
    values: Vec<LV>,
}

impl LuaValueStack {
    fn push(&mut self, val: LV) {
        self.values.push(val);
    }
    #[allow(dead_code)]
    fn pop(&mut self) -> Option<LV> {
        return self.values.pop();
    }
}

#[derive(Debug, Clone)]
struct LuaEnvData {
    // pretty sure there's a way to set this up without requiring Rc's here as well...
    _values: Rc<RefCell<HashMap<String, LV>>>,
    parent: Option<Rc<RefCell<LuaEnvData>>>,
}

impl LuaEnvData {
    fn get(&self, name: &str) -> Option<LV> {
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
                    None => None,
                }
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct LuaEnv {
    data: Rc<RefCell<LuaEnvData>>,
}

impl LuaEnv {
    fn empty() -> LuaEnv {
        return LuaEnv {
            data: Rc::new(RefCell::new(LuaEnvData {
                _values: Rc::new(RefCell::new(HashMap::new())),
                parent: None,
            })),
        };
    }
    fn new(values: HashMap<String, LV>, parent: Option<LuaEnv>) -> LuaEnv {
        let new_parent = match parent {
            None => None,
            Some(env) => Some(env.data.clone()),
        };

        return LuaEnv {
            data: Rc::new(RefCell::new(LuaEnvData {
                _values: Rc::new(RefCell::new(values)),
                parent: new_parent,
            })),
        };
    }
    fn set(&mut self, name: String, val: LV) {
        self.data
            .borrow_mut()
            ._values
            .borrow_mut()
            .insert(name, val);
    }
    fn get(&self, name: &str) -> Option<LV> {
        return self.data.borrow().get(name);
    }

    fn make_child_env(&self) -> LuaEnv {
        // this makes a new environment that has self as a parent
        // so lookups will go through it (basically local scope from global)
        return LuaEnv {
            data: Rc::new(RefCell::new(LuaEnvData {
                _values: Rc::new(RefCell::new(HashMap::new())),
                parent: Some(self.data.clone()),
            })),
        };
    }

    fn get_parent_env(&self) -> LuaEnv {
        let parent_data = self.data.borrow().parent.as_ref().unwrap().clone();
        return LuaEnv { data: parent_data };
    }
}

#[derive(Clone)]
pub struct LuaFrame {
    // This is a single frame, that includes the environment etc
    // calling into another function will build a frame that will
    // execute whatever needs to be executed
    pub code: Rc<CodeObj>, // the code itself
    pub pc: usize,         // program counter
    stack: LuaValueStack,  // value stack
    protected_entry: bool, // whether we called this via pcall
    // (so whether we should handle results cleanly)
    // XXX in an alternate universe we'd add a pcall bytecode operator,
    // and we could have a way to turn on protected calls and then turn it off
    // (would be useful for supporting exception handling in general)
    env: LuaEnv,
}

impl LuaFrame {
    pub fn from_code(code: Rc<CodeObj>, env: LuaEnv, protected_call: bool) -> LuaFrame {
        return LuaFrame {
            code,
            pc: 0,
            stack: LuaValueStack { values: vec![] },
            env,
            protected_entry: protected_call,
        };
    }

    pub fn frame_for_native_call(func: LV, args: LV, protected_call: bool) -> LuaFrame {
        return LuaFrame {
            code: CodeObj::native_obj(),
            pc: 0,
            stack: LuaValueStack {
                values: vec![func, args],
            },
            /// XXX this is wrong
            env: LuaEnv::empty(),
            protected_entry: protected_call,
        };
    }
    fn assign_args(&mut self, arglist: Vec<String>, args: LV) {
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
            }
            _ => {
                dbg!(args);
                panic!("Received wrong args the other day");
            }
        }
    }
}

#[allow(dead_code)]
pub struct LuaRunState {
    /*
     * store the current state of a program
     */
    pub file_path: String,
    compiled_code: Rc<CodeObj>,
    pub global_env: LuaEnv,
    // frame we are executing on
    pub current_frame: LuaFrame,
    // stack of frames (doesn't include existing frame)
    frame_stack: Vec<LuaFrame>,
    pub packages: HashMap<String, LV>,

    // allocation stuff
    pub alloc: LuaAllocator,
    last_id: usize,
}

pub struct LuaAllocator {
    // XXX run out of last_id's?
    last_id: usize,
}

impl LuaAllocator {
    pub fn new() -> LuaAllocator {
        return LuaAllocator { last_id: 1 };
    }

    fn get_id(&mut self) -> usize {
        let id = self.last_id;
        self.last_id += 1;
        return id;
    }
    // provide a garbage-collectable empty table
    pub fn allocate_tbl(&mut self) -> LV {
        let table_id = self.get_id();
        return LV::LuaTable {
            v: Rc::new(RefCell::new(HashMap::new())),
            id: table_id,
        };
    }
}

impl<'a> LuaRunState {
    fn enter_function_call(&mut self, func: LV, provided_args: LV, protected_call: bool) {
        // set up a new lua frame as the top level func and work from there
        match func {
            LV::LuaFunc {
                code,
                args,
                ellipsis,
                parent_env,
                ..
            } => {
                if ellipsis {
                    panic!("TODO implement ellipsis function calls");
                }
                let mut new_frame =
                    LuaFrame::from_code(code, parent_env.make_child_env(), protected_call);
                new_frame.assign_args(args, provided_args);
                // let's get this new frame set up
                // TODO noclone
                self.frame_stack.push(self.current_frame.clone());
                self.current_frame = new_frame;
            }
            LV::NativeFunc { .. } => {
                let mut new_frame =
                    LuaFrame::frame_for_native_call(func, provided_args, protected_call);
                self.frame_stack.push(self.current_frame.clone());
                self.current_frame = new_frame;
            }

            LV::PCall => {
                // XXX this means that pcall doesn't show up in the stack trace
                match provided_args {
                    LV::LuaList(mut v) => {
                        if v.len() == 0 {
                            self.raise_error(LuaErr {
                                _msg: "Invalid params to pcall (0-length)".to_string(),
                            });
                            return;
                        }
                        let new_args = v.split_off(1);
                        let new_args = LV::LuaList(new_args);
                        // Safety: list is only of length 1 now
                        let func = v.pop().unwrap();
                        self.enter_function_call(func, new_args, true);
                        return;
                    }
                    _ => self.raise_error(
                        LuaErr::msg::<(), String>("Invalid params to pcall".to_string())
                            .unwrap_err(),
                    ),
                }
            }
            _ => {
                panic!("Received a non-func to enter");
            }
        }
    }

    /**
     *  return multiple values from a funccall
     **/
    fn return_multi_from_funccall(&mut self, mut return_values: Vec<LV>) {
        let protected_return = self.current_frame.protected_entry;

        let mut previous_frame = self.frame_stack.pop().unwrap();
        // the previous stack frame is going to get an exprlist on its stack
        // (this is to handle all the association stuff nicely)
        if protected_return {
            // we need to provide the true just to say that an error didn't occur
            return_values.insert(0, LV::LuaTrue)
        }
        previous_frame.stack.push(LV::LuaList(return_values));
        // then go to the frame
        self.current_frame = previous_frame;
    }
    fn return_from_funccall(&mut self, return_value: LV) {
        // this is the same as just returning one element
        self.return_multi_from_funccall(vec![return_value]);
    }

    // XXX lua_fail should go through this
    fn raise_error(&mut self, err: LuaErr) {
        // here we are going to go up the stack until we find
        // some protected call (or we are at the top)
        while self.frame_stack.len() > 0 {
            if self.current_frame.protected_entry {
                // we'll return the value here
                let mut previous_frame = self.frame_stack.pop().unwrap();
                // we failed, so we'll add that to the stack (protected call)
                previous_frame
                    .stack
                    .push(LV::LuaList(vec![LV::LuaFalse, LV::LuaS(err._msg)]));
                self.current_frame = previous_frame;
                return;
            } else {
                // we need to keep on going up the stack
                self.current_frame = self.frame_stack.pop().unwrap();
            }
        }
        // XXX we'll want to add more info on the failure later
        vm_panic!(self, err._msg);
    }
    pub fn load_code(&mut self, new_code_obj: CodeObj) {
        // load in a code object to run
        let boxed_code = Rc::new(new_code_obj);

        let new_frame = LuaFrame::from_code(boxed_code.clone(), self.global_env.clone(), false);
        self.current_frame = new_frame;
        self.frame_stack = vec![];
    }

    pub fn enter_child_scope(&mut self) {
        // enter a do block, mainly
        self.current_frame.env = self.current_frame.env.make_child_env();
    }

    pub fn leave_child_scope(&mut self) {
        // leave a do block, mainly
        self.current_frame.env = self.current_frame.env.get_parent_env();
    }
    // provide a garbage-collectable empty table
    pub fn allocate_tbl(&mut self) -> LV {
        // TODO probably want to clean this up later on
        return self.alloc.allocate_tbl();
    }
}
#[allow(dead_code)]
#[derive(PartialEq, Debug)]
pub enum RunResult {
    Error(String),
    Done(String),
}

#[allow(dead_code, unused_variables)]
pub fn run_to_checkpoint(state: LuaRunState) -> RunResult {
    return RunResult::Error(String::from("Yikes"));
}

pub fn global_env(stdlib: &HashMap<String, LV>) -> LuaEnv {
    macro_rules! global {
        ($name: expr, $func: expr) => {
            (
                $name.to_string(),
                LV::NativeFunc {
                    name: $name.to_string(),
                    f: $func,
                    returns_multiple: false,
                },
            )
        };
    }

    // hacky
    macro_rules! pkg {
        ($name: expr) => {
            ($name.to_string(), stdlib.get($name).unwrap().clone())
        };
    }
    let globals: Vec<(String, LV)> = vec![
        global!("print", lua_print),
        global!("require", lua_require),
        global!("assert", lua_assert),
        global!("type", lua_type),
        global!("tonumber", lua_tonumber),
        global!("collectgarbage", lua_noop),
        global!("setmetatable", lua_noop),
        ("pcall".to_string(), LV::PCall),
        pkg!("string"),
        pkg!("math"),
        pkg!("table"),
        pkg!("io"),
        pkg!("os"),
        pkg!("coroutine"),
        // pkg!("package"),
    ];

    return LuaEnv::new(globals.iter().cloned().collect(), None);
}
pub fn initial_run_state<'a>(contents: &'a str, lua_file_path: &'a str) -> LuaRunState {
    let parsed_content = parse(contents);
    let compiled_code = compile(parsed_content, contents);
    let boxed_code = Rc::new(compiled_code);
    let mut alloc = LuaAllocator::new();
    let packages = stdlib(&mut alloc);
    let env = global_env(&packages);
    let state = LuaRunState {
        file_path: String::from(lua_file_path),
        compiled_code: boxed_code.clone(),
        current_frame: LuaFrame::from_code(boxed_code.clone(), env.clone(), false),
        global_env: env,
        frame_stack: vec![],
        packages: packages,
        alloc: alloc,
        last_id: 0,
    };
    return state;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env_inheritence() {
        let mut base_env = LuaEnv::new(HashMap::new(), None);
        assert_eq!(base_env.get("foo"), None);
        base_env.set("foo".to_string(), LV::Num(LNum::Float(1.0)));
        assert_eq!(base_env.get("foo"), Some(LV::Num(LNum::Float(1.0))));
        let mut child_env = base_env.make_child_env();
        assert_eq!(child_env.get("foo"), Some(LV::Num(LNum::Float(1.0))));
        child_env.set("foo".to_string(), LV::Num(LNum::Float(2.0)));
        assert_eq!(child_env.get("foo"), Some(LV::Num(LNum::Float(2.0))));
        assert_eq!(base_env.get("foo"), Some(LV::Num(LNum::Float(1.0))));
    }
}
