use eval::LuaRunState;
use compile::BC;

pub enum ExecResult {
    Error(String),
    Yield(String),
}

pub fn exec_to_next_yield(s: &mut LuaRunState, yield_result: Option<u8>) -> ExecResult{
    // Move the machine forward until we hit the next yield
    loop {
        // let's get the next bytecode instruction to run
        let bc = s.current_frame.pc;
        let next_instruction = &s.current_frame.code.bytecode[bc];

        match next_instruction {
            BC::NOOP => {panic!("")},
            BC::PUSH_NIL => {panic!("")},
            BC::PUSH_FALSE => {panic!("")},
            _ => {
                dbg!(next_instruction);
                panic!("Unhandled Bytecode");
            }
        }
    }
    return ExecResult::Error("No implementation".to_string());
}