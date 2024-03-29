use super::compile;
use compile::CodeObj;
use parse;

pub fn try_compile_block<'a>(input: &'a str) -> Result<CodeObj, String> {
    // take some input and then build out the code for it
    // this is mainly for REPL usage
    let parse_result = parse::try_parse(input);
    match parse_result {
        Err(err) => return Err(err),
        Ok(tokens) => {
            let compile_result = compile(tokens, input);
            return Ok(compile_result);
        }
    }
}
