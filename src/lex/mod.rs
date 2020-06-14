// loads of false positives from this decorator..
#[allow(dead_code)]
#[allow(unused_imports)]
use nom::character::complete::none_of;
use nom::sequence::preceded;
use nom::combinator::{map, opt};
use nom::multi::{many0, many1};
use nom::character::complete::{
    char as char_parse,
    one_of,
};
use nom::character::is_alphanumeric;
use nom::character::is_alphabetic;
use nom::branch::alt;
use nom::{
    Err,
    error::ErrorKind,
    IResult,
    bytes::complete::tag,
};

use std::collections::HashSet;

pub type IStream = [Lex];
pub type ISlice = [Lex];

mod num;
mod strings;


/**

Lua is a free-form language. It ignores spaces (including new lines) and 
comments between lexical elements (tokens), except as delimiters 
between names and keywords.

Names (also called identifiers) in Lua can be any string of letters,
 digits, and underscores, not beginning with a digit and not being
  a reserved word. Identifiers are used to name variables, table 
  fields, and labels. 

 */
#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Lex {
    Name(String),
    Keyword(String),
    Symbol(String),
    Str(String),
    Number(String),
    Ignore,
}

fn _find_name_bound(input: &str) -> Option<usize> {
    // find until where a name is present
    for (i, &item) in input.as_bytes().iter().enumerate(){
        if i == 0 {
            // first character needs to be either 
            // an underscore or an alphanumeric
            if is_alphabetic(item) || (item == b'_') {
                // this is still a name
                continue;
            } else {
                // not the right first character, bail
                return None;
            }
        } else {
            // if we're still looking at a name character continue
            if is_alphanumeric(item) || (item == b'_') {
                continue;
            } else {
                return Some(i);
            }
        }
    }
    let l = input.len();
    if l == 0{
        // don't match on empty string
        return None;
    }

    return Some(l);
}


//     return digit1(input).map(|(res, ds)| 
//             (res, Lex::Number(ds.to_string()))
//             );
// }

lazy_static! {
pub static ref KWDS: HashSet<&'static str> = "     and       break     do        else      elseif    end
     false     for       function  goto      if        in
     local     nil       not       or        repeat    return
     then      true      until     while".split_whitespace().collect();
}
fn _is_keyword(s: &str) -> bool {
    return KWDS.contains(s);
}

fn alphabetic(input: LexInput) -> IResult<LexInput, char> {
    return one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")(input);
}
fn alphanumeric(input: LexInput) -> IResult<LexInput, char> {
    return one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")(input);
}

use nom_locate::LocatedSpan;

pub type LexInput<'a> = LocatedSpan<&'a str>;
pub fn parse_name(starting_input: LexInput) -> IResult<LexInput, Lex> {
    let (input, first_char) = alphabetic(starting_input)?;
    let (input, rest) = many0(alphanumeric)(input)?;
    let rest_str: String = rest.into_iter().collect();
    let result_name = first_char.to_string() + &rest_str;

    if _is_keyword(&result_name){
        return Err(
            Err::Error((starting_input, ErrorKind::Alpha))
        );
    }

    return Ok((
        input,
        Lex::Name(result_name),
    ));
}

fn _whitespace_bound(input: &str) -> Option<usize> {
    for (i, &item) in input.as_bytes().iter().enumerate() {
        if item.is_ascii_whitespace(){
            continue;
        } else {
            if i==0 {
                return None;
            } else {
                return Some(i);
            }
        }
    }
    if input.len() == 0 {
        return None; 
    }
    return Some(input.len());
}

pub fn parse_comment(starting_input: LexInput) -> IResult<LexInput, Lex> {
    // start with the comment tag
    let (input, _ ) = tag("--")(starting_input)?;

    match strings::long_string(input) {
	Ok(ls_result) => Ok(ls_result),
	_ => {
	    // not a long string, just parse to the end of the line
	    let (input, comment_body) = many0(none_of("\n"))(input)?;
	    let comment_body_str = comment_body.into_iter().collect();
	    Ok((input, Lex::Str(comment_body_str)))
	}
    }
}

pub fn parse_whitespace(input: LexInput) -> IResult<LexInput, Lex> {
    let (rest_of_input, ws) = many1(one_of(" \t\n\r"))(input)?;
    return Ok((rest_of_input, Lex::Str(rest_of_input.to_string())))
    // match _whitespace_bound(input){
    //     Some(i) => {
    //         let (ws, rest_of_input) = input.split_at(i);
    //         return Ok((rest_of_input, Lex::Str(ws.to_string())));
    //     },
    //     None => {
    //         return Err(
    //             Err::Error((input, ErrorKind::TakeWhile1))
    //         )
    //     }
    // }
}

pub fn keyword_parse(i: LexInput) -> IResult<LexInput, Lex> {
    
    return map(alt((
        alt((tag("and"),tag("break"),tag("do"),tag("elseif"),tag("else"),
    tag("end"),tag("false"),tag("for"),tag("function"),
    tag("goto"),tag("if"))),
        alt((tag("in"),tag("local"),
    tag("nil"),tag("not"),tag("or"),tag("repeat"),
    tag("return"),tag("then"),tag("true"),tag("until"),
	     tag("while"))))),
    |kwd: LexInput| return Lex::Keyword(kwd.to_string()))(i);
}

pub fn symbol_parse(i: LexInput) -> IResult<LexInput, Lex> {
    return map(alt((
        alt((tag("+"),tag("-"),tag("*"),tag("//"),tag("%"),tag("^"),
    tag("#"),tag("&"),tag("~="),tag("|"),tag("<<"),tag(">>"),tag("/"),
    tag("=="),tag("~"),tag("<="))),
    alt((tag(">="),tag("<"),
    tag(">"),tag("="),tag("("),tag(")"),tag("{"),tag("}"),
    tag("["),tag("]"),tag("::"),tag(";"),tag(":"),tag(","),
	 tag("..."),tag(".."),tag("."))))),
	       |sym: LexInput| return Lex::Symbol(sym.to_string()))(i);
}
pub fn lex_all_aux(i: LexInput) -> IResult<LexInput, Vec<Lex>> {
    
    // return many1(parse_name)(i);
    let ignored_content = alt((parse_whitespace, parse_comment));
    // if there's a shebang line, ignore it by parsing it out
    let (i, _) = opt(preceded(tag("#"), many0(none_of("\n"))))(i)?;

    return many1(alt((
        map(ignored_content, |_| return Lex::Ignore),
        strings::parse_string,
        num::parse_number,
        parse_name,
	keyword_parse,
	symbol_parse,
    )))(i);
}

pub fn no_ignored_tokens(i: Vec<Lex>) -> Vec<Lex> {
    let mut return_value: Vec<Lex> = Vec::new();
    for token in i.iter(){
        match token {
            Lex::Ignore => {},
            _ => return_value.push(token.clone())
        }
    }
    return return_value;
}

pub fn lex_all(i: LexInput) -> IResult<LexInput, Vec<Lex>> {
    return lex_all_aux(i).map(|(rest, tokens)| (
        rest,
        no_ignored_tokens(tokens)
    ));
}

#[macro_export]
macro_rules! assert_full_parse {
    ($parser: expr, $input: expr, $expected_result: expr) => {
	let input_data = LexInput::new($input);
	let parse_result = $parser(input_data);
	match parse_result {
	    Ok((remaining_input, result)) => {
		assert_eq!(remaining_input.fragment(), &"");
		assert_eq!($expected_result, result);
	    },
	    Err(_) => {
		dbg!(parse_result).unwrap();
		panic!("full parse failure");
	    }
	}
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    use std::fs::File;
    use std::io::Read;

    #[test]
    fn test_parsing_constructs(){
        // confirm that we can actually parse the lua test files
        let mut file = File::open("lua_tests/constructs.lua").unwrap();
        let mut contents = String::new();
        dbg!(&file);
        file.read_to_string(&mut contents).unwrap();
        let parse_result = lex_all(LexInput::new(&contents));
        match parse_result {
            Ok((remaining_input, _)) => assert_eq!(remaining_input.fragment(), &""),
            Err(_) => {
                dbg!(parse_result).unwrap();
                panic!("Parse failure");
            }
        }
    }

    #[test]
    fn test_string_parsing(){
        assert_full_parse!(
            strings::parse_string,
	    "\"\\n\"",
            Lex::Str("\n".to_string())
        );
    }

    #[test]
    fn test_basic_lexing(){
        assert_full_parse!(
            parse_comment,
	    "-- hi there",
            Lex::Str(" hi there".to_string())
        );

        assert_full_parse!(
            lex_all,
	    "hi -- hi there",
            vec![Lex::Name("hi".to_string())]
	);

        assert_full_parse!(
            lex_all,
	    "string.format(a)",
	    vec![Lex::Name("string".to_string()),
		Lex::Symbol(".".to_string()),
		Lex::Name("format".to_string()),
		Lex::Symbol("(".to_string()),
		Lex::Name("a".to_string()),
		Lex::Symbol(")".to_string())]
        );

        assert_full_parse!(
            parse_name,
	    "_abc",
            Lex::Name("_abc".to_string())
        );
        assert_full_parse!(
            parse_name,
	    "name12",
            Lex::Name("name12".to_string())
        );
	
        assert_full_parse!(
            lex_all,
	    "name",
            vec![Lex::Name("name".to_string())]
        );

        assert_full_parse!(
            lex_all,
	    "name1+n",
            vec![Lex::Name("name1".to_string()),
                 Lex::Symbol("+".to_string()),
                 Lex::Name("n".to_string())]
	);

        assert_full_parse!(
            lex_all,
	    "name1 name2 name3",
            vec![
                Lex::Name("name1".to_string()),
                Lex::Name("name2".to_string()),
                Lex::Name("name3".to_string())
            ]
        );

        assert_full_parse!(
            lex_all,
	    "name1 name2(name3::name4",
            vec![
                Lex::Name("name1".to_string()),
                Lex::Name("name2".to_string()),
                Lex::Symbol("(".to_string()),
                Lex::Name("name3".to_string()),
                Lex::Symbol("::".to_string()),
                Lex::Name("name4".to_string()),
            ]
        );
    }
}
