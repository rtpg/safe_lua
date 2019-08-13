// loads of false positives from this decorator..
#[allow(dead_code)]
#[allow(unused_imports)]
use nom::character::complete::none_of;
use nom::combinator::opt;
use nom::sequence::preceded;
use nom::combinator::map;
use nom::multi::{many0, many1};
use nom::character::complete::{
    char as char_parse,
    digit1};
use nom::character::is_alphanumeric;
use nom::character::is_alphabetic;
use nom::branch::alt;
use nom::{
    Err,
    error::ErrorKind,
    IResult,
    bytes::complete::tag,
    combinator::peek,
};
use std::fs::File;
use std::io::Read;
use std::iter::FromIterator;

pub type IStream = [Lex];
pub type ISlice = [Lex];



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

pub fn parse_number(input: &str) -> IResult<&str, Lex> {
    return digit1(input).map(|(res, ds)| 
            (res, Lex::Number(ds.to_string()))
            );
}

pub fn parse_name(input: &str) -> IResult<&str, Lex> {
    match _find_name_bound(input) {
        Some(idx) => {
            let (result_name, rest_of_input ) = input.split_at(idx);
            return Ok((
                rest_of_input,
                Lex::Name(result_name.to_string()),
            ));
        },
        None =>  return Err(
            Err::Error((input, ErrorKind::Alpha))
        )
    }
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

pub fn parse_string(input: &str) -> IResult<&str, Lex> {
   let (i, quote_char) = alt((char_parse('"'),
                              char_parse('\'')))(input)?;

    
    let disallowed_characters = match quote_char {
        '\'' => "'\\",
        '"' => "\"\\",
        _ => panic!("Somehow matched a weird character")
    };

    // get the actual string contents
    let (i, string_contents) = many0(
        alt((
            // eiiher not the quote char
            // nore the escape
            none_of(disallowed_characters),
            // or the escape character followed by the quote
            preceded(char_parse('\\'), alt((
                char_parse(quote_char),
                map(char_parse('n'), |_| '\n'),
                // map(char_parse('a'), |_| '\a'),
                // map(char_parse('b'), |_| '\b'),
                // map(char_parse('f'), |_| '\f'),
            )))
        ))
    )(i)?;
    // then confirm that we have the last character
    let (i, _) = char_parse(quote_char)(i)?;
    // KNOWNWRONG not sure about the type safety of from_utf8
    return Ok((i, 
               Lex::Str(string_contents.into_iter().collect())));
}
pub fn parse_comment(input: &str) -> IResult<&str, &str> {
    match &input.get(..2) {
        Some("--") => {
            // dealing with a comment
            let split_text: Vec<&str> = input.splitn(2, "\n").collect();
            if split_text.len() == 1 {
                return Ok(("", input));
            } else{
                return Ok((split_text[1], split_text[0]));
            }
        },
        _ => {
            return Err(
                Err::Error((input, ErrorKind::Tag))
            );
        }
    }
}

pub fn parse_whitespace(input: &str) -> IResult<&str, &str> {
    match _whitespace_bound(input){
        Some(i) => {
            let (ws, rest_of_input) = input.split_at(i);
            return Ok((rest_of_input, ws));
        },
        None => {
            return Err(
                Err::Error((input, ErrorKind::TakeWhile1))
            )
        }
    }
}

pub fn lex_all_aux(i: &str) -> IResult<&str, Vec<Lex>> {
    
    let keyword_tokens = alt((
        alt((tag("and"),tag("break"),tag("do"),tag("else"),tag("elseif"),
    tag("end"),tag("false"),tag("for"),tag("function"),
    tag("goto"),tag("if"))),
        alt((tag("in"),tag("local"),
    tag("nil"),tag("not"),tag("or"),tag("repeat"),
    tag("return"),tag("then"),tag("true"),tag("until"),
    tag("while")))));

    let symbol_tokens  = alt((
        alt((tag("+"),tag("-"),tag("*"),tag("/"),tag("%"),tag("^"),
    tag("#"),tag("&"),tag("~"),tag("|"),tag("<<"),tag(">>"),tag("//"),
    tag("=="),tag("~="),tag("<="))),
    alt((tag(">="),tag("<"),
    tag(">"),tag("="),tag("("),tag(")"),tag("{"),tag("}"),
    tag("["),tag("]"),tag("::"),tag(";"),tag(":"),tag(","),
    tag("."),tag(".."),tag("...")))));

    let symbol_parse = symbol_tokens;
    let keyword_parse = keyword_tokens;

    // return many1(parse_name)(i);
    let ignored_content = alt((parse_whitespace, parse_comment));
    return many1(alt((
        map(ignored_content, |_| return Lex::Ignore),
        map(keyword_parse,
            |kwd: &str| return Lex::Keyword(kwd.to_string())),
        map(symbol_parse,
            |sym: &str| return Lex::Symbol(sym.to_string())),
        parse_name,
        parse_string,
        parse_number,
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

pub fn lex_all(i: &str) -> IResult<&str, Vec<Lex>> {
    return lex_all_aux(i).map(|(rest, tokens)| (
        rest,
        no_ignored_tokens(tokens)
    ));
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing_constructs(){
        // confirm that we can actually parse the lua test files
        let mut file = File::open("lua_tests/constructs.lua").unwrap();
        let mut contents = String::new();
        dbg!(&file);
        file.read_to_string(&mut contents);
        let parse_result = lex_all(contents.as_str());
        match parse_result {
            Ok((remaining_input, _)) => assert_eq!(remaining_input, ""),
            Err(_) => {
                dbg!(parse_result);
                panic!("Parse failure");
            }
        }
    }

    #[test]
    fn test_string_parsing(){
        assert_eq!(
            parse_string("\"\\n\""),
            Ok(("", Lex::Str("\n".to_string())))
        )
    }

    #[test]
    fn test_basic_lexing(){
        assert_eq!(
            parse_comment("-- hi there"),
            Ok(("", "-- hi there"))
        );

        assert_eq!(
            lex_all("hi -- hi there"),
            Ok(("", vec![Lex::Name("hi".to_string())]))
        );

        assert_eq!(
            parse_name("_abc"),
            Ok(("", Lex::Name("_abc".to_string())))
        );
        assert_eq!(
            parse_name("name12"),
            Ok(("", Lex::Name("name12".to_string())))
        );
        assert_eq!(
            lex_all("name"),
            Ok(("", vec![Lex::Name("name".to_string())]))
        );

        assert_eq!(
            lex_all("name1+n"),
            Ok(("", 
                vec![Lex::Name("name1".to_string()),
                     Lex::Symbol("+".to_string()),
                     Lex::Name("n".to_string())]))
        );

        assert_eq!(
            lex_all("name1 name2 name3"),
            Ok(("", vec![
                Lex::Name("name1".to_string()),
                Lex::Name("name2".to_string()),
                Lex::Name("name3".to_string())
            ]))
        );

        assert_eq!(
            lex_all("name1 name2(name3::name4"),
            Ok(("", vec![
                Lex::Name("name1".to_string()),
                Lex::Name("name2".to_string()),
                Lex::Symbol("(".to_string()),
                Lex::Name("name3".to_string()),
                Lex::Symbol("::".to_string()),
                Lex::Name("name4".to_string()),
            ]))
        )
    }
}