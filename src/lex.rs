use nom::combinator::opt;
use nom::sequence::preceded;
use nom::combinator::map;
use nom::multi::many1;
use nom::character::is_alphanumeric;
use nom::character::is_alphabetic;
use nom::branch::alt;
use nom::{
    Err,
    error::ErrorKind,
    IResult,
    bytes::complete::tag,
};

pub type IStream = str;

/**

Lua is a free-form language. It ignores spaces (including new lines) and 
comments between lexical elements (tokens), except as delimiters 
between names and keywords.

Names (also called identifiers) in Lua can be any string of letters,
 digits, and underscores, not beginning with a digit and not being
  a reserved word. Identifiers are used to name variables, table 
  fields, and labels. 

 */
#[derive(Debug, PartialOrd, PartialEq)]
pub enum Lex {
    Name(String),
    Keyword(String),
    Symbol(String),
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
    return Some(input.len());
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

pub fn lex_all(i: &str) -> IResult<&str, Vec<Lex>> {
    
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
    return many1(alt((
        map(preceded(opt(parse_whitespace), keyword_parse),
            |kwd: &str| return Lex::Keyword(kwd.to_string())),
        map(preceded(opt(parse_whitespace), symbol_parse),
            |sym: &str| return Lex::Symbol(sym.to_string())),
        preceded(opt(parse_whitespace), parse_name),
    )))(i);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_lexing(){
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