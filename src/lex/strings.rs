use super::*;
use std::str::FromStr;
use nom::multi::many_till;
use nom::character::complete::anychar;

pub fn parse_out_string_contents<'a>(input: &'a str) -> Option<(&'a str, &'a str)> {
    /**
     * Look at a string and determine if it starts as a lua string 
     * and if so return:
     *   - the string contents themselves
     *   - the remaining input
     **/

    // let's first figure out what kind of string this is
    // is this a simple quote?
    enum QuoteMode {
        Nothing, // failed parse
        SimpleQuote(u8),
        BlockQuote(usize), // level
    }

    // go to bytes, this will probably generate issues later
    let input = input.as_bytes();

    let quote_mode = match input.get(0) {
        None => QuoteMode::Nothing,
        Some(b'"') => QuoteMode::SimpleQuote(b'"'),
        Some(b'\'') => QuoteMode::SimpleQuote(b'\''),
        Some(b'[') => {
            // time to see if we can get an opening bracket sequence
            let mut cursor = 1;
            let mut found_closing_bracket = false;
            loop {
                match input.get(cursor){
                    Some(b'=') => {
                        // level should go up
                        cursor += 1;
                    },
                    Some(b'[') => {
                        // found bracket
                        found_closing_bracket = true;
                        break;
                    },
                    _ => {
                        // actually not an opening bracket
                        break;
                    }
                }
            }
            if found_closing_bracket {
                QuoteMode::BlockQuote(cursor-1)
            } else {
                QuoteMode::Nothing
            }
        },
        _ => QuoteMode::Nothing
    };

    match quote_mode {
        QuoteMode::Nothing => { return None; },
        QuoteMode::SimpleQuote(q) => {
            let string_contents_start = 1;
            let mut cursor = 1;
            // let's iterate until we find the end
            loop {
                match input.get(cursor) {
                    // failed parse
                    None => {return None;},
                    Some(b'\\') => {
                        // escape character escapes next char
                        // so jump over it
                        cursor += 2;
                    },
                    Some(s) if s == &q => {
                        // found end point
                        // this is messy and will likely not work correctly
                        let string_contents = std::str::from_utf8(&input[string_contents_start..cursor]).unwrap();
                        let remaining_input = std::str::from_utf8(&input[cursor..]).unwrap();
                        return Some((
                            string_contents,
                            remaining_input,
                        ));
                    },
                    Some(_) => {
                        // just move to next character
                        cursor += 1;
                    }
                }
            }
        },
        QuoteMode::BlockQuote(level) => {
            //[===[
            // objective is to find the closing 
            // ]===]
            let string_contents_start = level + 2;
            let mut cursor = string_contents_start;

            loop {
                match input.get(cursor) {
                    None => { return None; },
                    Some(b']') => {
                        // check if this is the closing token
                        let is_all_equals = {
                            let mut all_eq = true;
                            for i in cursor+1..cursor+level+1 {
                                match input.get(i){
                                    Some(b'=') => {},
                                    _ => {
                                        all_eq = false;
                                        break;
                                    }
                                }
                            }
                            all_eq
                        };

                        let has_closing_bracket = match input.get(cursor+level+1) {
                            Some(b']') => true,
                            _ => false
                        };
                        if is_all_equals && has_closing_bracket {
                            // this is the closing bracket, give the input slices
                            return Some((
                                std::str::from_utf8(
                                    &input[string_contents_start..cursor]
                                ).unwrap(),
                                std::str::from_utf8(
                                    // cursor on first ]
                                    // second ] is on cursor + 1 + level
                                    &input[
                                        cursor + level + 2..
                                    ]
                                ).unwrap()
                            ))
                        } else {
                            cursor += 1
                        }
                    },
                    Some(_) => {
                        cursor += 1
                    }
                }
            }
        }
    }
}

pub fn control_character(input: &str) -> IResult<&str, char> {
    // parse out control character after slash
    return alt((
        map(char_parse('n'), |_| '\n'),
        map(char_parse('\\'), |_| '\\'),
        anychar,
    ))(input);
}

pub fn short_string(input: &str) -> IResult<&str, Lex>{
    // get start quote
    let (input, start_quote) = alt((
        char_parse('\''),
        char_parse('"'),
    ))(input)?;

    // start parsing all the content
    
    let no_quotes: &str = &(start_quote.to_string() + "\\");

    let non_quote_parser = map(
        none_of(no_quotes), |c| c
    );

    let slash_parser = map(
        char_parse('\\'), |e| e
    );

    let parse_out_short_string = alt((
        non_quote_parser,
        preceded(slash_parser, control_character)
    ));

    let (input, string_contents) = many0(
        parse_out_short_string
    )(input)?;

    let (input, _) = char_parse(start_quote)(input)?;

    return Ok((input, Lex::Str(string_contents.iter().collect())));
}


pub fn long_string(input: &str) -> IResult<&str, Lex> {
    use super::super::parse::surrounded;
    // first let's find the opening brackets
    let (input, equals_level) = surrounded(
        tag("["),
        many0(char_parse('=')),
        tag("[")
    )(input)?;

    let mut cl_bkt = "]".to_owned();
    let eq: String = equals_level.into_iter().collect();
    cl_bkt.push_str(&eq);
    cl_bkt.push_str("]");
    
    let closing_bracket= tag(cl_bkt.as_str());
    let (input, (string_contents, _)) = many_till(
        anychar,
        closing_bracket,
    )(input)?;

    return Ok((input, Lex::Str(string_contents.into_iter().collect())));
}

pub fn parse_string(input: &str) -> IResult<&str, Lex> {
    return alt((
        short_string,
        long_string,
    ))(input);
}

// pub fn parse_string(input: &str) -> IResult<&str, Lex> {
//     match parse_out_string_contents(input) {
//         None => Err(Err::Error((input, ErrorKind::Tag))),
//         Some((
//             string_contents,
//             remainder
//             // OMG the next line is a mess
//         )) => Ok((&remainder, Lex::Str(string_contents.to_string())))
//     }
// }