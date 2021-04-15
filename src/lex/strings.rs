use super::*;
use nom::character::complete::anychar;
use nom::multi::many_till;

pub fn control_character(input: LexInput) -> IResult<LexInput, char> {
    // parse out control character after slash
    return alt((
        map(char_parse('n'), |_| '\n'),
        map(char_parse('\\'), |_| '\\'),
        anychar,
    ))(input);
}

pub fn short_string(input: LexInput) -> IResult<LexInput, Lex> {
    // get start quote
    let (input, start_quote) = alt((char_parse('\''), char_parse('"')))(input)?;

    let (input, loc) = position(input)?;
    // start parsing all the content

    let no_quotes: &str = &(start_quote.to_string() + "\\");

    let non_quote_parser = map(none_of(no_quotes), |c| c);

    let slash_parser = map(char_parse('\\'), |e| e);

    let parse_out_short_string = alt((non_quote_parser, preceded(slash_parser, control_character)));

    let (input, string_contents) = many0(parse_out_short_string)(input)?;

    let (input, _) = char_parse(start_quote)(input)?;

    return Ok((
        input,
        Lex {
            location: loc,
            val: LexValue::Str(string_contents.iter().collect()),
        },
    ));
}

pub fn long_string(input: LexInput) -> IResult<LexInput, Lex> {
    use parse::utils::surrounded;

    // first let's find the opening brackets
    let (input, equals_level) = surrounded(tag("["), many0(char_parse('=')), tag("["))(input)?;

    let (input, loc) = position(input)?;
    let mut cl_bkt = "]".to_owned();
    let eq: String = equals_level.into_iter().collect();
    cl_bkt.push_str(&eq);
    cl_bkt.push_str("]");

    let closing_bracket = tag(cl_bkt.as_str());
    let (input, (string_contents, _)) = many_till(anychar, closing_bracket)(input)?;

    return Ok((
        input,
        Lex {
            location: loc,
            val: LexValue::Str(string_contents.into_iter().collect()),
        },
    ));
}

pub fn parse_string(input: LexInput) -> IResult<LexInput, Lex> {
    return alt((short_string, long_string))(input);
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
