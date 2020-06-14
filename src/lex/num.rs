use nom::IResult;
use nom::character::complete::{
    digit1,
    hex_digit1
};
use nom::sequence::{
    preceded,
    pair,
};
use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::combinator::{ opt, map };
use super::{Lex, LexInput};

use super::super::parse::err_str;

fn hex_parse(i: LexInput) -> IResult<LexInput, String> {
    let (i, hex_prefix) = alt((tag("0x"), tag("0X")))(i)?;
    let (i, maybe_hex_digits) = opt(hex_digit1)(i)?;
    let (i, maybe_fraction) = opt(
        preceded(
            tag("."),
            opt(hex_digit1),
        )
    )(i)?;
    if maybe_hex_digits.is_none() {
        match maybe_fraction {
            None => {
                return err_str(i);
            },
            Some(None) => {
                return err_str(i);
            },
            Some(Some(_)) => { }
        }
    }

    let hex_digits = match maybe_hex_digits {
        None => "0",
        Some(n) => n.fragment()
    };

    let (i, maybe_exponent) = opt(
        pair(
            alt((tag("p"), tag("P"))),
            pair(
                opt(alt((tag("-"), tag("+")))),
                hex_digit1,
            )
        )
    )(i)?;

    let mut result = hex_prefix.to_string() + hex_digits;

    match maybe_fraction {
        None => {},
        Some(None) => {},
        Some(Some(fractions)) => {
           result += ".";
           result += &fractions.to_string();
        }
    }

    match maybe_exponent {
        None => {

        },
        Some(
            (exponent, (maybe_sign, digits))
        ) => {
            result += &exponent.to_string();
            match maybe_sign {
                None => {},
                Some(s) => { result += &s.to_string() }
            };
            result += &digits.to_string();
        }
    };

    return Ok((i, result));
}

pub fn parse_number(i: LexInput) -> IResult<LexInput, Lex>{
    return alt((
        map(
            hex_parse,
            |s| Lex::Number(s)
        ),
        parse_nonhex_number,
    ))(i);
}

fn parse_nonhex_number(input: LexInput) -> IResult<LexInput, Lex> {
    let (i, maybe_whole_part) = opt(digit1)(input)?;
    let (i, maybe_fraction) = opt(
        preceded(
            tag("."),
            opt(digit1)
        )
    )(i)?;
    
    // need either a digit or a . in front
    // if a . is in front we'll make the number zero
    // if we have a dot, we need either a whole part or something in the fraction, but not neither
    if maybe_whole_part.is_none() {
        match maybe_fraction {
            None => {
                // no input (no beginning of number)
                return err_str(i);
            },
            Some(None) => {
                // just .
                return err_str(i);
            },
            Some(Some(_)) => {
                // .n (valid)
            }
        }
    }
    // here either we have a dot or a whole part first
    let whole_part = match maybe_whole_part {
        Some(n) => &n.fragment(),
        None => "0",
    };

    let (i, maybe_exponent) = opt(
        pair(
            alt((tag("e"), tag("E"))),
            pair(
                opt(alt((tag("-"), tag("+")))),
                digit1
            )
        )
    )(i)?;

    let mut result = whole_part.to_string();

    match maybe_fraction {
        None => {},
        Some(None) => {}
        Some(Some(fraction)) => {
            result += ".";
            result += &fraction.to_string();
        }
    }

    match maybe_exponent {
        None => {},
        Some(
            (exponent, (maybe_sign, digits))
        ) => {
            result += &exponent.to_string();
            match maybe_sign {
                None => {},
                Some(sign) => {
                    result += &sign.to_string();
                }
            }
            result += &digits.to_string();
        }
    }

    return Ok((i, Lex::Number(result)));
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_full_parse;
    fn assert_number_parse(i: &str) {
	assert_full_parse!(
	    parse_number,
	    i,
	    Lex::Number(i.to_string())
	);
    }
    #[test]
    fn test_num_parse(){
        let valid_numbers = r#"
        3   345   0xff   0xBEBADA
        3.0     3.1416     314.16e-2     0.31416E1     34e1
        0x0.1E  0xA23p-4   0X1.921FB54442D18P+1
        "#;

        for n in valid_numbers.split_whitespace() {
            let value = n.to_string();
            assert_number_parse(&value);
        }
    }
}
