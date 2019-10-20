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
use super::Lex;

fn hex_parse(i: &str) -> IResult<&str, String> {
    let (i, hex_prefix) = alt((tag("0x"), tag("0X")))(i)?;
    let (i, hex_digits) = hex_digit1(i)?;
    let (i, maybe_fraction) = opt(
        preceded(
            tag("."),
            hex_digit1,
        )
    )(i)?;
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
        Some(fractions) => {
           result += ".";
           result += fractions;
        }
    }

    match maybe_exponent {
        None => {

        },
        Some(
            (exponent, (maybe_sign, digits))
        ) => {
            result += exponent;
            match maybe_sign {
                None => {},
                Some(s) => { result += s }
            };
            result += digits;
        }
    };

    return Ok((i, result));
}

pub fn parse_number(i: &str) -> IResult<&str, Lex>{
    return alt((
        map(
            hex_parse,
            |s| Lex::Number(s)
        ),
        parse_nonhex_number,
    ))(i);
}

fn parse_nonhex_number(input: &str) -> IResult<&str, Lex> {
    let (i, whole_part) = digit1(input)?;
    let (i, maybe_fraction) = opt(
        preceded(
            tag("."),
            digit1
        )
    )(i)?;

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
        Some(fraction) => {
            result += ".";
            result += fraction;
        }
    }

    match maybe_exponent {
        None => {},
        Some(
            (exponent, (maybe_sign, digits))
        ) => {
            result += exponent;
            match maybe_sign {
                None => {},
                Some(sign) => {
                    result += sign;
                }
            }
            result += digits;
        }
    }

    return Ok((i, Lex::Number(result)));
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_number_parse {
        ($input: expr) => {
            assert_eq!(
                parse_number(&$input.to_string()),
                Ok(("", Lex::Number($input.to_string())))
            );
        };
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
            assert_number_parse!(value);
        }
    }
}