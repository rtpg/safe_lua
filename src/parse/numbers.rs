use crate::eval::LNum;

struct HexStrComp<'a> {
    whole_part: &'a str,
    fractional_part: Option<&'a str>,
    exponent_part: Option<&'a str>,
}

fn get_componenets(s: &str) -> Result<HexStrComp, String> {
    // let's get the exponent first
    let exponent_splitter = if s.contains("p") { "p" } else { "P" };
    let exponent_part: Option<&str>;
    let exponent_components: Vec<&str> = s.split(exponent_splitter).collect();
    let mut remaining: &str = s;
    match exponent_components.len() {
        1 => {
            exponent_part = None;
        } // no component
        2 => {
            exponent_part = Some(exponent_components[1]);
            remaining = exponent_components[0];
        }
        _ => return Err("Invalid components".to_string()),
    };

    // we have the exponent part now, lets check for the other part
    let fractional_components: Vec<&str> = remaining.split(".").collect();
    let fractional_part: Option<&str>;
    let whole_part: &str;
    match fractional_components.len() {
        1 => {
            fractional_part = None;
            whole_part = remaining;
        }
        2 => {
            fractional_part = Some(fractional_components[1]);
            whole_part = fractional_components[0];
        }
        _ => {
            return Err("invalid components (fractional)".to_string());
        }
    }
    return Ok(HexStrComp {
        whole_part,
        fractional_part,
        exponent_part,
    });
}
/**
 * Parse a floating hexdecimal number
 * (no negatives need to be handled here)
 **/
fn parse_lua_hex_internal(s: &str) -> Result<LNum, String> {
    let hex_str_comp: HexStrComp;
    match get_componenets(s) {
        Ok(hsc) => hex_str_comp = hsc,
        Err(e) => return Err(e),
    }

    // at this point we are going to build up the final value

    // TODO this could be more performant with cleverness
    let components: Vec<_> = s.split(".").collect();
    if components.len() == 0 {
        return Err("Empty string cannot be hex parsed".to_string());
    }
    if hex_str_comp.fractional_part.is_none() && hex_str_comp.exponent_part.is_none() {
        // this is actually an integer, treat it as such
        let result_i64 = i64::from_str_radix(hex_str_comp.whole_part, 16);
        match result_i64 {
            Ok(v) => return Ok(LNum::Int(v)),
            Err(e) => return Err(e.to_string()),
        }
    }

    // otherwise we are actually going to be using a fractional component
    let whole_part_s = hex_str_comp.whole_part;
    let mut result: f64;
    match i64::from_str_radix(whole_part_s, 16) {
        Ok(v) => result = v as f64,
        Err(e) => return Err(e.to_string()),
    }
    match hex_str_comp.fractional_part {
        None => {}
        Some(v) => {
            match i64::from_str_radix(v, 16) {
                Ok(m) => {
                    // the fractional component is equal to 10^(-n) * the component
                    // 0.4 => 4 * 10^-1
                    // 0.40 => 4 * 10^-2
                    let fractional_component: f64 =
                        (m as f64) * (10 as f64).powf(-(components[1].len() as f64));
                    result = result + fractional_component;
                }
                Err(e) => return Err(e.to_string()),
            }
        }
    }

    match hex_str_comp.exponent_part {
        None => {}
        Some(exp_s) => {
            match i64::from_str_radix(exp_s, 16) {
                Ok(m) => {
                    // this exponent component does *2^(val)
                    let multiplier = (2 as f64).powf(m as f64);
                    result = result * multiplier;
                }
                Err(e) => return Err(e.to_string()),
            }
        }
    }
    return Ok(LNum::Float(result));
}

pub fn parse_lua_hex(n: &str) -> Result<LNum, String> {
    let start_str = if n.starts_with("0x") { "0x" } else { "0X" };
    if n.starts_with(start_str) {
        let hex_wo_pfx = n.trim_start_matches(start_str);
        return parse_lua_hex_internal(hex_wo_pfx);
    } else {
        return Err("not a hex string".to_string());
    }
}

#[test]
fn test_hex_parsing() {
    assert_eq!(parse_lua_hex(&String::from("0x4")), Ok(LNum::Int(4)));
    assert_eq!(parse_lua_hex(&String::from("0x10")), Ok(LNum::Int(16)));
    assert_eq!(
        parse_lua_hex(&String::from("0x10.5")),
        Ok(LNum::Float(16.5))
    );
    assert_eq!(parse_lua_hex("0x1p4"), Ok(LNum::Float(16.0)));
    assert_eq!(
        parse_lua_hex(&String::from("10.5")),
        Err(String::from("not a hex string"))
    );
}
