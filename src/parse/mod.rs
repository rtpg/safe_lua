extern crate nom;
extern crate nom_packrat;
extern crate nom_recursive;

mod binop;
mod expr;
mod func;
pub mod numbers;
pub mod utils;

use nom::sequence::terminated;
use nom_locate::LocatedSpan;
use parse::expr::expr;

use parse::func::{funcbody, funcname};
use parse::utils::surrounded;

use lex::lex_all;

use lex;
use parse::nom::multi::separated_list;
use parse::nom::sequence::separated_pair;
use parse::nom::sequence::tuple;

use parse::nom::combinator::opt;

use ast;
use ast::HasLoc;
use lex::IStream;
use lex::LexInput;
use parse::nom::multi::many0;
use parse::nom::sequence::{pair, preceded};

use parse::nom::{branch::alt, combinator::map, error::ErrorKind, Err, IResult};

// impl AsBytes for &IStream {

// }

// impl Compare for &IStream {

// }

#[allow(dead_code)]
fn chunk<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Chunk<'a>> {
    let (i, bl) = block(i)?;
    return Ok((i, ast::Chunk { block: bl }));
}

#[allow(dead_code)]
fn num_component(i: char) -> bool {
    return i.is_numeric();
}

pub fn err<'a, 'b, T>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, T> {
    return Err(Err::Error((i, ErrorKind::Alpha)));
}

pub fn err_str<T>(i: LexInput) -> IResult<LexInput, T> {
    return Err(Err::Error((i, ErrorKind::Alpha)));
}

fn num_parser<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Expr<'a>> {
    match i.get(0) {
        Some(Lex {
            location: loc,
            val: Number(n),
        }) => return Ok((&i[1..], ast::Expr::Numeral(n.to_string(), *loc))),
        _ => return err(i),
    }
}

fn literal_string_parser<'a, 'b>(
    i: &'b IStream<'a>,
) -> IResult<&'b IStream<'a>, (String, LocatedSpan<&'a str>)> {
    match i.get(0) {
        Some(Lex {
            location: loc,
            val: Str(n),
        }) => return Ok((&i[1..], (n.to_string(), *loc))),
        _ => return Err(Err::Error((i, ErrorKind::Alpha))),
    }
}

use lex::Lex;
use lex::LexValue::*;

fn name<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, String> {
    let (i, v) = name_with_loc(i)?;
    return Ok((i, v.0));
}

fn name_with_loc<'a, 'b>(
    i: &'b IStream<'a>,
) -> IResult<&'b IStream<'a>, (String, LocatedSpan<&'a str>)> {
    match i.get(0) {
        Some(Lex {
            location: loc,
            val: Name(n),
        }) => return Ok((&i[1..], (n.to_string(), *loc))),
        _ => return Err(Err::Error((i, ErrorKind::Alpha))),
    }
}

fn kwd<'a, 'b, 's>(
    tag: &'s str,
) -> impl Fn(&'b IStream<'a>) -> IResult<&'b IStream<'a>, lex::Lex<'a>>
where
    'a: 'b,
{
    let local_tag = String::from(tag);
    return move |i: &'b IStream<'a>| {
        let s = &local_tag;
        let token = i.get(0);
        match token {
            Some(Lex {
                location: loc,
                val: lex::LexValue::Keyword(_s),
            }) => {
                if s == _s {
                    return Ok((
                        &i[1..],
                        lex::Lex {
                            location: *loc,
                            val: Keyword(_s.to_string()),
                        },
                    ));
                } else {
                    return Err(Err::Error((i, ErrorKind::Tag)));
                }
            }
            // HACK I fucked up and made keywords and symbols
            // just cast everything to a keyword
            Some(Lex {
                location: loc,
                val: Symbol(_s),
            }) => {
                if s == _s {
                    return Ok((
                        &i[1..],
                        Lex {
                            location: *loc,
                            val: Keyword(_s.to_string()),
                        },
                    ));
                } else {
                    return Err(Err::Error((i, ErrorKind::Tag)));
                }
            }
            _ => return Err(Err::Error((i, ErrorKind::Tag))),
        }
    };
}

fn parlist_w_namelist<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Parlist> {
    let (i, namelist) = separated_list(kwd(","), name)(i)?;
    let (i, maybe_ellipsis) = opt(pair(kwd(","), kwd("...")))(i)?;

    return Ok((
        i,
        ast::Parlist {
            namelist: namelist,
            has_ellipsis: maybe_ellipsis.is_some(),
        },
    ));
}

fn parlist_wo_namelist<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Parlist> {
    let (i, _) = kwd("...")(i)?;
    return Ok((
        i,
        ast::Parlist {
            namelist: vec![],
            has_ellipsis: true,
        },
    ));
}

fn parlist<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Parlist> {
    return alt((parlist_wo_namelist, parlist_w_namelist))(i);
}

fn exprlist<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Exprlist<'a>> {
    return separated_list(kwd(","), expr)(i);
}

fn args<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Args<'a>> {
    return alt((
        map(table_constructor, |(t, _)| ast::Args::Table(t)),
        map(literal_string_parser, |(s, _)| ast::Args::Literal(s)),
        map(surrounded(kwd("("), opt(exprlist), kwd(")")), |maybe_e| {
            ast::Args::List(maybe_e)
        }),
    ))(i);
}

fn prefix<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Prefix<'a>> {
    return alt((
        map(surrounded(kwd("("), expr, kwd(")")), |e| {
            ast::Prefix::ParenedExpr(e)
        }),
        map(name_with_loc, |(n, loc)| ast::Prefix::Varname(n, loc)),
    ))(i);
}

fn suffix<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Suffix<'a>> {
    use ast::Suffix::*;
    return alt((
        map(preceded(kwd("."), name), |n| DotAccess(n)),
        map(surrounded(kwd("["), expr, kwd("]")), |e| ArrAccess(e)),
        map(
            pair(opt(preceded(kwd(":"), name)), args),
            |(opt_name, a)| MethodCall(opt_name, a),
        ),
    ))(i);
}
fn prefixexp<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Prefixexpr<'a>> {
    let (i, pref) = prefix(i)?;
    let (i, suffixes) = many0(suffix)(i)?;
    return Ok((
        i,
        ast::Prefixexpr {
            prefix: pref,
            suffixes: suffixes,
        },
    ));
}

fn namelist<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Namelist> {
    return separated_list(kwd(","), name)(i);
}

fn var<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Var<'a>> {
    // hacky: it's likely that the best thing to do here is
    // just accept prefixexprs instead of this failure model

    let (i, expression) = prefixexp(i)?;
    // let's check that this finishes in the right way
    match expression.suffixes.len() {
        0 => {
            match expression.prefix {
                // if there are no suffixes we're working just on a prefix
                ast::Prefix::Varname(name, location) => {
                    return Ok((i, ast::Var::N(name, location)))
                }
                // if it's a paren, that's not valid and we fail
                ast::Prefix::ParenedExpr(_) => {
                    return err(i);
                }
            }
        }
        _ => {
            // not empty list, we need to check that the last expression is usable

            let last_suffix = &expression.suffixes[expression.suffixes.len() - 1];

            let var_prefix = &expression.suffixes[0..expression.suffixes.len() - 1];

            match last_suffix {
                ast::Suffix::MethodCall(_, _) => {
                    // not allowd to use a method call as an lvalue
                    return err(i);
                }
                ast::Suffix::DotAccess(name) => {
                    return Ok((
                        i,
                        ast::Var::DotAccess(
                            ast::Prefixexpr {
                                prefix: expression.prefix,
                                suffixes: var_prefix.to_vec(),
                            },
                            name.to_string(),
                        ),
                    ))
                }
                ast::Suffix::ArrAccess(expr) => {
                    return Ok((
                        i,
                        ast::Var::ArrAccess(
                            ast::Prefixexpr {
                                prefix: expression.prefix,
                                suffixes: var_prefix.to_vec(),
                            },
                            // I bet there's a better mechanism there
                            expr.clone(),
                        ),
                    ));
                }
            }
        }
    }
}
// return alt((
//     map(
//         pair(prefixexp,
//             surrounded(
//                 kwd("["),
//                 expr,
//                 kwd("]")
//             )),
//         |(p, e)| ast::Var::ArrAccess(p, e),
//     ),
//     map(
//         pair(
//             prefixexp,
//             preceded(kwd("."), name)
//         ),
//         |(p, n)| ast::Var::DotAccess(p, n),
//     ),
//     map(name, |n| ast::Var::N(n)),
// ))(i);

fn varlist<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Varlist<'a>> {
    return map(separated_list(kwd(","), var), |l| ast::Varlist { vars: l })(i);
}

fn funcdecl<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Stat<'a>> {
    return map(
        preceded(kwd("function"), pair(funcname, funcbody)),
        |(n, b)| {
            let loc = n.loc.clone();
            return ast::Stat {
                v: ast::StatV::FuncDecl(n, b),
                loc: loc,
            };
        },
    )(i);
}

fn stat<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Stat<'a>> {
    return alt((
        map(kwd(";"), |k| ast::Stat {
            v: ast::StatV::Semicol,
            loc: k.location,
        }),
        map(
            separated_pair(varlist, kwd("="), exprlist),
            |(varlist, explist)| {
                let the_loc = HasLoc::loc(&varlist).clone();
                return ast::Stat {
                    v: ast::StatV::Eql(varlist, explist),
                    loc: the_loc.clone(),
                };
            },
        ),
        // FFFFFFFFFFFFFFF
        // prefix expressions capture function calls
        // this is more open than lua's normal syntax
        map(expr, |e| {
            let the_loc = e.loc();
            ast::Stat {
                v: ast::StatV::RawExpr(e),
                loc: the_loc,
            }
        }),
        map(
            surrounded(kwd("::"), name_with_loc, kwd("::")),
            |(name, loc)| ast::Stat {
                v: ast::StatV::Label(name),
                loc: loc,
            },
        ),
        map(kwd("break"), |b| ast::Stat {
            v: ast::StatV::Break,
            loc: b.location,
        }),
        map(preceded(kwd("goto"), name_with_loc), |(n, l)| ast::Stat {
            v: ast::StatV::Goto(n),
            loc: l,
        }),
        map(
            pair(surrounded(kwd("repeat"), block, kwd("until")), expr),
            |(b, e)| ast::Stat {
                v: ast::StatV::Repeat(b.clone(), e),
                loc: b.clone().loc().clone(),
            },
        ),
        map(surrounded(kwd("do"), block, kwd("end")), |b| {
            let loc = b.loc();
            return ast::Stat {
                v: ast::StatV::Do(b),
                loc: loc,
            };
        }),
        map(
            pair(
                preceded(kwd("while"), expr),
                surrounded(kwd("do"), block, kwd("end")),
            ),
            |(e, b)| {
                let loc = e.loc();
                return ast::Stat {
                    v: ast::StatV::While(e, b),
                    loc: loc,
                };
            },
        ),
        funcdecl,
        map(
            tuple((
                surrounded(kwd("if"), expr, kwd("then")),
                block,
                many0(pair(surrounded(kwd("elseif"), expr, kwd("then")), block)),
                opt(preceded(kwd("else"), block)),
                kwd("end"),
            )),
            |(p, t, ei_l, e_b, _)| {
                let loc = p.loc();
                return ast::Stat {
                    v: ast::StatV::If {
                        predicate: p,
                        then_block: t,
                        elif_list: ei_l,
                        else_block: e_b,
                    },
                    loc: loc,
                };
            },
        ),
        map(
            tuple((
                surrounded(kwd("for"), name_with_loc, kwd("=")),
                separated_pair(expr, kwd(","), expr),
                opt(preceded(kwd(","), expr)),
                surrounded(kwd("do"), block, kwd("end")),
            )),
            |((n, loc), (e1, e2), m_e3, b)| ast::Stat {
                v: ast::StatV::For(n, e1, e2, m_e3, b),
                loc: loc,
            },
        ),
        map(
            tuple((
                pair(kwd("for"), terminated(namelist, kwd("in"))),
                exprlist,
                surrounded(kwd("do"), block, kwd("end")),
            )),
            |((k, n), el, b)| ast::Stat {
                v: ast::StatV::ForIn(n, el, b),
                loc: k.location,
            },
        ),
        map(
            preceded(
                pair(kwd("local"), kwd("function")),
                pair(name_with_loc, funcbody),
            ),
            |((n, loc), b)| ast::Stat {
                v: ast::StatV::LocalFuncDecl(n, b),
                loc: loc,
            },
        ),
        map(
            pair(
                pair(kwd("local"), namelist),
                opt(preceded(kwd("="), exprlist)),
            ),
            |((k, nl), m_el)| ast::Stat {
                v: ast::StatV::LocalNames(nl, m_el),
                loc: k.location,
            },
        ),
    ))(i);
}

fn retstat<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Retstat<'a>> {
    let (i, m_elist) = surrounded(kwd("return"), opt(exprlist), opt(kwd(";")))(i)?;
    return Ok((
        i,
        ast::Retstat {
            return_expr: m_elist,
        },
    ));
}

fn block<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Block<'a>> {
    let (i, stats) = many0(stat)(i)?;
    let (i, m_r) = opt(retstat)(i)?;
    Ok((
        i,
        ast::Block {
            stats: stats,
            retstat: m_r,
        },
    ))
}

fn function_def<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Expr<'a>> {
    let (i, _) = kwd("function")(i)?;
    let (i, f_b) = funcbody(i)?;
    return Ok((i, ast::Expr::Functiondef(f_b)));
}

fn field<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Field<'a>> {
    // '[' exp ']' '=' exp
    let bracked_field = map(
        pair(
            surrounded(kwd("["), expr, kwd("]")),
            preceded(kwd("="), expr),
        ),
        |(e1, e2)| ast::Field::Bracketed(e1, e2),
    );
    // Name '=' exp
    let plain_eq = map(separated_pair(name, kwd("="), expr), |(n, e)| {
        ast::Field::Named(n, e)
    });

    return alt((bracked_field, plain_eq, map(expr, |e| ast::Field::Raw(e))))(i);
}

fn fieldlist<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Fieldlist<'a>> {
    let fieldsep = alt((kwd(","), kwd(";")));
    let (i, fields) = separated_list(fieldsep, field)(i)?;
    let (i, _) = opt(alt((kwd(","), kwd(";"))))(i)?;
    return Ok((i, fields));
}

fn table_constructor<'a, 'b>(
    i: &'b IStream<'a>,
) -> IResult<&'b IStream<'a>, (ast::Tableconstructor<'a>, LocatedSpan<&'a str>)> {
    let (i, loc) = kwd("{")(i)?;
    let (i, maybe_fieldlist) = opt(fieldlist)(i)?;
    let (i, _) = kwd("}")(i)?;
    return Ok((i, (maybe_fieldlist, loc.location)));
}

/**
 * Attempt parsing a string, return None if it fails
 *
 * a non-panic version of parse
 */
pub fn try_parse<'a>(input: &'a str) -> Result<ast::Block<'a>, String> {
    let input = LexInput::new(&input);
    let (input, tokens) = lex_all(input).unwrap();
    if input.to_string().len() > 0 {
        // failed lex
        return Err("Failed to lex all input".to_string());
    }
    let (remaining_tokens, b) = block(&tokens).unwrap();
    if remaining_tokens.len() > 0 {
        // failed parse
        return Err("Failed to parse all input".to_string());
    }
    return Ok(b);
}

#[cfg(test)]
pub fn try_specific_parse<'a, 'b, ParseType>(
    parser: impl for<'c> Fn(&'c IStream<'a>) -> IResult<&'c IStream<'a>, ParseType>,
    input: &'a str,
) -> Result<ParseType, String> {
    // try parsing with a specific parser
    let input = LexInput::new(&input);
    let (input, tokens) = lex_all(input).unwrap();
    if input.to_string().len() > 0 {
        return Err("failed to lex all input".to_string());
    }
    let (remaining_tokens, result) = parser(&tokens).unwrap();
    if remaining_tokens.len() > 0 {
        return Err("Failed to parse all input".to_string());
    }
    return Ok(result);
}

pub fn parse<'a>(input: &'a str) -> ast::Block<'a> {
    let input = LexInput::new(input);
    let (input, tokens) = lex_all(input).unwrap();
    if input.to_string().len() > 0 {
        dbg!(&input.to_string()[..20]);
        panic!("remaining input");
    }
    let (remaining_tokens, b) = block(&tokens).unwrap();
    if remaining_tokens.len() > 0 {
        for t in &remaining_tokens[..30] {
            println!("line {}: {:?}", t.location.location_line(), t.val);
        }
        panic!("Remaining tokens");
    }
    return b;
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    use file_contents;

    use std::fs::File;
    use std::io::Read;

    macro_rules! assert_parse_all {
        ($parser:expr, $input:expr) => {
            let (remaining_input, lexed) = lex_all(LexInput::new($input)).unwrap();
            if remaining_input.to_string().len() != 0 {
                dbg!(&remaining_input.to_string()[..remaining_input.to_string().len().min(10)]);
                panic!("Input remained from lexing")
            }
            let (remaining_tokens, _) = $parser(&lexed).unwrap();
            if remaining_tokens.len() != 0 {
                dbg!(&remaining_tokens[..remaining_tokens.len().min(10)]);
                panic!("Input remained from parsing")
            };
        };
    }

    #[test]
    fn test_snippets() {
        assert_parse_all!(
            block,
            r#"
local dummy
local _ENV = (function (...) return ... end)(_G, dummy)   -- {

do local _ENV = {assert=assert}; assert(true) end
mt = {_G = _G}
local foo,x
A = false    -- "declare" A
do local _ENV = mt
  function foo (x)
    A = x
    do local _ENV =  _G; A = 1000 end
    return function (x) return A .. x end
  end
end
assert(getenv(foo) == mt)
x = foo('hi'); assert(mt.A == 'hi' and A == 1000)
assert(x('*') == mt.A .. '*')

do local _ENV = {assert=assert, A=10};
  do local _ENV = {assert=assert, A=20};
    assert(A==20);x=A
  end
  assert(A==10 and x==20)
end
assert(x==20)


print('OK')

return 5,f

            "#
        );

        assert_parse_all!(expr, "assert(a.t:x(2,3) == -95)");

        assert_parse_all!(expr, "function (x) return A .. x end");

        assert_parse_all!(prefixexp, "c");

        assert_parse_all!(expr, "2");

        assert_parse_all!(var, "c[2]");

        assert_parse_all!(varlist, "c[2], a[b]");

        assert_parse_all!(block, "c[2], a[b] = -((a + d/2 - a[b]) ^ a.x), b");

        assert_parse_all!(
            block,
            "
check(function ()
  local a,b,c,d
  a = b*2
  c[2], a[b] = -((a + d/2 - a[b]) ^ a.x), b
end,
  'LOADNIL',
  'MUL',
  'DIV', 'ADD', 'GETTABLE', 'SUB', 'GETTABLE', 'POW',
    'UNM', 'SETTABLE', 'SETTABLE', 'RETURN')

            "
        );
    }
    #[test]
    fn test_expr_parse() {
        assert_parse_all!(block, "for i = 1, #arg do res = res & arg[i] end");

        assert_parse_all!(funcname, "foo.bar");

        assert_parse_all!(parlist, "...");

        assert_parse_all!(funcbody, "(a) return a end");

        assert_parse_all!(expr, "{...}");

        assert_parse_all!(parlist, "x, y, z, ...");

        assert_parse_all!(
            block,
            "local arg = {...}
             local res = x & y & z"
        );

        assert_parse_all!(
            block,
            "
              if not z then
    return ((x or -1) & (y or -1)) & 0xFFFFFFFF
  else
    local arg = {...}
    local res = x & y & z
    for i = 1, #arg do res = res & arg[i] end
    return res & 0xFFFFFFFF
  end"
        );

        assert_parse_all!(funcdecl, "function foo.bar (x) return 3 end");

        assert_parse_all!(block, "return bit.band(...) ~= 0");

        assert_parse_all!(
            funcdecl,
            "
function bit.btest (...)
  return bit.bnd(...) ~= 0
end
            "
        );

        assert_parse_all!(
            block,
            r#"
            
-- no built-in 'bit32' library: implement it using bitwise operators

local bit = {}

function bit.bnot (a)
  return ~a & 0xFFFFFFFF
end


--
-- in all vararg functions, avoid creating 'arg' table when there are
-- only 2 (or less) parameters, as 2 parameters is the common case
--

function bit.band (x, y, z, ...)
  if not z then
    return ((x or -1) & (y or -1)) & 0xFFFFFFFF
  else
    local arg = {...}
    local res = x & y & z
    for i = 1, #arg do res = res & arg[i] end
    return res & 0xFFFFFFFF
  end
end

function bit.bor (x, y, z, ...)
  if not z then
    return ((x or 0) | (y or 0)) & 0xFFFFFFFF
  else
    local arg = {...}
    local res = x | y | z
    for i = 1, #arg do res = res | arg[i] end
    return res & 0xFFFFFFFF
  end
end

function bit.bxor (x, y, z, ...)
  if not z then
    return ((x or 0) ~ (y or 0)) & 0xFFFFFFFF
  else
    local arg = {...}
    local res = x ~ y ~ z
    for i = 1, #arg do res = res ~ arg[i] end
    return res & 0xFFFFFFFF
  end
end

function bit.btest (...)
  return bit.band(...) ~= 0
end

function bit.lshift (a, b)
  return ((a & 0xFFFFFFFF) << b) & 0xFFFFFFFF
end

function bit.rshift (a, b)
  return ((a & 0xFFFFFFFF) >> b) & 0xFFFFFFFF
end

function bit.arshift (a, b)
  a = a & 0xFFFFFFFF
  if b <= 0 or (a & 0x80000000) == 0 then
    return (a >> b) & 0xFFFFFFFF
  else
    return ((a >> b) | ~(0xFFFFFFFF >> b)) & 0xFFFFFFFF
  end
end

function bit.lrotate (a ,b)
  b = b & 31
  a = a & 0xFFFFFFFF
  a = (a << b) | (a >> (32 - b))
  return a & 0xFFFFFFFF
end

function bit.rrotate (a, b)
  return bit.lrotate(a, -b)
end

local function checkfield (f, w)
  w = w or 1
  assert(f >= 0, "field cannot be negative")
  assert(w > 0, "width must be positive")
  assert(f + w <= 32, "trying to access non-existent bits")
  return f, ~(-1 << w)
end

function bit.extract (a, f, w)
  local f, mask = checkfield(f, w)
  return (a >> f) & mask
end

function bit.replace (a, v, f, w)
  local f, mask = checkfield(f, w)
  v = v & mask
  a = (a & ~(mask << f)) | (v << f)
  return a & 0xFFFFFFFF
end

return bit
            "#
        );

        assert_parse_all!(
            block,
            r#"
function bit.band (x, y, z, ...)
  if not z then
    return ((x or -1) & (y or -1)) & 0xFFFFFFFF
  else
    local arg = {...}
    local res = x & y & z
    for i = 1, #arg do res = res & arg[i] end
    return res & 0xFFFFFFFF
  end
end
            "#
        );
        assert_parse_all!(
            expr,
            r#"
f(x, 3 + 5, y[z].foo.bar)
            "#
        );

        assert_parse_all!(
            suffix,
            r#"
(x, 3)
            "#
        );

        assert_parse_all!(exprlist, "x.y[z],y,3");

        assert_parse_all!(expr, "3");

        assert_parse_all!(expr, "print \"testing syntax\"");

        assert_parse_all!(args, "\"some string \"");

        assert_parse_all!(expr, "print \"testing syntax\"");

        assert_parse_all!(
            block,
            r#"
; do ; a = 3; assert(a == 3) end;
            "#
        );

        assert_parse_all!(expr, "2.0^-2 == 1/4 and -2^- -2 == - - -4");

        assert_parse_all!(retstat, "return i, 'jojo';");

        assert_parse_all!(
            stat,
            r#"
           if type(i) ~= 'number' then return i,'jojo'; end 
            "#
        );

        assert_parse_all!(expr, "string.format(prog, s, s)");

        assert_parse_all!(
            block,
            r#"
              local s = v[1]
    local p = load(string.format(prog, s, s), "")
    IX = false
    assert(p() == v[2] and IX == not not v[2])
    i = i + 1
    if i % 60000 == 0 then print('+') end  
            "#
        );

        assert_parse_all!(
            block,
            r#"
  if type(i) ~= 'number' then return i,'jojo'; end;
  if i > 0 then return i, f(i-1); end;
            "#
        );

        assert_parse_all!(
            function_def,
            r#"
            function (i)
  if i < 10 then return 'a'
  elseif i < 20 then return 'b'
  elseif i < 30 then return 'c'
  else return 8
  end
end
"#
        );
    }
    #[test]
    fn test_full_parse() {
        let input = r#"
local x = 3;
        "#;
        let (_, lexed) = lex_all(LexInput::new(input)).unwrap();
        println!("Successfully lexed");
        dbg!(stat(&lexed)).unwrap();
        // parse(input) ;
    }

    #[test]
    fn test_kwd_parse() {
        let lexed = lex_all(LexInput::new(",")).unwrap().1;

        match kwd(",")(&lexed) {
            Ok(([], Lex { val: v, .. })) => {
                assert_eq!(v, lex::LexValue::Keyword(",".to_string()))
            }
            _ => panic!("Parsing failed"),
        };
    }

    #[test]
    fn test_parsing_of_lua_files() {
        file_contents!("lua_tests/constructs.lua", contents);

        // parse panics on failures so....
        parse(contents.as_str());
    }

    #[test]
    fn test_parsing_of_all_test_files() {
        use std::fs;

        let file_paths = fs::read_dir("lua_tests/.").unwrap();
        let mut last_failure: Option<std::path::PathBuf> = None;
        let mut test_count = 0;
        let mut failure_count = 0;

        for path in file_paths {
            let unwrapped_path = path.unwrap().path();

            // we don't handle UTF-8 stuff yet
            // let files_w_non_utf8 = vec![
            // "files.lua", "pm.lua",
            // ];

            // UTF handling
            let _attempted_utf8_check = File::open(unwrapped_path.clone());

            let can_read_file = {
                let mut f = File::open(unwrapped_path.clone()).unwrap();
                let mut buf = String::new();
                match f.read_to_string(&mut buf) {
                    Err(_) => false,
                    Ok(_) => true,
                }
            };

            test_count += 1;
            if !can_read_file {
                println!("Skipping {} for non-UTF8", unwrapped_path.display());
                failure_count += 1;
                continue;
            }

            // for f in files_w_non_utf8 {
            // if unwrapped_path.display().to_string().contains(f) {
            // continue;
            // }
            // }

            println!("Parsing {}...", unwrapped_path.display());

            file_contents!(unwrapped_path.clone(), contents);
            match try_parse(contents.as_str()) {
                Err(err) => {
                    // failed parse
                    println!("Failed with {}!", err);
                    failure_count += 1;
                    last_failure = Some(unwrapped_path);
                }
                Ok(_) => {
                    // succeeded parse
                    println!("success");
                }
            };
        }

        match last_failure {
            None => {
                //passed, nothing to do
            }
            Some(filepath) => {
                println!(
                    "Parsed {} files, with {} failures",
                    test_count, failure_count
                );
                println!("Re-attempt failed parse of {}", filepath.display());

                file_contents!(filepath, contents);
                parse(contents.as_str());
            }
        }
    }

    #[test]
    fn test_parse() {
        let input = "nil";
        let l: Vec<lex::Lex> = lex_all(LexInput::new(input)).unwrap().1;
        let result = expr(&l);
        match result {
            Ok((vec, ast)) => {
                assert_eq!(vec.len(), 0);
                match ast {
                    ast::Expr::Nil(loc) => {
                        assert_eq!(loc.location_offset(), 3);
                        assert_eq!(loc.location_line(), 1);
                    }
                    v @ _ => {
                        dbg!(v);
                        panic!("failed");
                    }
                }
            }
            e @ _ => {
                dbg!(e).unwrap();
                panic!("failed");
            }
        }

        let m: Vec<lex::Lex> = lex_all(LexInput::new("'hi this is a string with an \\' escape'"))
            .unwrap()
            .1;

        match expr(&m) {
            Ok((vec, ast)) => {
                assert_eq!(vec.len(), 0);
                match ast {
                    ast::Expr::LiteralString(s, loc) => {
                        assert_eq!(s, "hi this is a string with an ' escape");
                        assert_eq!(loc.location_offset(), 1);
                        assert_eq!(loc.location_line(), 1);
                    }
                    v @ _ => {
                        dbg!(v);
                        panic!("failed");
                    }
                }
            }
            e @ _ => {
                dbg!(e).unwrap();
                panic!("failed");
            }
        }
    }
}
