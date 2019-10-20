extern crate nom;
extern crate nom_recursive;
extern crate nom_packrat;

use lex::{
    ISlice,
    lex_all
};

use parse::nom::sequence::tuple;
use parse::nom::sequence::separated_pair;
use parse::nom::multi::{
    separated_list
};
use lex;

use parse::nom::combinator::opt;

use parse::nom::sequence::{
    preceded,
    terminated,
    pair,
};
use parse::nom::multi::many0;
use ast;
use lex::IStream;

use parse::nom::{
    IResult,
    branch::{
        alt,
    },
    combinator::{
        map
    },
    Err,
    error::ErrorKind,
};


// impl AsBytes for &IStream {

// }

// impl Compare for &IStream {

// }

#[allow(dead_code)]
fn chunk(i: &IStream) -> IResult<&IStream, ast::Chunk> {
    let (i, bl) = block(i)?;
    return Ok((i, ast::Chunk {block: bl}));
}

#[allow(dead_code)]
fn num_component(i: char) -> bool {
    return i.is_numeric();
}
fn num_parser(i: &IStream) -> IResult<&ISlice, ast::Expr> {
    match i.get(0) {
        Some(lex::Lex::Number(n)) => return Ok((
            &i[1..], ast::Expr::Numeral(n.to_string())
        )),
        _ => return Err(
                Err::Error((i, ErrorKind::Alpha))
        )
    }
}

fn literal_string_parser(i: &IStream) -> IResult<&ISlice, String> {
    match i.get(0) {
        Some(lex::Lex::Str(n)) => return Ok((
            &i[1..], n.to_string()
        )),
        _ => return Err(
                Err::Error((i, ErrorKind::Alpha))
        )
    }
}

fn name(i: &IStream) -> IResult<&ISlice, String> {
    match i.get(0) {
        Some(lex::Lex::Name(n)) => return Ok((
            &i[1..], n.to_string()
        )),
        _ => return Err(
                Err::Error((i, ErrorKind::Alpha))
        )
    }
}

fn kwd<'a>(tag: &'a str) -> impl Fn(&'a IStream) -> IResult<&'a IStream, lex::Lex> {
    move |i: &IStream| {
        let s = tag.clone();
        let token = i.get(0);
        match token{
            Some(lex::Lex::Keyword(_s)) => {
                if s == _s {
                    return Ok((&i[1..], lex::Lex::Keyword(_s.to_string())))
                } else {
                    return Err(Err::Error((i, ErrorKind::Tag)))
                }
            },
            // HACK I fucked up and made keywords and symbols
            // just cast everything to a keyword
            Some(lex::Lex::Symbol(_s)) => {
                if s == _s {
                    return Ok((&i[1..], lex::Lex::Keyword(_s.to_string())))
                } else {
                    return Err(Err::Error((i, ErrorKind::Tag)))
                }
            }
            _ => return Err(
                Err::Error((i, ErrorKind::Tag))
            )
        }
    }
}

fn parlist(i: &ISlice) -> IResult<&IStream, ast::Parlist> {
    // WRONG doesn't handle elipsis yet

    let (i, namelist) = separated_list(
        kwd(","),
        name
        )(i)?;
    return Ok((
        i,
        ast::Parlist {
            namelist: namelist,
            has_ellipsis: false,
        }
    ));
}

fn exprlist(i: &IStream) -> IResult<&IStream, ast::Exprlist>{
    return separated_list(kwd(","), expr)(i);
}

fn args(i: &IStream) -> IResult<&IStream, ast::Args> {
    return alt((
        map(table_constructor, |t| ast::Args::Table(t)),
        map(literal_string_parser, |s| ast::Args::Literal(s)),
        map(
            surrounded(
                kwd("("), opt(exprlist), kwd(")")
            ),
            |maybe_e| ast::Args::List(maybe_e),
        )
    ))(i);
}

fn funcbody(i: &IStream) -> IResult<&IStream, ast::Funcbody> {
    let (i, m_parlist) = surrounded(
        kwd("("),
        opt(parlist),
        kwd(")"))(i)?;
    let (i, (b, _)) = pair(block, kwd("end"))(i)?;
    return Ok((i, ast::Funcbody {parlist: m_parlist, body: b}));
}

fn funccall(i: &IStream) -> IResult<&IStream, ast::Funccall> {
    let (i, p) = prefixexp(i)?;
    let (i, maybe_name) = opt(
        preceded(kwd(":"), name)
    )(i)?;
    let (i, a) = args(i)?;
    return Ok((i, ast::Funccall {expr: p, 
                                 command_name: maybe_name,
                                 args: a}));
}

pub fn surrounded<T, U, V, I, O, O1, O2>(
    left_parser: T, inner: U, right_parser: V) -> impl Fn(I) -> IResult<I, O>
 where T: Fn(I) -> IResult<I, O1>,
       U: Fn(I) -> IResult<I, O>,
       V: Fn(I) -> IResult<I, O2> {
    return preceded(
        left_parser, terminated(inner, right_parser)
    );
}

fn prefix(i: &IStream) -> IResult<&IStream, ast::Prefix> {
    return alt((
        map(surrounded(kwd("("), expr, kwd(")"),),
             |e| ast::Prefix::ParenedExpr(e)),
        map(name, |n| ast::Prefix::Varname(n)),
    ))(i);
}

fn suffix(i: &IStream) -> IResult<&IStream, ast::Suffix> {
    use ast::Suffix::*;
    return alt((
        map(preceded(kwd("."), name),
            |n| DotAccess(n)),
        map(surrounded(kwd("["), expr, kwd("]")),
            |e| ArrAccess(e)),
        map(
            pair(
                opt(
                    preceded(
                        kwd(":"),
                        name
                    )
                ),
                args,
            ),
            |(opt_name, a)| MethodCall(opt_name, a)
        )
    ))(i);
}
fn prefixexp(i: &IStream) -> IResult<&IStream, ast::Prefixexpr> {
    let (i, pref) = prefix(i)?;
    let (i, suffixes) = many0(suffix)(i)?;
    return Ok((i, ast::Prefixexpr {
        prefix: pref,
        suffixes: suffixes
    }));
}


fn namelist(i: &IStream) -> IResult<&IStream, ast::Namelist> {
    return separated_list(kwd(","), name)(i);
}

fn funcname(i: &IStream) -> IResult<&IStream, ast::Funcname> {
    let (i, f_n) = name(i)?;
    let (i, o_n_c) = many0(preceded(kwd("."), name))(i)?;
    let (i, m_m_c) = opt(preceded(kwd(":"), name))(i)?;
    return Ok((i, ast::Funcname {
        first_name_component: f_n,
        other_name_components: o_n_c,
        method_component: m_m_c,
    }));
}

fn var(i: &IStream) -> IResult<&IStream, ast::Var>{
    return alt((
        map(
            pair(prefixexp,
                surrounded(
                    kwd("["),
                    expr,
                    kwd("]")
                )),
            |(p, e)| ast::Var::ArrAccess(p, e),
        ),
        map(
            pair(
                prefixexp,
                preceded(kwd("."), name)
            ),
            |(p, n)| ast::Var::DotAccess(p, n),
        ),
        map(name, |n| ast::Var::N(n)),
    ))(i);
}
fn varlist(i: &IStream) -> IResult<&IStream, ast::Varlist> {
    return map(
        separated_list(kwd(","), var),
        |l| ast::Varlist {vars: l}
    )(i);
}

fn stat(i: &IStream) -> IResult<&IStream, ast::Stat> {
    return alt((
        map(kwd(";"), |_| ast::Stat::Semicol),
        map(
            separated_pair(varlist, kwd("="), exprlist),
            |(varlist, explist)| ast::Stat::Eql(varlist, explist)
        ),
        // FFFFFFFFFFFFFFF
        // prefix expressions capture function calls
        // this is more open than lua's normal syntax
        map(
            expr,
            |e| ast::Stat::RawExpr(e),
        ),
        map(
            surrounded(kwd("::"), name, kwd("::")),
            |name| ast::Stat::Label(name),
        ),
        map(
            kwd("break"),
            |_| ast::Stat::Break,
        ),
        map(
            preceded(kwd("goto"), name),
            |n| ast::Stat::Goto(n),
        ),
        map(
            pair(
                surrounded(
                   kwd("repeat"),block,kwd("until")
                ),
                expr,
            ),
            |(b, e)| ast::Stat::Repeat(b, e)
        ),
        map(
            surrounded(
                kwd("do"),
                block,
                kwd("end")
            ),
            |b| ast::Stat::Do(b),
        ),
        map(
            pair(
                preceded(kwd("while"), expr),
                surrounded(kwd("do"), block, kwd("end"))
            ),
            |(e, b)| ast::Stat::While(e, b),
        ),
        map(
            tuple((
                surrounded(kwd("if"), expr, kwd("then")),
                block,
                many0(
                    pair(
                        surrounded(kwd("elseif"), expr, kwd("then")),
                        block
                    )
                ),
                opt(preceded(kwd("else"), block)),
                kwd("end"),
            )),
            |(p, t, ei_l, e_b, _)| ast::Stat::If {
                predicate: p,
                then_block: t,
                elif_list: ei_l,
                else_block: e_b,
            }
        ),
        map(
            tuple((
                surrounded(kwd("for"), name, kwd("=")),
                separated_pair(expr, kwd(","), expr),
                opt(
                    preceded(kwd(","), expr)
                ),
                surrounded(kwd("do"), block, kwd("end"))
            )),
            |(n, (e1, e2), m_e3, b)| ast::Stat::For(n, e1, e2, m_e3, b)
        ),
        map(
            tuple((
                surrounded(kwd("for"), namelist, kwd("in")),
                exprlist,
                surrounded(kwd("do"), block, kwd("end")),
            )),
            |(n, el, b)| ast::Stat::ForIn(n, el, b)
        ),
        map(
            preceded(
                kwd("function"),
                pair(funcname, funcbody),
            ),
            |(n, b)| ast::Stat::FuncDecl(n, b)
        ),
        map(
            preceded(
                pair(kwd("local"), kwd("function")),
                pair(name, funcbody),
            ),
            |(n, b)| ast::Stat::LocalFuncDecl(n, b),
        ),
        map(
            pair(
                preceded(kwd("local"), namelist),
                opt(
                    preceded(kwd("="), exprlist)
                )
            ),
            |(nl, m_el)| ast::Stat::LocalNames(nl, m_el)
        )
    ))(i);
}
// fn funccall(i: &IStream) -> IResult<&IStream, ast::Funccall> {

// }
fn retstat(i: &IStream) -> IResult<&IStream, ast::Retstat>{
    let (i, m_elist) = surrounded(
        kwd("return"),
        opt(exprlist),
        opt(kwd(";"))
    )(i)?;
    return Ok((i, ast::Retstat { return_expr: m_elist}));
}

fn block(i: &IStream) -> IResult<&IStream, ast::Block> {
    let (i, stats) = many0(stat)(i)?;
    let (i, m_r) = opt(retstat)(i)?;
    Ok((i, ast::Block {
        stats: stats,
        retstat: m_r,
    }))
}

fn function_def(i: &IStream) -> IResult<&IStream, ast::Expr> {
    let (i, _) = kwd("function")(i)?;
    let (i, f_b) = funcbody(i)?;
    return Ok((
        i,
        ast::Expr::Functiondef(f_b)
    ))
}

fn field(i: &IStream) -> IResult<&IStream, ast::Field> {
    // '[' exp ']' '=' exp
    let bracked_field = map(
        pair(
            surrounded(kwd("["), expr, kwd("]")),
            preceded(kwd("="), expr)
        ),
        | (e1, e2) | ast::Field::Bracketed(e1, e2)
    );
    // Name '=' exp
    let plain_eq = map(
        separated_pair(name, kwd("="), expr),
        | (n, e) | ast::Field::Named(n, e)
    );

    return alt((
        bracked_field,
        plain_eq,
        map(expr, |e| ast::Field::Raw(e))
    ))(i);

}

fn fieldlist(i: &IStream) -> IResult<&IStream,  ast::Fieldlist> {
    let fieldsep = alt((kwd(","), kwd(";")));
    let (i, fields) = separated_list(fieldsep, field)(i)?;
    let (i, _) = opt(alt((kwd(","), kwd(";"))))(i)?;
    return Ok((i, fields));
}

fn table_constructor(i: &IStream) -> IResult<&IStream, ast::Tableconstructor> {
    let (i, _) = kwd("{")(i)?;
    let (i, maybe_fieldlist) = opt(fieldlist)(i)?;
    let (i, _) = kwd("}")(i)?;
    return Ok(
        (i, maybe_fieldlist)
    );
}

fn expr_constants(i: &IStream) -> IResult<&IStream, ast::Expr> {
    // return expr constants or parenthesized
    return alt((
        map(kwd("nil"), |_| ast::Expr::Nil),
        map(kwd("true"), |_| ast::Expr::True),
        map(kwd("false"), |_| ast::Expr::False),
        num_parser,
        map(literal_string_parser, |s| ast::Expr::LiteralString(s)),
        map(kwd("..."), |_| ast::Expr::Ellipsis),
        map(table_constructor, |t| ast::Expr::Tbl(t)),
        function_def,
        surrounded(
            kwd("("), expr, kwd(")")
        ),
    ))(i);
}

fn unary_op(i: &IStream) -> IResult<&IStream, String>{
    let (i, result) = alt((
        kwd("#"),
        kwd("-"),
        kwd("not"),
        kwd("~"),
    ))(i)?;

    match result {
        lex::Lex::Keyword(k) => {
            return Ok((i, k));
        },
        _ => {
            panic!("Impossible code path");
        }
    }
}

fn expr(i: &IStream) -> IResult<&IStream, ast::Expr>{
    // parse out any unary operators and then read inner expressions;

    let (i, mut unops) = many0(unary_op)(i)?;
    let (i, inner_expr) = expr2(i)?;

    let mut return_result = inner_expr;
    unops.reverse();
    for unop in unops {
        return_result = ast::Expr::UnOp(
            unop,
            Box::new(return_result)
        );
    }

    return Ok((i, return_result));
}

fn expr2(i: &IStream) -> IResult<&IStream, ast::Expr> {
    // basically, parse binary opereators since they're 
    // left recursive
    let (i, first_expr) = expr_consume(i)?;
    let (i, bin_op_list) = many0(binop_right)(i)?;
    if bin_op_list.len() == 0 {
        // no binary operations, return the expression
        return Ok((i, first_expr));
    } else {
        // we need to fold the binary operator
        let mut result_expression = first_expr;
        for (operator, right_exp) in bin_op_list {
            result_expression = ast::Expr::BinOp(
                Box::new(result_expression),
                operator,
                Box::new(right_exp),
            )
        }
        return Ok((i, result_expression));
    }
}

fn binop_right(i: &IStream) -> IResult<&IStream, (ast::BinaryOperator, ast::Expr)> {
    let (i, operator) = alt((
        kwd("//"),kwd(">>"),kwd("<<"),kwd(".."),
        kwd("<="),kwd(">="),kwd("=="),kwd("~="),
        kwd("and"),kwd("or"),
        kwd("+"), kwd("-"), kwd("*"), kwd("/"), kwd("^"), kwd("%"),
        kwd("&"), kwd("~"), kwd("|"), kwd("<"), kwd(">"),
    ))(i)?;
    let (i, right_expr) = expr(i)?;
    return Ok((i,
    (operator, right_expr)));
}

fn expr_consume(i: &IStream) -> IResult<&IStream, ast::Expr> {
    return alt((
        expr_constants,
        map(prefixexp, |p| ast::Expr::Pref(Box::new(p))),
        // TOOD UnOp
    ))(i);
}

pub fn parse(input: &str) -> ast::Block { 
    let (input, tokens) = lex_all(input).unwrap();
    if input.len() > 0 {
        dbg!(&input[..20]);
        panic!("remaining input");
    }
    let (remaining_tokens, b) = block(&tokens).unwrap();
    if remaining_tokens.len() > 0 {
        for t in &remaining_tokens[..10]{
            dbg!(t);
        }
        panic!("Remaining tokens");
    }
    return b;
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    use std::fs::File;
    use std::io::Read;

    macro_rules! assert_parse_all {
        ($parser:expr, $input:expr) => {
            let (remaining_input, lexed) = lex_all($input).unwrap();
            if remaining_input.len() != 0 {
                dbg!(&remaining_input[..10]);
                panic!("Input remained from lexing")
            }
            let (remaining_tokens, _) = $parser(&lexed).unwrap();
            if remaining_tokens.len() != 0 {
                dbg!(&remaining_tokens[..10]);
                panic!("Input remained from parsing")
            };
        };
    }
    #[test]
    fn test_expr_parse(){
        let input = r#"
f(x, 3 + 5, y[z].foo.bar)
        "#;

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

        assert_parse_all!(
            exprlist,
            "x.y[z],y,3"
        );

        assert_parse_all!(
            expr,
            "3"
        );

        assert_parse_all!(
            expr,
            "print \"testing syntax\""
        );

        assert_parse_all!(
            args,
            "\"some string \""
        );

        assert_parse_all!(
            expr,
            "print \"testing syntax\""
        );

        assert_parse_all!(
            block,
            r#"
; do ; a = 3; assert(a == 3) end;
            "#
        );

        assert_parse_all!(
            expr,
            "2.0^-2 == 1/4 and -2^- -2 == - - -4"
        );

        assert_parse_all!(
            retstat,
            "return i, 'jojo';"
        );

        assert_parse_all!(
            stat,
            r#"
           if type(i) ~= 'number' then return i,'jojo'; end 
            "#
        );

        assert_parse_all!(
            expr,
            "string.format(prog, s, s)"
        );

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
    fn test_full_parse(){
        let input = r#"
local x = 3;
        "#;
        let (_, lexed) = lex_all(input).unwrap();
        println!("Successfully lexed");
        dbg!(stat(&lexed)).unwrap();
        // parse(input) ;
    }

    #[test]
    fn test_kwd_parse(){
        let lexed = lex_all(",").unwrap().1;
        assert_eq!(
            kwd(",")(&lexed),
            Ok((
                vec![].as_ref(),
                lex::Lex::Keyword(",".to_string())
            ))
        );
    }

    macro_rules! file_contents {
        ($filename:expr, $into_var:ident) => {
            let mut $into_var = String::new();
            {
                let mut file = File::open($filename).unwrap();
                file.read_to_string(&mut $into_var).unwrap();
            }
        };
    }

    #[test]
    fn test_parsing_of_lua_files(){
        file_contents!("lua_tests/constructs.lua", contents);

        // parse panics on failures so....
        parse(contents.as_str());
    }

    #[test]
    fn test_parse(){
        let l: Vec<lex::Lex> = lex_all("nil").unwrap().1;
        assert_eq!( 
            expr(&l),
         Ok((
            vec![].as_ref(),
            ast::Expr::Nil
        )));

        let m: Vec<lex::Lex> = lex_all("'hi this is a string with an \\' escape'").unwrap().1;

        assert_eq!(
            expr(&m),Ok((
                vec![].as_ref(),
                ast::Expr::LiteralString(
                    String::from("hi this is a string with an ' escape")
                )
            ))
        )
    }

    
}