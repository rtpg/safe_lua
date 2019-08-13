extern crate nom;
use lex::{
    ISlice,
    lex_all
};
use parse::nom::sequence::tuple;
use parse::nom::sequence::separated_pair;
use parse::nom::character::complete::one_of;
use parse::nom::multi::{
    many1,
    separated_list
};
use lex;

use parse::nom::combinator::opt;

use parse::nom::sequence::{
    preceded,
    pair,
    delimited,
};
use parse::nom::multi::many0;
use parse::nom::character::complete::none_of;
use parse::nom::character::complete::char as char_parse;
use ast;
use lex::IStream;

use parse::nom::{
    IResult,
    bytes::complete::tag,
    branch::{
        alt,
    },
    combinator::{
        map
    },
    Err,
    error::ErrorKind,
    AsBytes,
    Compare,
};


// impl AsBytes for &IStream {

// }

// impl Compare for &IStream {

// }

fn chunk(i: &IStream) -> IResult<&IStream, ast::Chunk> {
    let (i, bl) = block(i)?;
    return Ok((i, ast::Chunk {block: bl}));
}

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
        match i.get(0){
            Some(lex::Lex::Keyword(_s)) => {
                if &s == _s {
                    return Ok((&i[1..], lex::Lex::Keyword(_s.to_string())))
                } else {
                    return Err(Err::Error((i, ErrorKind::Tag)))
                }
            },
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
            delimited(
                kwd("("), opt(exprlist), kwd(")")
            ),
            |maybe_e| ast::Args::List(maybe_e),
        )
    ))(i);
}

fn funcbody(i: &IStream) -> IResult<&IStream, ast::Funcbody> {
    let (i, m_parlist) = delimited(
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
fn prefixexp(i: &IStream) -> IResult<&IStream, ast::Prefixexpr> {
    return alt((
        map(var, |v| ast::Prefixexpr::V(Box::new(v))),
        map(
            funccall,
            |f| ast::Prefixexpr::Call(Box::new(f)),
        ),
        map(
            delimited(
                kwd("("),
                expr,
                kwd(")"),
            ),
            |e| ast::Prefixexpr::ParendExpr(e),
        )
    ))(i);
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
                delimited(
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
        map(name, |n| ast::Var::N(n))
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
        // map(
        //     funccall,
        //     |f| ast::Stat::Call
        // ),
        map(
            delimited(kwd("::"), name, kwd("::")),
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
            delimited(
                kwd("do"),
                block,
                kwd("end")
            ),
            |b| ast::Stat::Do(b),
        ),
        map(
            pair(
                preceded(kwd("while"), expr),
                delimited(kwd("do"), block, kwd("end"))
            ),
            |(e, b)| ast::Stat::While(e, b),
        ),
        map(
            tuple((
                delimited(kwd("if"), expr, kwd("then")),
                block,
                many0(
                    pair(
                        delimited(kwd("elseif"), expr, kwd("then")),
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
                delimited(kwd("for"), name, kwd("=")),
                separated_pair(expr, kwd(","), expr),
                opt(
                    preceded(kwd(","), expr)
                ),
                delimited(kwd("do"), block, kwd("end"))
            )),
            |(n, (e1, e2), m_e3, b)| ast::Stat::For(n, e1, e2, m_e3, b)
        ),
        map(
            tuple((
                delimited(kwd("for"), namelist, kwd("in")),
                exprlist,
                delimited(kwd("do"), block, kwd("end")),
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
    let (i, m_elist) = delimited(
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
            delimited(kwd("["), expr, kwd("]")),
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

fn binary_op(i: &IStream) -> IResult<&IStream, ast::Expr> {
    let (i, left_expr) = expr(i)?;
    let (i, operator) = alt((
        kwd("//"),kwd(">>"),kwd("<<"),kwd(".."),
        kwd("<="),kwd(">="),kwd("=="),kwd("~="),
        kwd("and"),kwd("or"),
        kwd("+"), kwd("-"), kwd("*"), kwd("/"), kwd("^"), kwd("%"),
        kwd("&"), kwd("~"), kwd("|"), kwd("<"), kwd(">"),
    ))(i)?;
    let (i, right_expr) = expr(i)?;
    return Ok((i, ast::Expr::BinOp(Box::new(left_expr), 
                                   operator, 
                                   Box::new(right_expr))));
}

fn expr(i: &IStream) -> IResult<&IStream, ast::Expr> {

    return alt((
        map(kwd("nil"), |_| ast::Expr::Nil),
        map(kwd("true"), |_| ast::Expr::True),
        map(kwd("false"), |_| ast::Expr::False),
        num_parser,
        map(literal_string_parser, |s| ast::Expr::LiteralString(s)),
        map(kwd("..."), |_| ast::Expr::Ellipsis),
        function_def,
        // TODO Pref
        map(table_constructor, |t| ast::Expr::Tbl(t)),
        binary_op, 
        // TOOD UnOp
    ))(i);
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_full_parse(){

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