extern crate nom;
use parse::nom::sequence::tuple;
use parse::nom::sequence::separated_pair;
use parse::nom::character::complete::one_of;
use parse::nom::multi::{
    many1,
    separated_list
};

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
    }
};

fn chunk(i: &IStream) -> IResult<&IStream, ast::Chunk> {
    let (i, bl) = block(i)?;
    return Ok((i, ast::Chunk {block: bl}));
}

fn num_component(i: char) -> bool {
    return i.is_numeric();
}
fn num_parser(i: &IStream) -> IResult<&IStream, ast::Expr> {
    // KNOWNWRONG this doesn't handle most numbers
    
    return map(
        many1(one_of("0123456789")), |_| ast::Expr::Numeral(String::from("1")),
    )(i);
}

fn literal_string_parser(i: &IStream) -> IResult<&IStream, String> {
   let (i, quote_char) = alt((char_parse('"'),
                               char_parse('\'')))(i)?;
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
            preceded(char_parse('\\'), char_parse(quote_char)),
        ))
    )(i)?;
    // then confirm that we have the last character
    let (i, _) = char_parse(quote_char)(i)?;
    // KNOWNWRONG not sure about the type safety of from_utf8
    return Ok((i, string_contents.into_iter().collect())); 
}

fn name(i: &IStream) -> IResult<&IStream, String> {
    // WRONG the definition of name is a bit more complex normally
    // Like with the keywords
    let (i, contents) = many1(
        one_of("abcdefghijklmnopqrstuvwxyz$_")
    )(i)?;
    
    return Ok((
        i,
        contents.into_iter().collect()
    ));
}

fn parlist(i: &IStream) -> IResult<&IStream, ast::Parlist> {
    // WRONG doesn't handle elipsis yet
    let (i, namelist) = separated_list(
        tag(","),
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
    return separated_list(tag(","), expr)(i);
}

fn args(i: &IStream) -> IResult<&IStream, ast::Args> {
    return alt((
        map(table_constructor, |t| ast::Args::Table(t)),
        map(literal_string_parser, |s| ast::Args::Literal(s)),
        map(
            delimited(
                tag("("), opt(exprlist), tag(")")
            ),
            |maybe_e| ast::Args::List(maybe_e),
        )
    ))(i);
}

fn funcbody(i: &IStream) -> IResult<&IStream, ast::Funcbody> {
    let (i, m_parlist) = delimited(
        tag("("),
        opt(parlist),
        tag(")"))(i)?;
    let (i, (b, _)) = pair(block, tag("end"))(i)?;
    return Ok((i, ast::Funcbody {parlist: m_parlist, body: b}));
}

fn funccall(i: &IStream) -> IResult<&IStream, ast::Funccall> {
    let (i, p) = prefixexp(i)?;
    let (i, maybe_name) = opt(
        preceded(tag(":"), name)
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
                tag("("),
                expr,
                tag(")"),
            ),
            |e| ast::Prefixexpr::ParendExpr(e),
        )
    ))(i);
}

fn namelist(i: &IStream) -> IResult<&IStream, ast::Namelist> {
    return separated_list(tag(","), name)(i);
}

fn funcname(i: &IStream) -> IResult<&IStream, ast::Funcname> {
    let (i, f_n) = name(i)?;
    let (i, o_n_c) = many0(preceded(tag("."), name))(i)?;
    let (i, m_m_c) = opt(preceded(tag(":"), name))(i)?;
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
                    tag("["),
                    expr,
                    tag("]")
                )),
            |(p, e)| ast::Var::ArrAccess(p, e),
        ),
        map(
            pair(
                prefixexp,
                preceded(tag("."), name)
            ),
            |(p, n)| ast::Var::DotAccess(p, n),
        ),
        map(name, |n| ast::Var::N(n))
    ))(i);
}
fn varlist(i: &IStream) -> IResult<&IStream, ast::Varlist> {
    return map(
        separated_list(tag(","), var),
        |l| ast::Varlist {vars: l}
    )(i);
}

fn stat(i: &IStream) -> IResult<&IStream, ast::Stat> {
    return alt((
        map(tag(";"), |_| ast::Stat::Semicol),
        map(
            separated_pair(varlist, tag("="), exprlist),
            |(varlist, explist)| ast::Stat::Eql(varlist, explist)
        ),
        // map(
        //     funccall,
        //     |f| ast::Stat::Call
        // ),
        map(
            delimited(tag("::"), name, tag("::")),
            |name| ast::Stat::Label(name),
        ),
        map(
            tag("break"),
            |_| ast::Stat::Break,
        ),
        map(
            preceded(tag("goto"), name),
            |n| ast::Stat::Goto(n),
        ),
        map(
            delimited(
                tag("do"),
                block,
                tag("end")
            ),
            |b| ast::Stat::Do(b),
        ),
        map(
            pair(
                preceded(tag("while"), expr),
                delimited(tag("do"), block, tag("end"))
            ),
            |(e, b)| ast::Stat::While(e, b),
        ),
        map(
            tuple((
                delimited(tag("if"), expr, tag("then")),
                block,
                many0(
                    pair(
                        delimited(tag("elseif"), expr, tag("then")),
                        block
                    )
                ),
                opt(preceded(tag("else"), block)),
                tag("end"),
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
                delimited(tag("for"), name, tag("=")),
                separated_pair(expr, tag(","), expr),
                opt(
                    preceded(tag(","), expr)
                ),
                delimited(tag("do"), block, tag("end"))
            )),
            |(n, (e1, e2), m_e3, b)| ast::Stat::For(n, e1, e2, m_e3, b)
        ),
        map(
            tuple((
                delimited(tag("for"), namelist, tag("in")),
                exprlist,
                delimited(tag("do"), block, tag("end")),
            )),
            |(n, el, b)| ast::Stat::ForIn(n, el, b)
        ),
        map(
            preceded(
                tag("function"),
                pair(funcname, funcbody),
            ),
            |(n, b)| ast::Stat::FuncDecl(n, b)
        ),
        map(
            preceded(
                pair(tag("local"), tag("function")),
                pair(name, funcbody),
            ),
            |(n, b)| ast::Stat::LocalFuncDecl(n, b),
        ),
        map(
            pair(
                preceded(tag("local"), namelist),
                opt(
                    preceded(tag("="), exprlist)
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
        tag("return"),
        opt(exprlist),
        opt(tag(";"))
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
    let (i, _) = tag("function")(i)?;
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
            delimited(tag("["), expr, tag("]")),
            preceded(tag("="), expr)
        ),
        | (e1, e2) | ast::Field::Bracketed(e1, e2)
    );
    // Name '=' exp
    let plain_eq = map(
        separated_pair(name, tag("="), expr),
        | (n, e) | ast::Field::Named(n, e)
    );

    return alt((
        bracked_field,
        plain_eq,
        map(expr, |e| ast::Field::Raw(e))
    ))(i);

}

fn fieldlist(i: &IStream) -> IResult<&IStream,  ast::Fieldlist> {
    let fieldsep = alt((tag(","), tag(";")));
    let (i, fields) = separated_list(fieldsep, field)(i)?;
    let (i, _) = opt(alt((tag(","), tag(";"))))(i)?;
    return Ok((i, fields));
}

fn table_constructor(i: &IStream) -> IResult<&IStream, ast::Tableconstructor> {
    let (i, _) = tag("{")(i)?;
    let (i, maybe_fieldlist) = opt(fieldlist)(i)?;
    let (i, _) = tag("}")(i)?;
    return Ok(
        (i, maybe_fieldlist)
    );
}

fn binary_op(i: &IStream) -> IResult<&IStream, ast::Expr> {
    let (i, left_expr) = expr(i)?;
    let (i, operator) = alt((
        tag("//"),tag(">>"),tag("<<"),tag(".."),
        tag("<="),tag(">="),tag("=="),tag("~="),
        tag("and"),tag("or"),
        tag("+"), tag("-"), tag("*"), tag("/"), tag("^"), tag("%"),
        tag("&"), tag("~"), tag("|"), tag("<"), tag(">"),
    ))(i)?;
    let (i, right_expr) = expr(i)?;
    return Ok((i, ast::Expr::BinOp(Box::new(left_expr), 
                                   String::from(operator), 
                                   Box::new(right_expr))));
}

fn expr(i: &IStream) -> IResult<&IStream, ast::Expr> {

    return alt((
        map(tag("nil"), |_| ast::Expr::Nil),
        map(tag("true"), |_| ast::Expr::True),
        map(tag("false"), |_| ast::Expr::False),
        num_parser,
        map(literal_string_parser, |s| ast::Expr::LiteralString(s)),
        map(tag("..."), |_| ast::Expr::Ellipsis),
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
        assert_eq!(expr("nil"), Ok((
            "",
            ast::Expr::Nil
        )));

        assert_eq!(
            expr("'hi this is a string with an \\' escape'"),Ok((
                "",
                ast::Expr::LiteralString(
                    String::from("hi this is a string with an ' escape")
                )
            ))
        )
    }
}