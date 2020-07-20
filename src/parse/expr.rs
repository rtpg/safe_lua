use super::*;

pub fn expr<'a>(i: &'a IStream<'a>) -> IResult<&'a IStream<'a>, ast::Expr>{
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

fn expr2<'a>(i: &'a IStream<'a>) -> IResult<&'a IStream<'a>, ast::Expr> {
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


fn expr_consume<'a>(i: &'a IStream<'a>) -> IResult<&'a IStream<'a>, ast::Expr> {
    return alt((
        expr_constants,
        map(prefixexp, |p| ast::Expr::Pref(Box::new(p))),
    ))(i);
}

fn expr_constants<'a>(i: &'a IStream<'a>) -> IResult<&'a IStream<'a>, ast::Expr> {
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
    ))(i);
}
