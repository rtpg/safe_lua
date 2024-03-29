use super::*;

pub fn expr<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Expr<'a>> {
    // parse out any unary operators and then read inner expressions;
    return expr2(i);
    // let (i, mut unops) = many0(unary_op)(i)?;
    // let (i, inner_expr) = expr2(i)?;

    // let mut return_result = inner_expr;
    // unops.reverse();
    // for unop in unops {
    //     return_result = ast::Expr::UnOp(
    //         unop,
    //         Box::new(return_result)
    //     );
    // }

    // return Ok((i, return_result));
}

fn expr2<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Expr<'a>> {
    // basically, parse binary opereators since they're
    // left recursive
    use parse::binop::parse_binops;
    return parse_binops(i);
    // let (i, first_expr) = expr_consume(i)?;
    // let (i, bin_op_list) = many0(binop_right)(i)?;
    // if bin_op_list.len() == 0 {
    //     // no binary operations, return the expression
    //     return Ok((i, first_expr));
    // } else {
    //     // we need to fold the binary operator
    //     let mut result_expression = first_expr;
    //     for (operator, right_exp) in bin_op_list {
    //         result_expression = ast::Expr::BinOp(
    //             Box::new(result_expression),
    //             operator,
    //             Box::new(right_exp),
    //         )
    //     }
    //     return Ok((i, result_expression));
    // }
}

pub fn expr_consume<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Expr<'a>> {
    return alt((
        expr_constants,
        map(prefixexp, |p| ast::Expr::Pref(Box::new(p))),
    ))(i);
}

fn expr_constants<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, ast::Expr<'a>> {
    // return expr constants or parenthesized
    return alt((
        map(kwd("nil"), |k| ast::Expr::Nil(k.location)),
        map(kwd("true"), |k| ast::Expr::True(k.location)),
        map(kwd("false"), |k| ast::Expr::False(k.location)),
        num_parser,
        map(literal_string_parser, |(s, loc)| {
            ast::Expr::LiteralString(s, loc)
        }),
        map(kwd("..."), |k| ast::Expr::Ellipsis(k.location)),
        map(table_constructor, |(t, loc)| ast::Expr::Tbl(t, loc)),
        function_def,
    ))(i);
}
