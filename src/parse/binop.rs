// order of operations defined at 
// https://www.lua.org/manual/5.3/manual.html#3.4.8

// ^ (exponent)
// % // / * (multop)
// - + (plusopt)
// .. (concat)
// >> << (bitshift)
// & (bitand)
// ~ (bitnot)
// | (bitor)
// == ~= >= <= > < (compareop)
// and (logicand)
// or (logicor)

use super::ast;
use lex::IStream;

use parse::expr::expr_consume;
use parse::{
    many0,
    pair,
    alt,
    kwd,
    expr,
    
};
    
use parse::nom::{
    IResult
};

type EResult<'a, 'b> = IResult<&'b IStream<'a>, ast::Expr<'a>>;

pub fn parse_binops<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    // entry point for parsing binary operations
    // this will go down the cascade of operators as described above
    return logicor(i);
}

pub fn exponent<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = expr_consume(i)?;
    let (i, binops) = many0(
	pair(
         kwd("^"),
	 exponent
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn unary_op<'a,'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, String>{
    // helper function
    let (i, result) = alt((
        kwd("#"),
        kwd("-"),
        kwd("not"),
        kwd("~"),
    ))(i)?;

    use lex::LexValue::Keyword;
    
    match result {
	Lex {val: Keyword(k), .. } => {
            return Ok((i, k));
        },
        _ => {
            panic!("Impossible code path");
        }
    }
}
fn maybe_unaryop<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b>{
    // not # - ~
    let (i, mut unops) = many0(unary_op)(i)?;
    let (i, inner_expr) = exponent(i)?;
    let mut return_result = inner_expr;
    // we need to apply unary ops in reverse order from their
    // parsing
    unops.reverse();
    for unop in unops {
	return_result = ast::Expr::UnOp(
	    unop,
	    Box::new(return_result)
	);
    }
    return Ok((i, return_result));
}

fn multop<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = maybe_unaryop(i)?;
    let (i, binops) = many0(
	pair(
	 alt((
	    kwd("%"), kwd("//"), kwd("/"), kwd("*"),
	 )),
	 multop
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn plusop<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = multop(i)?;
    let (i, binops) = many0(
	pair(
	 alt((
	    kwd("-"), kwd("+"),
	 )),
	 plusop
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn concat<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = plusop(i)?;
    let (i, binops) = many0(
	pair(
         kwd(".."),
	 concat
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn bitshift<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = concat(i)?;
    let (i, binops) = many0(
	pair(
	 alt((
	    kwd(">>"),kwd("<<"),
	 )),
	 bitshift
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn bitand<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = bitshift(i)?;
    let (i, binops) = many0(
	pair(
         kwd("&"),
	 bitand
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn bitnot<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = bitand(i)?;
    let (i, binops) = many0(
	pair(
         kwd("~"),
	 bitnot
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn bitor<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = bitnot(i)?;
    let (i, binops) = many0(
	pair(
         kwd("|"),
	 bitor 
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn compareop<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = bitor(i)?;
    let (i, binops) = many0(
	pair(
	 alt((
	    kwd("=="), kwd("~="), kwd(">="), kwd("<="), kwd(">"), kwd("<"),
	 )),
	 compareop 
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn logicand<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = compareop(i)?;
    let (i, binops) = many0(
	pair(
         kwd("and"),
	 logicand 
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}

fn logicor<'a, 'b>(i: &'b IStream<'a>) -> EResult<'a, 'b> {
    let (i, first_expr) = logicand(i)?;
    let (i, binops) = many0(
	pair(
	 kwd("or"),
	 logicor
	))(i)?;
    return Ok(
	(i,
	 fold_binops(first_expr, binops)
	)
    );
}
use lex::Lex;
fn fold_binops<'a>(first_expr: ast::Expr<'a>, binops: Vec<(Lex<'a>, ast::Expr<'a>)>)
		   -> ast::Expr<'a> {
    if binops.len() == 0 {
	return first_expr;
    } else {
	// we take each binary operator and build up a new expression from it
	let mut result_expression = first_expr;
	for (operator, right_expr) in binops {
	    result_expression = ast::Expr::BinOp(
		Box::new(result_expression),
		operator.val,
		Box::new(right_expr)
	    )
	}
	return result_expression;
    }
}


fn binop_right<'a, 'b>(i: &'b IStream<'a>) -> IResult<&'b IStream<'a>, (ast::BinaryOperator, ast::Expr<'a>)> {
    let (i, operator) = alt((
        kwd("//"),kwd(">>"),kwd("<<"),kwd(".."),
        kwd("<="),kwd(">="),kwd("=="),kwd("~="),
        kwd("and"),kwd("or"),
        kwd("+"), kwd("-"), kwd("*"), kwd("/"), kwd("^"), kwd("%"),
        kwd("&"), kwd("~"), kwd("|"), kwd("<"), kwd(">"),
    ))(i)?;
    let (i, right_expr) = expr(i)?;
    return Ok((i,
    (operator.val, right_expr)));
}
