// order of operations defined at 
// https://www.lua.org/manual/5.3/manual.html#3.4.8

// ^ (exponent)
// % // / * (multop)
// - + (plusop)
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
	 // this one feels pretty wrong...
	 maybe_unaryop 
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
	 maybe_unaryop 
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
	 multop 
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

#[cfg(test)]
mod test {
    use parse::try_specific_parse;
    use parse::expr;
    use pretty_assertions::{assert_eq};
use super::*;
    use ast::Expr;
    use nom_locate::LocatedSpan;

    #[ignore]
    #[test]
    fn test_parse_binop(){
	let result_expr = try_specific_parse(
	    expr,
	    "a+b+c"
	).unwrap();

	let expected = Expr::binop(
	    Expr::binop(
		Expr::name("a"),
		"+",
		Expr::name("b"),
	    ),
	    "+",
	    Expr::name("c")
	);

	assert_eq!(expected, result_expr);

	// now try with minuses
	let minus_expr = try_specific_parse(
	    expr,
	    "-a-b-c",
	).unwrap();

	let minus_expected = Expr::binop(
	    Expr::binop(
		Expr::unary(
		    "-",
		    Expr::name("a"),
		),
		"-",
		Expr::name("b")
	    ),
	    "-",
	    Expr::name("c"),
	);

	assert_eq!(minus_expected, minus_expr);
    }
    
    #[test]
    fn test_fold_binops(){
	// I want to test here that when folding we're being left
	// associative rather than right associative

	// expression is a + b + c
	// we want (a + b) + c
	// not a + (b + c)
	let a =  Expr::new_name("a", LocatedSpan::new(""));
	let b =  Expr::new_name("b", LocatedSpan::new(""));
	let c =  Expr::new_name("c", LocatedSpan::new(""));
	let result = fold_binops(
	    a.clone(),
	    vec![
		(Lex::kwd("+".to_string(), None), b.clone()),
		(Lex::kwd("+".to_string(), None), c.clone()),
	    ]
	);

	let expected = Expr::binop(
	    Expr::binop(
		a,
		"+",
		b
	    ),
	    "+",
	    c
	);

	assert_eq!(result, expected);
    }
}
