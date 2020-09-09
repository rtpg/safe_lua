#[allow(dead_code)]

use nom_locate::LocatedSpan;
use lex::{
    LexValue
};

// As usual in extended BNF, {A} means 0 or more As, and [A] means an optional A.
	// chunk ::= block
#[derive(Debug, Clone, PartialEq)]
pub struct Chunk<'a> {
    pub block: Block<'a>
}
	// block ::= {stat} [retstat]
#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    pub stats: Vec<Stat<'a>>,
    pub retstat: Option<Retstat<'a>>
}

impl<'a, 'b> HasLoc<'a, 'b> for Block<'a> {
    fn loc(&'b self) -> LocatedSpan<&'a str> {
	if self.stats.len() == 0 {
	    return LocatedSpan::new("UNKNOWN LOC (BLOCK)")
	}
	return self.stats[0].loc
    }
}
// pub fn parse_block(input: &Vec<Lex>) -> R<Block> {
//     let (input, stats) = many1(parse_stat)(input)?;
//     let (input, return_stat) = opt(parse_retstat)(input)?;
//     return Ok(
//         (input, Block {
//             stats: stats,
//             retstat: return_stat
//         })
//     );
// }

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct Stat<'a> {
    pub v: StatV<'a>,
    pub loc: LocatedSpan<&'a str>
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatV<'a> {
	// stat ::=  ‘;’ | 
    Semicol,
	// 	 varlist ‘=’ explist |
    Eql(Varlist<'a>, Exprlist<'a>),
    // the following is us being more open to thing
	// 	 rawexpr | 
    RawExpr(Expr<'a>),
	// 	 label | 
    Label(Name),
	// 	 break | 
    Break,
	// 	 goto Name | 
    Goto(Name),
	// 	 do block end | 
    Do(Block<'a>),
	// 	 while exp do block end | 
    While(Expr<'a>, Block<'a>),
	// 	 repeat block until exp | 
    Repeat(Block<'a>, Expr<'a>),
	// 	 if exp then block {elseif exp then block} [else block] end | 
    If {
        predicate: Expr<'a>,
        then_block: Block<'a>,
        elif_list: Vec<(Expr<'a>, Block<'a>)>,
        else_block: Option<Block<'a>>,
    },
	// 	 for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end | 
    For(Name, Expr<'a>, Expr<'a>, Option<Expr<'a>>, Block<'a>),
	// 	 for namelist in explist do block end | 
    ForIn(Namelist, Exprlist<'a>, Block<'a>),
	// 	 function funcname funcbody | 
    FuncDecl(Funcname<'a>, Funcbody<'a>),
	// 	 local function Name funcbody | 
    LocalFuncDecl(Name, Funcbody<'a>),
	// 	 local namelist [‘=’ explist] 
    LocalNames(Namelist, Option<Exprlist<'a>>),
}

// pub fn parse_stat(input: &Lexes) -> R<Stat> {
//     return alt((
//         (map(_symbol(";"),|_| Stat::Semicol,)),
//         (map(preceded(
//              _symbol("goto"),
//              _name,),
//         |Lex::Name(n)| Stat::Goto(n))),
//         (map(
//             preceded(
//                 _symbol("do"),
//                 terminated(
//                     parse_block,
//                     _symbol("end")
//                 )
//             ),
//             |b| Stat::Do(b)
//         )),
//         (map(
//             preceded(
//                 _symbol("while"),
//                 separated_pair(
//                     parse_expr,
//                     _symbol("do"),
//                     terminated(
//                         parse_block,
//                         _symbol("end")
//                     )
//                 )
//             ),
//             |(expr, b)| Stat::While(expr, b)
//         )),
//         (map(
//             preceded(
//                 _symbol("repeat"),
//                 separated_pair(
//                     parse_block,
//                     _symbol("until"),
//                     terminated(parse_expr, _symbol("end"),
//                     )
//                 )
//             ),
//             |(b, e)| Stat::Repeat(b, e)
//         ))
//     ))(input);
// }

	// retstat ::= return [explist] [‘;’]
#[derive(Debug, Clone, PartialEq)]
pub struct Retstat<'a> {
    pub return_expr: Option<Exprlist<'a>>
}

	// label ::= ‘::’ Name ‘::’
#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub name: Name
}
	// funcname ::= Name {‘.’ Name} [‘:’ Name]
#[derive(Debug, Clone, PartialEq)]
pub struct Funcname<'a> {
    pub first_name_component: Name,
    pub other_name_components: Vec<Name>,
    pub method_component: Option<Name>,
    pub loc: LocatedSpan<&'a str>
}
	// varlist ::= var {‘,’ var}
#[derive(Debug, Clone, PartialEq)]
pub struct Varlist<'a> {
    pub vars: Vec<Var<'a>>
}
	// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 
#[derive(Debug, Clone, PartialEq)]
pub enum Var<'a> {
    N(Name, LocatedSpan<&'a str>),
    ArrAccess(Prefixexpr<'a>, Expr<'a>),
    DotAccess(Prefixexpr<'a>, Name),
}

pub trait HasLoc<'a, 'b> {
    fn loc(&'b self) -> LocatedSpan<&'a str>;
}

impl<'a, 'b> HasLoc<'a, 'b> for Varlist<'a> {
    fn loc(&'b self) -> LocatedSpan<&'a str> {
	self.vars[0].loc()
    }
}
impl<'a, 'b> HasLoc<'a, 'b> for Var<'a> {
    fn loc(&'b self) -> LocatedSpan<&'a str> {
	use self::Var::*;
	match self {
	    N(_, loc) => *loc,
	    ArrAccess(p, _e) => p.loc(),
	    DotAccess(p, _n) => p.loc(),
	}
    }
}
	// namelist ::= Name {‘,’ Name}
pub type Namelist = Vec<Name>;
	// explist ::= exp {‘,’ exp}
pub type Exprlist<'a> = Vec<Expr<'a>>;

	// exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef | 
	// 	 prefixexp | tableconstructor | exp binop exp | unop exp 
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Nil(LocatedSpan<&'a str>),
    False(LocatedSpan<&'a str>), 
    True(LocatedSpan<&'a str>),
    Numeral(String, LocatedSpan<&'a str>),
    LiteralString(String, LocatedSpan<&'a str>),
    Ellipsis(LocatedSpan<&'a str>),
  	// functiondef ::= function funcbody
    Functiondef(Funcbody<'a>),
    Pref(Box<Prefixexpr<'a>>),
    Tbl(Tableconstructor<'a>, LocatedSpan<&'a str>),
    BinOp(Box<Expr<'a>>, BinaryOperator, Box<Expr<'a>>),
    UnOp(UnaryOperator, Box<Expr<'a>>),
}

impl<'a, 'b> HasLoc<'a, 'b> for Expr<'a> {
    fn loc(&'b self) -> LocatedSpan<&'a str> {
	use ast::Expr::*;
	match self {
	    True(l) => *l,
	    False(l) => *l,
	    Nil(l) => *l,
	    Numeral(_, l) => *l,
	    LiteralString(_, l) => *l,
	    Ellipsis(l) => *l,
	    Functiondef(fb) => fb.location,
	    Pref(pexpr) => pexpr.loc(),
	    Tbl(_tbl, loc) => *loc,
	    BinOp(fst, _op, _snd) => fst.loc(),
	    UnOp(_unop, exp) => exp.loc()
	}
    }
}
	// prefixexp ::= var | functioncall | ‘(’ exp ‘)’
#[derive(Debug, Clone, PartialEq)]
pub struct Prefixexpr<'a> {
    pub prefix: Prefix<'a>,
    pub suffixes: Vec<Suffix<'a>>,
}

impl<'a, 'b> HasLoc<'a, 'b> for Prefixexpr<'a> {
    fn loc(&'b self) -> LocatedSpan<&'a str> {
	match &self.prefix {
	    Prefix::ParenedExpr(expr) => expr.loc(),
	    Prefix::Varname(_, loc) => *loc,
	}
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Prefix<'a> {
    ParenedExpr(Expr<'a>),
    Varname(Name, LocatedSpan<&'a str>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Suffix<'a> {
    // a.b
    DotAccess(Name),
    // a[c]
    ArrAccess(Expr<'a>),
    // a(:b)(args)
    MethodCall(Option<Name>, Args<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Funccall<'a> {
    pub expr: Prefixexpr<'a>,
    pub command_name: Option<String>,
    pub args: Args<'a>
}
	// args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString 
#[derive(Debug, Clone, PartialEq)]
pub enum Args<'a> {
    List(Option<Exprlist<'a>>),
    Table(Tableconstructor<'a>),
    Literal(String)
}

	// funcbody ::= ‘(’ [parlist] ‘)’ block end
#[derive(Debug, Clone, PartialEq)]
pub struct Funcbody<'a> {
    pub parlist: Option<Parlist>,
    pub body: Block<'a>,
    pub location: LocatedSpan<&'a str>,
}
	// parlist ::= namelist [‘,’ ‘...’] | ‘...’
#[derive(Debug, Clone, PartialEq)]
pub struct Parlist {
    pub namelist: Namelist,
    pub has_ellipsis: bool
}
	// tableconstructor ::= ‘{’ [fieldlist] ‘}’
pub type Tableconstructor<'a> = Option<Fieldlist<'a>>;

	// fieldlist ::= field {fieldsep field} [fieldsep]
pub type Fieldlist<'a> = Vec<Field<'a>>;

	// field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
#[derive(Debug, Clone, PartialEq)]
pub enum Field<'a> {
    Bracketed(Expr<'a>, Expr<'a>),
    Named(Name, Expr<'a>),
    Raw(Expr<'a>),
}
	// fieldsep ::= ‘,’ | ‘;’

	// binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ | 
	// 	 ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ | 
	// 	 ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ | 
	// 	 and | or
pub type BinaryOperator = LexValue;
pub type UnaryOperator = String;
	// unop ::= ‘-’ | not | ‘#’ | ‘~’
pub type Name = String;
