#[allow(dead_code)]

use lex::{
    Lex,
};

// As usual in extended BNF, {A} means 0 or more As, and [A] means an optional A.
	// chunk ::= block
#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    pub block: Block
}
	// block ::= {stat} [retstat]
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stats: Vec<Stat>,
    pub retstat: Option<Retstat>
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
pub enum Stat {
	// stat ::=  ‘;’ | 
    Semicol,
	// 	 varlist ‘=’ explist |
    Eql(Varlist, Exprlist),
    // the following is us being more open to thing
	// 	 rawexpr | 
    RawExpr(Expr),
	// 	 label | 
    Label(Name),
	// 	 break | 
    Break,
	// 	 goto Name | 
    Goto(Name),
	// 	 do block end | 
    Do(Block),
	// 	 while exp do block end | 
    While(Expr, Block),
	// 	 repeat block until exp | 
    Repeat(Block, Expr),
	// 	 if exp then block {elseif exp then block} [else block] end | 
    If {
        predicate: Expr,
        then_block: Block,
        elif_list: Vec<(Expr, Block)>,
        else_block: Option<Block>,
    },
	// 	 for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end | 
    For(Name, Expr, Expr, Option<Expr>, Block),
	// 	 for namelist in explist do block end | 
    ForIn(Namelist, Exprlist, Block),
	// 	 function funcname funcbody | 
    FuncDecl(Funcname, Funcbody),
	// 	 local function Name funcbody | 
    LocalFuncDecl(Name, Funcbody),
	// 	 local namelist [‘=’ explist] 
    LocalNames(Namelist, Option<Exprlist>),
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
pub struct Retstat {
    pub return_expr: Option<Exprlist>
}

	// label ::= ‘::’ Name ‘::’
#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub name: Name
}
	// funcname ::= Name {‘.’ Name} [‘:’ Name]
#[derive(Debug, Clone, PartialEq)]
pub struct Funcname {
    pub first_name_component: Name,
    pub other_name_components: Vec<Name>,
    pub method_component: Option<Name>,
}
	// varlist ::= var {‘,’ var}
#[derive(Debug, Clone, PartialEq)]
pub struct Varlist {
    pub vars: Vec<Var>
}
	// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 
#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    N(Name),
    ArrAccess(Prefixexpr, Expr),
    DotAccess(Prefixexpr, Name),
}
	// namelist ::= Name {‘,’ Name}
pub type Namelist = Vec<Name>;
	// explist ::= exp {‘,’ exp}
pub type Exprlist = Vec<Expr>;

	// exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef | 
	// 	 prefixexp | tableconstructor | exp binop exp | unop exp 
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Nil,
    False, 
    True,
    Numeral(String),
    LiteralString(String),
    Ellipsis,
  	// functiondef ::= function funcbody
    Functiondef(Funcbody),
    Pref(Box<Prefixexpr>),
    Tbl(Tableconstructor),
    BinOp(Box<Expr>, BinaryOperator, Box<Expr>),
    UnOp(UnaryOperator, Box<Expr>),
}

	// prefixexp ::= var | functioncall | ‘(’ exp ‘)’
#[derive(Debug, Clone, PartialEq)]
pub struct Prefixexpr {
    pub prefix: Prefix,
    pub suffixes: Vec<Suffix>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Prefix {
    ParenedExpr(Expr),
    Varname(Name),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Suffix {
    // a.b
    DotAccess(Name),
    // a[c]
    ArrAccess(Expr),
    // a(:b)(args)
    MethodCall(Option<Name>, Args),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Funccall {
    pub expr: Prefixexpr,
    pub command_name: Option<String>,
    pub args: Args
}
	// args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString 
#[derive(Debug, Clone, PartialEq)]
pub enum Args {
    List(Option<Exprlist>),
    Table(Tableconstructor),
    Literal(String)
}

	// funcbody ::= ‘(’ [parlist] ‘)’ block end
#[derive(Debug, Clone, PartialEq)]
pub struct Funcbody {
    pub parlist: Option<Parlist>,
    pub body: Block,
}
	// parlist ::= namelist [‘,’ ‘...’] | ‘...’
#[derive(Debug, Clone, PartialEq)]
pub struct Parlist {
    pub namelist: Namelist,
    pub has_ellipsis: bool
}
	// tableconstructor ::= ‘{’ [fieldlist] ‘}’
pub type Tableconstructor = Option<Fieldlist>;

	// fieldlist ::= field {fieldsep field} [fieldsep]
pub type Fieldlist = Vec<Field>;

	// field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
#[derive(Debug, Clone, PartialEq)]
pub enum Field {
    Bracketed(Expr, Expr),
    Named(Name, Expr),
    Raw(Expr),
}
	// fieldsep ::= ‘,’ | ‘;’

	// binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ | 
	// 	 ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ | 
	// 	 ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ | 
	// 	 and | or
pub type BinaryOperator = Lex;
pub type UnaryOperator = String;
	// unop ::= ‘-’ | not | ‘#’ | ‘~’
pub type Name = String;