// As usual in extended BNF, {A} means 0 or more As, and [A] means an optional A.
	// chunk ::= block
#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub block: Block
}
	// block ::= {stat} [retstat]
#[derive(Debug, PartialEq)]
pub struct Block {
    pub stats: Vec<Stat>,
    pub retstat: Option<Retstat>
}

#[derive(Debug, PartialEq)]
pub enum Stat {
	// stat ::=  ‘;’ | 
    Semicol,
	// 	 varlist ‘=’ explist |
    Eql(Varlist, Exprlist),
	// 	 functioncall | 
    Call(Funccall),
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

	// retstat ::= return [explist] [‘;’]
#[derive(Debug, PartialEq)]
pub struct Retstat {
    pub return_expr: Option<Exprlist>
}
	// label ::= ‘::’ Name ‘::’
#[derive(Debug, PartialEq)]
pub struct Label {
    pub name: Name
}
	// funcname ::= Name {‘.’ Name} [‘:’ Name]
#[derive(Debug, PartialEq)]
pub struct Funcname {
    pub first_name_component: Name,
    pub other_name_components: Vec<Name>,
    pub method_component: Option<Name>,
}
	// varlist ::= var {‘,’ var}
#[derive(Debug, PartialEq)]
pub struct Varlist {
    pub vars: Vec<Var>
}
	// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
pub enum Prefixexpr {
    V(Box<Var>),
    // functioncall ::=  prefixexp args | prefixexp ‘:’ Name args 
    Call(Box<Funccall>),
    ParendExpr(Expr)
}
#[derive(Debug, PartialEq)]
pub struct Funccall {
    pub expr: Prefixexpr,
    pub command_name: Option<String>,
    pub args: Args
}
	// args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString 
#[derive(Debug, PartialEq)]
pub enum Args {
    List(Option<Exprlist>),
    Table(Tableconstructor),
    Literal(String)
}

	// funcbody ::= ‘(’ [parlist] ‘)’ block end
#[derive(Debug, PartialEq)]
pub struct Funcbody {
    pub parlist: Option<Parlist>,
    pub body: Block,
}
	// parlist ::= namelist [‘,’ ‘...’] | ‘...’
#[derive(Debug, PartialEq)]
pub struct Parlist {
    pub namelist: Namelist,
    pub has_ellipsis: bool
}
	// tableconstructor ::= ‘{’ [fieldlist] ‘}’
pub type Tableconstructor = Option<Fieldlist>;

	// fieldlist ::= field {fieldsep field} [fieldsep]
pub type Fieldlist = Vec<Field>;

	// field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
#[derive(Debug, PartialEq)]
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
type BinaryOperator = String;
type UnaryOperator = String;
	// unop ::= ‘-’ | not | ‘#’ | ‘~’
type Name = String;