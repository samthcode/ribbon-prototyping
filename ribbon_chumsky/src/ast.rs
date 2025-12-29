use std::fmt::Debug;

use chumsky::span::Spanned;

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    /// This includes variables - everything is a path
    Path(Path<'src>),

    Num(&'src str),
    String(&'src str),
    Char(char),
    Bool(bool),

    List(Vec<Spanned<Expr<'src>>>),
    Bin(
        Box<Spanned<Expr<'src>>>,
        Spanned<BinOp>,
        Box<Spanned<Expr<'src>>>,
    ),
    Unary(Spanned<UnaryOp>, Box<Spanned<Expr<'src>>>),
    Block(Box<Block<'src>>),
    Fn(Box<Func<'src>>),
    Binding(Box<Binding<'src>>),
}

#[derive(Debug, Clone)]
pub struct Path<'src> {
    pub segments: Vec<Spanned<PathSegment<'src>>>,
}

#[derive(Debug, Clone)]
pub enum PathSegment<'src> {
    /// Identifier or concrete type
    Ident(&'src str),
    /// Generic type
    Ty(&'src str, Vec<Spanned<Ty<'src>>>),
}

#[derive(Debug, Clone)]
pub struct Block<'src> {
    pub stmts: Vec<Spanned<Expr<'src>>>,
    pub ret: Option<Spanned<Expr<'src>>>,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,

    /// `<`
    Lt,
    // `>`
    Gt,
    /// `==`
    Equality,
    /// `!=`
    Inequality,
    /// `<=`
    LtEquality,
    /// `>=`
    GtEquality,

    /// `&`
    BwAnd,
    /// `^`
    BwXor,
    /// `|`
    BwOr,

    /// `&&`
    LogicalAnd,
    /// `||`
    LogicalOr,
    /// `:>`
    MethodPipe,
    /// `~>`
    FuncPipe,

    /// `~?`
    ErrProp,

    /// `<<`
    ShiftL,
    /// `>>`
    ShiftR,

    /// `..`
    Range,
    /// `..=`
    RangeIncl,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
    Deref,
    Ref,
}

#[derive(Debug, Clone)]
pub struct Binding<'src> {
    pub pat: Spanned<Pat<'src>>,
    pub ty: Option<Spanned<Ty<'src>>>,
    pub val: Spanned<Expr<'src>>,
}

#[derive(Debug, Clone)]
pub enum Pat<'src> {
    Ident(&'src str),
}

#[derive(Debug, Clone)]
pub enum Ty<'src> {
    Path(Path<'src>),
    Func(Vec<Spanned<Ty<'src>>>, Box<Spanned<Ty<'src>>>),
}

#[derive(Debug, Clone)]
pub struct Func<'src> {
    pub params: Vec<Spanned<Param<'src>>>,
    pub ty: Option<Spanned<Ty<'src>>>,
    pub body: Spanned<Expr<'src>>,
}

#[derive(Debug, Clone)]
pub struct Param<'src> {
    pub pat: Spanned<Pat<'src>>,
    pub ty: Spanned<Ty<'src>>,
    pub default: Option<Spanned<Expr<'src>>>,
}
