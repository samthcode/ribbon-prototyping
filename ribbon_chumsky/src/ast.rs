use std::fmt::Debug;

use chumsky::span::Spanned;

#[derive(Debug, Clone)]
pub enum Item<'src> {
    Binding(Binding<'src>),
    /// Note that this can also be a binding but the dedicated variant should be parsed first
    Expr(Expr<'src>),
    /// e.g. `type Point = struct {x: i32, y: i32};`
    TypeDef(TypeDef<'src>),
    /// e.g. `fn some_func = () => "Hello World"`
    FuncDef(FuncDef<'src>),
}

#[derive(Debug, Clone)]
pub enum TypeDef<'src> {
    // TODO: Figure out what should be in these variants
    Struct(Spanned<Ident<'src>>, Vec<Spanned<Field<'src>>>),
    Interface(Spanned<Ident<'src>>, Vec<Spanned<Binding<'src>>>),
}

#[derive(Debug, Clone)]
pub struct FuncDef<'src> {
    pub name: Spanned<Ident<'src>>,
    pub params: Vec<Spanned<Param<'src>>>,
    pub ret_ty: Option<Spanned<Ty<'src>>>,
    pub body: Spanned<Expr<'src>>,
}

#[derive(Debug, Clone)]
pub struct Field<'src> {
    pub name: Spanned<Ident<'src>>,
    pub ty: Spanned<Ty<'src>>,
}

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Path(Path<'src>),

    Num(&'src str),
    String(&'src str),
    Char(char),
    Bool(bool),

    List(Vec<Spanned<Expr<'src>>>),
    ListIndex(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    Bin(
        Box<Spanned<Expr<'src>>>,
        Spanned<BinOp>,
        Box<Spanned<Expr<'src>>>,
    ),
    Unary(Spanned<UnaryOp>, Box<Spanned<Expr<'src>>>),
    Block(Box<Block<'src>>),
    AnonFunc(Box<AnonFunc<'src>>),
    Binding(Box<Binding<'src>>),

    FunctionCall(Box<Spanned<Expr<'src>>>, Vec<Spanned<Expr<'src>>>),

    FieldAccess(Box<Spanned<Expr<'src>>>, Spanned<&'src str>),
    MethodCall(Box<MethodStyleCall<'src, &'src str>>),
    ChainedFunctionCall(Box<MethodStyleCall<'src, Path<'src>>>),
}

#[derive(Debug, Clone)]
pub struct Ident<'src>(pub &'src str);

/// A generic method-style call, which could be `.`, `:`, or `~`
/// a := "hello world"
/// a     .slice    (" ")
/// ^      ^^^^^     ^^^
/// object function  arguments
///
/// Note that for this to be `.`, parentheses must be present, otherwise it is a field access
#[derive(Debug, Clone)]
pub struct MethodStyleCall<'src, T> {
    pub object: Spanned<Expr<'src>>,
    pub function: Spanned<T>,
    /// The `Option` denotes that there may not be parentheses after the function name.
    /// This is only possible with `:` and `~`
    pub arguments: Option<Vec<Spanned<Expr<'src>>>>,
}

#[derive(Debug, Clone)]
pub struct Path<'src> {
    pub segments: Vec<Spanned<PathSegment<'src>>>,
}

#[derive(Debug, Clone)]
pub struct PathSegment<'src> {
    pub ident: Spanned<Ident<'src>>,
    /// The `Option` denotes that the segment may not have any generics attached
    pub generics: Option<Vec<Spanned<Ty<'src>>>>,
}

#[derive(Debug, Clone)]
pub struct Block<'src> {
    pub items: Vec<Spanned<Item<'src>>>,
    // TODO(lowering): Verify that this is an expression
    pub ret: Option<Spanned<Item<'src>>>,
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
    Ident(Ident<'src>),
}

#[derive(Debug, Clone)]
pub enum Ty<'src> {
    Path(Path<'src>),
    Func(Vec<Spanned<Ty<'src>>>, Box<Spanned<Ty<'src>>>),
}

#[derive(Debug, Clone)]
pub struct AnonFunc<'src> {
    pub params: Vec<Spanned<Param<'src>>>,
    pub ret_ty: Option<Spanned<Ty<'src>>>,
    pub body: Spanned<Expr<'src>>,
}

#[derive(Debug, Clone)]
pub struct Param<'src> {
    pub pat: Spanned<Pat<'src>>,
    pub ty: Spanned<Ty<'src>>,
    pub default: Option<Spanned<Expr<'src>>>,
}
