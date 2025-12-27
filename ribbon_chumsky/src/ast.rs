use chumsky::span::Spanned;

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Var(&'src str),

    Num(&'src str),
    String(&'src str),
    Char(char),
    Bool(bool),
    
    List(Vec<Spanned<Expr<'src>>>),

    Bin(Box<Spanned<Expr<'src>>>, BinOp, Box<Spanned<Expr<'src>>>),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
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
    Concrete(&'src str),
    Generic(&'src str, Vec<Spanned<Ty<'src>>>),
}

#[derive(Debug, Clone)]
pub struct Fn<'src> {
    pub args: Vec<Spanned<Arg<'src>>>,
    pub body: Vec<Spanned<Expr<'src>>>,
}

#[derive(Debug, Clone)]
pub struct Arg<'src> {
    pub name: &'src str,
    pub default: Option<Spanned<Expr<'src>>>,
}
