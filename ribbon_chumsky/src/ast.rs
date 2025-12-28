use std::fmt::Debug;

use chumsky::span::Spanned;

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Var(&'src str),

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

    Block(Box<Block<'src>>),

    Fn(Box<Fn<'src>>),
}

#[derive(Debug, Clone)]
pub struct Block<'src> {
    pub stmts: Vec<Spanned<Expr<'src>>>,
    pub ret: Option<Spanned<Expr<'src>>>,
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
