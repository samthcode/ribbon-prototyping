use std::fmt::{Debug, Display};

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
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO
        write!(f, "{self:#?}")
    }
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone)]
pub struct Binding<'src> {
    pub pat: Spanned<Pat<'src>>,
    pub ty: Option<Spanned<Ty<'src>>>,
    pub val: Spanned<Expr<'src>>,
}

impl Debug for Binding<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Binding")
            .field(
                "pat",
                &format_args!("{:#?}@{}", self.pat.inner, self.pat.span),
            )
            .field(
                "ty",
                &format_args!(
                    "{}@{}",
                    self.ty
                        .clone()
                        .map(|t| format!("{:#?}", t.inner))
                        .unwrap_or("<infer>".to_string()),
                    self.ty.clone().map(|t| t.span).unwrap_or_default()
                ),
            )
            .field("val", &format_args!("{}@{}", self.val.inner, self.val.span))
            .finish()
    }
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
