use chumsky::input::{Stream, ValueInput};
use chumsky::prelude::*;
use logos::Logos;

use crate::ast::{BinOp, Binding, Expr, Pat, Ty};
use crate::tok::Token;

pub fn parse_from_source<'toks, 'src: 'toks>(
    src: &'src str,
) -> ParseResult<Vec<Spanned<Binding<'toks>>>, Rich<'toks, Token<'src>>> {
    let toks = Token::lexer(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });

    let stream = Stream::from_iter(toks).map((0..src.len()).into(), |(t, s): (_, _)| (t, s));

    root_parser().parse(stream)
}

fn root_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Vec<Spanned<Binding<'toks>>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    // A module is comprised purely of bindings for functions, structs, traits, enums, constants etc.
    binding_parser()
        .separated_by(just(Token::Semi).repeated())
        .allow_trailing()
        .collect()
}

fn binding_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Spanned<Binding<'toks>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    pat_parser()
        .then_ignore(just(Token::Colon))
        .then(ty_parser().or_not())
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|((pat, ty), val)| Binding { pat, ty, val })
        .spanned()
}

fn expr_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Spanned<Expr<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let terminal = select! {
            Token::Ident(i) => Expr::Var(i),
            Token::LitNumber(n) => Expr::Num(n),
            // TODO: Process the string and deal with unterminated string
            Token::LitString(s) => Expr::String(&s[1..s.len()-1]),
            // TODO: Process characters properly including \u.... etc.
            Token::LitChar(c) => Expr::Char(c[1..2].parse().unwrap()),
            Token::LitBool(b) => Expr::Bool(b)
        }
        .spanned();
        let list = expr
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .map(Expr::List)
            .delimited_by(just(Token::LSquare), just(Token::RSquare))
            .spanned();
        let atom = terminal.or(list);
        let mul = choice((
            atom.clone()
                .then(select! {
                    Token::Mul => BinOp::Mul,
                    Token::Div => BinOp::Div
                })
                .then(atom.clone())
                .map(|((lhs, op), rhs)| Expr::Bin(Box::new(lhs), op, Box::new(rhs)))
                .spanned(),
            atom.clone(),
        ));
        let add = mul
            .clone()
            .then(select! {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
            })
            .then(mul.clone())
            .map(|((lhs, op), rhs)| Expr::Bin(Box::new(lhs), op, Box::new(rhs)))
            .spanned();
        choice((add, mul, atom))
    })
}

fn ty_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Spanned<Ty<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|ty| {
        select! {
            Token::Ident(i) => i
        }
        .then(
            ty.separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LSquare), just(Token::RSquare))
                .or_not(),
        )
        .map(|(name, maybe_args)| match maybe_args {
            Some(args) => Ty::Generic(name, args),
            None => Ty::Concrete(name),
        })
        .spanned()
    })
}

fn pat_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Spanned<Pat<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    select! {
        Token::Ident(i) => Pat::Ident(i)
    }
    .spanned()
}
