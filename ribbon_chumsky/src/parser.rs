use chumsky::input::{IterInput, ValueInput};
use chumsky::pratt::*;
use chumsky::prelude::*;
use logos::Logos;

use crate::ast::{BinOp, Binding, Block, Expr, Func, Param, Pat, Ty};
use crate::tok::Token;

pub fn parse_from_source<'toks, 'src: 'toks>(
    src: &'src str,
) -> ParseResult<Vec<Spanned<Binding<'toks>>>, Rich<'toks, Token<'src>>> {
    let toks = Token::lexer(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });

    let stream = IterInput::new(toks, (src.len()..src.len()).into());

    root_parser().parse(stream)
}

fn root_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Vec<Spanned<Binding<'src>>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    // A module is comprised purely of bindings for functions, structs, traits, enums, constants etc.
    binding_parser(expr_parser())
        .separated_by(just(Token::Semi).repeated())
        .allow_trailing()
        .collect()
}

/// Parses a binding e.g. `PAT:TY?=EXPR`
///
/// Note that this needs to take the expression parser as an input because it will be used recursively in the `expr_parser` function
fn binding_parser<'toks, 'src: 'toks, I>(
    expr_parser: impl Parser<
        'toks,
        I,
        Spanned<Expr<'src>>,
        chumsky::extra::Err<Rich<'toks, Token<'src>>>,
    >,
) -> impl Parser<'toks, I, Spanned<Binding<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    pat_parser()
        .then_ignore(just(Token::Colon))
        .then(ty_parser().or_not())
        .then_ignore(just(Token::Eq))
        .then(expr_parser)
        .map(|((pat, ty), val)| Binding { pat, ty, val })
        .spanned()
}

fn expr_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Spanned<Expr<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let binding = binding_parser(expr.clone())
            .map(|b| Expr::Binding(Box::new(b.inner)))
            .spanned();

        let stmt = binding.or(expr.clone());

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
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .map(Expr::List)
            .delimited_by(just(Token::LSquare), just(Token::RSquare))
            .spanned();
        let paren_expr = expr
            .clone()
            .map(|e| e.inner)
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .spanned();

        let block = stmt
            .separated_by(just(Token::Semi).repeated())
            .collect::<Vec<_>>()
            .then(just(Token::Semi).or_not())
            .delimited_by(just(Token::LCurly), just(Token::RCurly))
            .map(|(stmts, semi)| {
                if semi.is_some() || stmts.is_empty() {
                    Expr::Block(Box::new(Block { stmts, ret: None }))
                } else {
                    Expr::Block(Box::new(Block {
                        stmts: stmts[0..stmts.len() - 1].into(),
                        ret: Some(stmts[stmts.len() - 1].clone()),
                    }))
                }
            })
            .spanned()
            .boxed();

        let param = pat_parser()
            .then_ignore(just(Token::Colon))
            .then(ty_parser())
            .then(just(Token::Eq).ignore_then(expr.clone()).or_not())
            .map(|((pat, ty), default)| Param { pat, ty, default })
            .spanned();
        let params = param
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .boxed();
        let r#fn = choice((
            params
                .clone()
                .then(just(Token::MinusGt).ignore_then(ty_parser()))
                .then(
                    just(Token::EqGt)
                        .ignore_then(expr.clone())
                        .or(block.clone()),
                )
                .map(|((params, ty), body)| Func {
                    params,
                    ty: Some(ty),
                    body,
                }),
            params
                .then_ignore(just(Token::EqGt))
                .then(expr)
                .map(|(params, body)| Func {
                    params,
                    ty: None,
                    body,
                }),
        ))
        .map(|f| Expr::Fn(Box::new(f)))
        .spanned()
        .boxed();

        let atom = choice((terminal, r#fn, block, list, paren_expr));

        let infix_op = |prec, tok, bin_op| {
            infix(
                prec,
                just(tok).to(bin_op).spanned(),
                |lhs: Spanned<Expr<'_>>, op, rhs, _| {
                    let span = SimpleSpan::from(lhs.span.start..rhs.span.end);
                    Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(span)
                },
            )
        };
        let pratt = atom.pratt((
            infix_op(left(2), Token::Mul, BinOp::Mul),
            infix_op(left(2), Token::Div, BinOp::Div),
            infix_op(left(1), Token::Plus, BinOp::Add),
            infix_op(left(1), Token::Minus, BinOp::Sub),
        ));
        pratt
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
