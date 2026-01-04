use chumsky::input::{IterInput, ValueInput};
use chumsky::pratt::{infix, left, none, postfix, prefix};
use chumsky::prelude::*;
use logos::Logos;

use crate::ast::*;
use crate::tok::Token;

pub fn parse_from_source<'toks, 'src: 'toks>(
    src: &'src str,
) -> ParseResult<Vec<Spanned<Item<'toks>>>, Rich<'toks, Token<'src>>> {
    let toks = Token::lexer(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });

    let stream = IterInput::new(toks, (src.len()..src.len()).into());

    module_parser().parse(stream)
}

fn module_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Vec<Spanned<Item<'src>>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    // A module is comprised purely of bindings for functions, structs, traits, enums, constants etc.
    item_parser()
        .separated_by(just(Token::Semi).repeated().at_least(1))
        .allow_trailing()
        .collect::<Vec<_>>()
}

fn item_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Spanned<Item<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    let pat = select! {
        Token::Ident(i) => Pat::Ident(Ident(i))
    }
    .spanned()
    .labelled("pattern");

    recursive(|item| {
        let block = item
            .separated_by(just(Token::Semi).repeated().at_least(1))
            .collect::<Vec<_>>()
            .then(just(Token::Semi).or_not())
            .delimited_by(just(Token::LCurly), just(Token::RCurly))
            .map(|(items, semi)| {
                if semi.is_some() || items.is_empty() {
                    Expr::Block(Box::new(Block { items, ret: None }))
                } else {
                    Expr::Block(Box::new(Block {
                        items: items[0..items.len() - 1].into(),
                        ret: Some(items[items.len() - 1].clone()),
                    }))
                }
            })
            .spanned()
            .boxed()
            .labelled("block");

        let expr = recursive(|expr| {
            let path = path_parser(ty_parser()).map(|p| Expr::Path(p.inner).with_span(p.span));

            let terminal = select! {
                Token::LitNumber(n) => Expr::Num(n),
                // TODO: Process the string and deal with unterminated string
                Token::LitString(s) => Expr::String(&s[1..s.len()-1]),
                // TODO: Process characters properly including \u.... etc.
                Token::LitChar(c) => Expr::Char(c[1..2].parse().unwrap()),
                Token::LitBool(b) => Expr::Bool(b)
            }
            .spanned()
            .or(path.clone());

            // Needed as a binding is not allowed everywhere that an expression is
            // let binding_expr = pat
            //     .then_ignore(just(Token::Colon))
            //     .then(ty_parser().or_not())
            //     .then_ignore(just(Token::Eq))
            //     .then(expr.clone())
            //     .map(|((pat, ty), val)| Expr::Binding(Box::new(Binding { pat, ty, val })))
            //     .spanned()
            //     .boxed();
            //
            // let expr_or_binding = expr.clone().or(binding_expr);

            let list = expr
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .map(Expr::List)
                .delimited_by(just(Token::LSquare), just(Token::RSquare))
                .spanned()
                .labelled("list expression");

            let paren_expr = expr
                .clone()
                .map(|e| e.inner)
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .spanned()
                .labelled("parenthesised expression");

            let param = pat
                .then_ignore(just(Token::Colon))
                .then(ty_parser())
                .then(just(Token::Eq).ignore_then(expr.clone()).or_not())
                .map(|((pat, ty), default)| Param { pat, ty, default })
                .spanned()
                .labelled("parameter");
            let params = param
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .labelled("parameter list");
            let anon_func = choice((
                params
                    .clone()
                    .then(just(Token::MinusGt).ignore_then(ty_parser()))
                    .then(
                        just(Token::EqGt)
                            .ignore_then(expr.clone())
                            .or(block.clone()),
                    )
                    .map(|((params, ty), body)| {
                        Expr::AnonFunc(Box::new(AnonFunc {
                            params,
                            ret_ty: Some(ty),
                            body,
                        }))
                    }),
                params
                    .then_ignore(just(Token::EqGt))
                    .then(expr.clone())
                    .map(|(params, body)| {
                        Expr::AnonFunc(Box::new(AnonFunc {
                            params,
                            ret_ty: None,
                            body,
                        }))
                    }),
            ))
            .spanned()
            .boxed()
            .labelled("anonymous function");

            let argument_list = expr
                .clone()
                .labelled("function argument")
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen));

            let field_access_or_method_call = terminal
                .clone()
                .then_ignore(just(Token::Dot))
                .then(select! {Token::Ident(i) => i}.spanned())
                .then(argument_list.clone().or_not())
                .map(|((object, name), arguments)| match arguments {
                    Some(arguments) => Expr::MethodCall(Box::new(MethodStyleCall {
                        object,
                        function: name,
                        arguments: Some(arguments),
                    })),
                    None => Expr::FieldAccess(Box::new(object), name),
                })
                .spanned()
                .boxed();

            let colon_method_call = terminal
                .clone()
                .then_ignore(just(Token::Colon))
                .then(select! {Token::Ident(i) => i}.spanned())
                .then(argument_list.clone().or_not())
                .map(|((object, name), arguments)| {
                    Expr::MethodCall(Box::new(MethodStyleCall {
                        object,
                        function: name,
                        arguments,
                    }))
                })
                .spanned()
                .boxed();

            let chained_function_call = terminal
                .clone()
                .then_ignore(just(Token::Tilde))
                .then(path_parser(ty_parser()))
                .then(argument_list.clone().or_not())
                .map(|((object, function), arguments)| {
                    Expr::ChainedFunctionCall(Box::new(MethodStyleCall {
                        object,
                        function,
                        arguments,
                    }))
                })
                .spanned()
                .boxed();

            let atom = choice((
                field_access_or_method_call,
                colon_method_call,
                chained_function_call,
                terminal,
                anon_func,
                block.clone(),
                list,
                paren_expr,
            ));

            let prefix_op = |prec, tok, unary_op| {
                prefix(
                    prec,
                    just(tok).to(unary_op).spanned(),
                    |op: Spanned<UnaryOp>, rhs: Spanned<Expr<'_>>, s| {
                        Expr::Unary(op, Box::new(rhs)).with_span(s.span())
                    },
                )
            };

            atom.pratt((
                // Propagate error
                infix(
                    left(13),
                    just(Token::TildeQuestion).to(BinOp::ErrProp).spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                // Function call
                postfix(
                    12,
                    argument_list,
                    |object: Spanned<Expr<'_>>, arguments, s| {
                        Expr::FunctionCall(Box::new(object), arguments).with_span(s.span())
                    },
                ),
                // List index
                postfix(
                    12,
                    just(Token::Dot).ignore_then(
                        expr.labelled("list index")
                            .delimited_by(just(Token::LSquare), just(Token::RSquare)),
                    ),
                    |list: Spanned<Expr<'_>>, idx: Spanned<Expr<'src>>, s| {
                        Expr::ListIndex(Box::new(list), Box::new(idx)).with_span(s.span())
                    },
                ),
                // Unary
                prefix_op(11, Token::Minus, UnaryOp::Neg),
                prefix_op(11, Token::Bang, UnaryOp::Not),
                prefix_op(11, Token::Mul, UnaryOp::Deref),
                prefix_op(11, Token::Amp, UnaryOp::Ref),
                // Algebraic
                infix(
                    left(10),
                    select! {
                        Token::Mul => BinOp::Mul,
                        Token::Div => BinOp::Div,
                        Token::Mod => BinOp::Mod
                    }
                    .spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                infix(
                    left(9),
                    select! {
                        Token::Plus => BinOp::Add,
                        Token::Minus => BinOp::Sub
                    }
                    .spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                // Bitwise
                infix(
                    left(8),
                    select! {
                        Token::ShiftL => BinOp::ShiftL,
                        Token::ShiftR => BinOp::ShiftR
                    }
                    .spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                infix(
                    left(7),
                    just(Token::Amp).to(BinOp::BwAnd).spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                infix(
                    left(6),
                    just(Token::Caret).to(BinOp::BwXor).spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                infix(
                    left(5),
                    just(Token::Pipe).to(BinOp::BwOr).spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                // Comparison
                infix(
                    left(4),
                    select! {
                        Token::GtEq => BinOp::GtEquality,
                        Token::LtEq => BinOp::LtEquality,
                        Token::BangEq => BinOp::Inequality,
                        Token::Eq => BinOp::Equality,
                        Token::Gt => BinOp::Gt,
                        Token::Lt => BinOp::Lt,
                    }
                    .spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                infix(
                    left(3),
                    just(Token::And).to(BinOp::LogicalAnd).spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                infix(
                    left(2),
                    just(Token::Or).to(BinOp::LogicalOr).spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                // Ranges
                infix(
                    none(1),
                    select! {
                        Token::DotDot => BinOp::Range,
                        Token::DotDotEq => BinOp::RangeIncl
                    }
                    .spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                // Piping
                infix(
                    left(0),
                    just(Token::ColonGt).to(BinOp::MethodPipe).spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
                infix(
                    left(0),
                    just(Token::TildeGt).to(BinOp::FuncPipe).spanned(),
                    |lhs: Spanned<Expr<'_>>, op, rhs, s| {
                        Expr::Bin(Box::new(lhs), op, Box::new(rhs)).with_span(s.span())
                    },
                ),
            ))
            .labelled("expression")
        });

        let binding = pat
            .then_ignore(just(Token::Colon))
            .then(ty_parser().or_not())
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .map(|((pat, ty), val)| Item::Binding(Binding { pat, ty, val }))
            .spanned();

        let field = select! {Token::Ident(i) => Ident(i)}
            .spanned()
            .then_ignore(just(Token::Colon))
            .then(ty_parser())
            .map(|(name, ty)| Field { name, ty })
            .spanned();
        let struct_ = just(Token::KwType)
            .ignore_then(select! {Token::Ident(i) => Ident(i)}.spanned())
            .then_ignore(just(Token::Eq))
            .then_ignore(just(Token::KwStruct))
            .then(
                field
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LCurly), just(Token::RCurly)),
            )
            .map(|(name, fields)| Item::TypeDef(TypeDef::Struct(name, fields)))
            .spanned();

        // There is unfortunately quite a lot of repetition here
        let param = pat
            .then_ignore(just(Token::Colon))
            .then(ty_parser())
            .then(just(Token::Eq).ignore_then(expr.clone()).or_not())
            .map(|((pat, ty), default)| Param { pat, ty, default })
            .spanned()
            .labelled("parameter");
        let params = param
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .labelled("parameter list");
        let func_def = just(Token::KwFn)
            .ignore_then(select! {Token::Ident(i) => Ident(i)}.spanned())
            .then_ignore(just(Token::Eq))
            .then(params)
            .then(choice((
                just(Token::MinusGt)
                    .ignore_then(ty_parser())
                    .then(
                        just(Token::EqGt)
                            .ignore_then(expr.clone())
                            .or(block.clone()),
                    )
                    .map(|(ty, body)| (Some(ty), body)),
                just(Token::EqGt)
                    .ignore_then(expr.clone())
                    .map(|body| (None, body)),
            )))
            .map(|((name, params), (ret_ty, body))| {
                Item::FuncDef(FuncDef {
                    name,
                    params,
                    ret_ty,
                    body,
                })
            })
            .spanned()
            .boxed()
            .labelled("function definition");

        choice((
            binding,
            func_def,
            expr.map(|e| Item::Expr(e.inner).with_span(e.span)),
            struct_,
        ))
    })
}

fn ty_parser<'toks, 'src: 'toks, I>()
-> impl Parser<'toks, I, Spanned<Ty<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>> + Clone
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|ty| {
        let path = path_parser(ty.clone())
            .map(|path| Ty::Path(path.inner))
            .boxed();

        let func = ty
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then_ignore(just(Token::MinusGt))
            .then(ty)
            .map(|(args, ret)| Ty::Func(args, Box::new(ret)));

        path.or(func).spanned().labelled("type")
    })
}

fn path_parser<'toks, 'src: 'toks, I>(
    ty_parser: impl Parser<'toks, I, Spanned<Ty<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
    + 'toks
    + Clone,
) -> impl Parser<'toks, I, Spanned<Path<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>> + Clone
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    path_segment_parser(ty_parser)
        .separated_by(just(Token::Path))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|segments| Path { segments })
        .spanned()
        .labelled("path")
        .boxed()
}

fn path_segment_parser<'toks, 'src: 'toks, I>(
    ty_parser: impl Parser<'toks, I, Spanned<Ty<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
    + Clone,
) -> impl Parser<'toks, I, Spanned<PathSegment<'src>>, chumsky::extra::Err<Rich<'toks, Token<'src>>>>
+ Clone
where
    I: ValueInput<'toks, Token = Token<'src>, Span = SimpleSpan>,
{
    select! {
        Token::Ident(i) => Ident(i)
    }
    .spanned()
    .then(
        ty_parser
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LSquare), just(Token::RSquare))
            .or_not(),
    )
    .map(|(ident, generics)| PathSegment { ident, generics })
    .spanned()
    .labelled("path segment")
}
