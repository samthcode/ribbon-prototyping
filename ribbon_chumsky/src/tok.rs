use std::fmt::Display;

use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token<'src> {
    Error,

    // Keywords
    #[token("const")]
    KwConst,
    #[token("struct")]
    KwStruct,
    #[token("trait")]
    KwTrait,
    #[token("enum")]
    KwEnum,
    #[token("return")]
    KwReturn,
    #[token("use")]
    KwUse,
    #[token("for")]
    KwFor,
    #[token("while")]
    KwWhile,

    // Literals
    #[regex(r"(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice())]
    LitNumber(&'src str),
    #[regex(r#""([^"\\\x00-\x1F]|\\(['"\\bnfrt/]|u[a-fA-F0-9]{4}))*""#, |lex| lex.slice())]
    LitString(&'src str),
    #[regex(r#""([^"\\\x00-\x1F]|\\(['"\\bnfrt/]|u[a-fA-F0-9]{4}))*$"#, |lex| lex.slice())]
    LitUnterminatedString(&'src str),
    #[regex(r#"'([^"\\\x00-\x1F]|\\(['"\\bnfrt/]|u[a-fA-F0-9]{4}))'"#, |lex| lex.slice())]
    LitChar(&'src str),
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    LitBool(bool),

    // Identifier
    #[regex(r"[\p{L}\p{M}][\p{L}\p{M}\p{Nd}]*(\?|!)?", |lex| lex.slice())]
    Ident(&'src str),

    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,

    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token("@")]
    At,
    #[token("#")]
    Hash,
    #[token("~")]
    Tilde,

    #[token("&")]
    Amp,
    #[token("^")]
    Caret,
    #[token("|")]
    Pipe,
    #[token("<<")]
    ShiftL,
    #[token(">>")]
    ShiftR,

    #[token("!")]
    Bang,

    #[token("$")]
    Dollar,

    #[token("<")]
    Lt,
    #[token(">")]
    Gt,

    #[token("==")]
    EqEq,
    #[token("!=")]
    BangEq,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,

    #[token("=")]
    Eq,
    #[token("&=")]
    AmpEq,
    #[token("^=")]
    CaretEq,
    #[token("|=")]
    PipeEq,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    MulEq,
    #[token("/=")]
    DivEq,
    #[token("%=")]
    ModEq,
    #[token(".=")]
    DotEq,
    #[token("&&=")]
    AndEq,
    #[token("||=")]
    OrEq,
    #[token("<<=")]
    ShiftLEq,
    #[token(">>=")]
    ShiftREq,

    #[token("&&")]
    And,
    #[token("||")]
    Or,

    #[token("::")]
    Path,

    #[token("..")]
    DotDot,
    #[token("..=")]
    DotDotEq,

    #[token(":>")]
    ColonGt,
    #[token("~>")]
    TildeGt,
    #[token("->")]
    MinusGt,
    #[token("=>")]
    EqGt,

    #[token("~?")]
    TildeQuestion,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        write!(
            f,
            "{}",
            match self {
                Plus => "+",
                Minus => "-",
                Mul => "*",
                Div => "/",
                Mod => "%",
                LParen => "(",
                RParen => ")",
                LSquare => "[",
                RSquare => "]",
                LCurly => "{",
                RCurly => "}",
                Comma => ",",
                Dot => ".",
                Colon => ":",
                Semi => ";",
                At => "@",
                Hash => "#",
                Tilde => "~",
                Amp => "&",
                Caret => "^",
                Pipe => "|",
                Bang => "!",
                Eq => "=",
                Dollar => "$",
                Lt => "<",
                Gt => ">",
                EqEq => "==",
                BangEq => "!=",
                LtEq => "<=",
                GtEq => ">=",
                AmpEq => "&=",
                CaretEq => "^=",
                PipeEq => "|=",
                PlusEq => "+=",
                MinusEq => "-=",
                MulEq => "*=",
                DivEq => "/=",
                ModEq => "%=",
                DotEq => ".=",
                And => "&&",
                Or => "||",
                Path => "::",
                DotDot => "..",
                ColonGt => ":>",
                TildeGt => "~>",
                TildeQuestion => "~?",
                ShiftL => "<<",
                ShiftR => ">>",
                AndEq => "&&=",
                OrEq => "||=",
                ShiftLEq => "<<=",
                ShiftREq => ">>=",
                DotDotEq => "..=",
                MinusGt => "->",
                EqGt => "=>",
                Error => "<error>",
                KwConst => "const",
                KwStruct => "struct",
                KwTrait => "trait",
                KwEnum => "enum",
                KwReturn => "return",
                KwUse => "use",
                KwFor => "for",
                KwWhile => "while",
                LitNumber(_) => "number",
                LitString(_) => "string",
                LitUnterminatedString(_) => "unterminated string",
                LitChar(_) => "character",
                LitBool(_) => "bool",
                Ident(_) => "identifier",
            }
        )
    }
}
