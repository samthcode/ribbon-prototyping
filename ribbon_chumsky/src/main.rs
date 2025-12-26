use logos::Logos;

mod tok;

fn main() {
    let input = std::env::args()
        .nth(1)
        .expect("expected input wrapped in `\"` to lex");
    let lexer = tok::Token::lexer(&input).spanned();
    for (t, span) in lexer {
        println!("{span:?} {t:?}")
    }
}
