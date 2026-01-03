use ariadne::{Color, Label, Report, ReportKind, Source};

mod arena;
mod ast;
mod ir;
mod lower;
mod parser;
mod span;
mod tok;

use parser::parse_from_source;

use crate::lower::Lowerer;

fn main() {
    let input = std::env::args()
        .nth(1)
        .expect("expected input wrapped in `\"` to lex");
    let result = parse_from_source(&input);
    match result.into_result() {
        Ok(ast) => {
            println!("{ast:#?}");
            let ir = Lowerer::lower(ast);
            println!("{ir:#?}");
        }
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, ((), err.span().into_range()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(((), err.span().into_range()))
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(&input))
                    .unwrap();
            }
        }
    }
}
