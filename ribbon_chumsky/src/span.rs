use chumsky::span::SimpleSpan;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Span {
    start: u32,
    end: u32,
}

impl From<SimpleSpan> for Span {
    fn from(value: SimpleSpan) -> Self {
        Self {
            start: value.start as u32,
            end: value.end as u32,
        }
    }
}
