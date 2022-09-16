use rowan::TextRange;

use crate::{models::ValueKind, syntax::SyntaxKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorKind {
    Unexpected(SyntaxKind),
    UnexpectedVarargs,
    UnexpectedEof,
    EmptyList,
    Dismatched,
    Unterminated(SyntaxKind),
    Undefined,
    MissingWhitespace,
    MacroWhitespace,
    InvalidSymbol,
    MethodNotAllowed,
    FieldAndMethodNotAllowed,
    LiteralCall(ValueKind),
    DirectCall(ValueKind),
    MultiCatch,
    CatchNotLast,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Error {
    pub range: TextRange,
    pub kind: ErrorKind,
}

impl Error {
    pub(crate) fn new(range: TextRange, kind: ErrorKind) -> Self {
        Error { range, kind }
    }
}
