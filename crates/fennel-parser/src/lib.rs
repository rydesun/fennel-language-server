mod ast;
mod errors;
mod lexer;
mod parser;
mod syntax;

use std::collections::HashSet;

pub use ast::{models, Ast};
pub use errors::{Error, ErrorKind};
pub use rowan::TextRange;
pub(crate) use syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum FennelLanguage {}
impl rowan::Language for FennelLanguage {
    type Kind = syntax::SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= syntax::SyntaxKind::ROOT as u16);
        raw.0.into()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

type SyntaxNode = rowan::SyntaxNode<FennelLanguage>;
type SyntaxToken = rowan::SyntaxToken<FennelLanguage>;
type SyntaxElement = rowan::SyntaxElement<FennelLanguage>;

pub fn parse(
    text: impl Iterator<Item = char>,
    globals: HashSet<String>,
) -> ast::Ast {
    let parsed = parser::Parser::new(Box::new(text)).parse();
    ast::Ast::new(parsed.green_node, parsed.errors, globals)
}
