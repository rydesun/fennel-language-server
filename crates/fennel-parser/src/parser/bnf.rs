use rowan::TextRange;

use crate::{parser::sets::TokenSet, SyntaxKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Rule {
    pub(crate) expect: SyntaxKind,
    pub(crate) notation: Notation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Notation {
    Once,
    #[allow(dead_code)]
    OncePeek(TokenSet),
    Repeat(TokenSet),
    Repeat1(TokenSet),
    Repeat1Peek(TokenSet, TokenSet),
    RepeatPeek(TokenSet, TokenSet),
    Optional(TokenSet),
    Close(Option<TextRange>),
}

impl Rule {
    pub(crate) fn expand(&self) -> Option<Self> {
        use Notation::*;

        let kind = self.expect;
        match self.notation {
            Repeat(l) | Repeat1(l) => {
                Some(Rule { expect: kind, notation: Repeat(l) })
            }
            RepeatPeek(l1, l2) | Repeat1Peek(l1, l2) => {
                Some(Rule { expect: kind, notation: RepeatPeek(l1, l2) })
            }
            _ => None,
        }
    }
}

macro_rules! notation {
    (($kind:ident)) => {
        crate::parser::bnf::Rule { expect: $kind, notation: Once }
    };
    (($kind:ident ? $set:expr)) => {
        crate::parser::bnf::Rule { expect: $kind, notation: Optional($set) }
    };
    (($kind:ident * $set:expr)) => {
        crate::parser::bnf::Rule { expect: $kind, notation: Repeat($set) }
    };
    (($kind:ident + $set:expr)) => {
        crate::parser::bnf::Rule { expect: $kind, notation: Repeat1($set) }
    };
    (($kind:ident ** $set:expr, $set2:expr)) => {
        crate::parser::bnf::Rule {
            expect: $kind,
            notation: RepeatPeek($set, $set2),
        }
    };
    (($kind:ident ++ $set:expr, $set2:expr)) => {
        crate::parser::bnf::Rule {
            expect: $kind,
            notation: Repeat1Peek($set, $set2),
        }
    };
}
