use core::fmt;
use std::{
    collections::BTreeMap,
    ops::Bound::{Excluded, Included},
};

use rowan::TextRange;

use crate::SyntaxToken;

// TODO: TextRange as key
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct LSymbols(pub(crate) BTreeMap<u32, LSymbol>);

pub(crate) type RSymbols = Vec<RSymbol>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LSymbol {
    pub token: Token,
    pub scope: Scope,
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RSymbol {
    pub(crate) token: Token,
    pub(crate) special: SpecialKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub text: String,
    pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Scope {
    pub kind: ScopeKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value {
    pub kind: ValueKind,
    pub range: Option<TextRange>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScopeKind {
    Func,
    Lambda,
    Param,
    MacroParam,
    Local,
    Var,
    Let,
    WithOpen,
    IterValue,
    AccuValue,
    Global,
    Macro,
    Match,
    MatchTry,
    Catch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueKind {
    Func,
    Param,
    MacroParam,
    Match,
    Number,
    String,
    Nil,
    Bool,
    SeqTable,
    KvTable,
    Macro,
    Module,
    FileHandle,
    Unknown,
    Symbol,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SpecialKind {
    Normal,
    MacroWrap,
    MacroUnquote,
    HashArg,
}

impl LSymbol {
    pub(crate) fn contains_token(&self, r_symbol: &Token) -> bool {
        self.token.text == r_symbol.text
            && self.scope.range.contains_range(r_symbol.range)
    }
}

impl LSymbols {
    pub(crate) fn new(symbols: impl Iterator<Item = LSymbol>) -> Self {
        Self(symbols.map(|s| (s.token.range.start().into(), s)).collect())
    }

    pub(crate) fn range(&self, offset: u32) -> impl Iterator<Item = &LSymbol> {
        self.0.range((Included(0), Excluded(offset))).rev().map(|(_, s)| s)
    }

    pub(crate) fn nearest(&self, token: &Token) -> Option<&LSymbol> {
        self.range(token.range.start().into())
            .find(|l_symbol| l_symbol.contains_token(token))
    }

    pub(crate) fn get(&self, start: u32) -> Option<&LSymbol> {
        self.0.get(&start)
    }
}

impl From<SyntaxToken> for Token {
    fn from(token: SyntaxToken) -> Self {
        Token { text: token.text().to_string(), range: token.text_range() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CompletionKind {
    Keyword,
    Func,
    Module,
    Field,
    Operator,
    Var,
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Func => "function",
            Self::Param => "parameter",
            Self::Match => "match pattern",
            Self::Number => "number",
            Self::String => "string",
            Self::Nil => "nil",
            Self::Bool => "bool",
            Self::SeqTable => "sequential table",
            Self::KvTable => "key/value table",
            Self::Macro => "macro",
            Self::MacroParam => "macro parameter",
            Self::Module => "module",
            Self::FileHandle => "file handle",
            Self::Symbol => "symbol",
            Self::Unknown => "(lsp:unknown)",
        };
        write!(f, "{}", name)
    }
}
