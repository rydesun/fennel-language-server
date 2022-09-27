use std::path::PathBuf;

use rowan::ast::AstNode;

use crate::{
    ast::{func, macros::ast_assoc, models, nodes::*},
    SyntaxKind, SyntaxNode,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum EvalAst {
    NilAst(NilAst),
    LastReturnAst(LastReturnAst),
    CondReturnAst(CondReturnAst),
    MultiReturnAst(MultiReturnAst),
    FuncAst(func::FuncAst),
    Sexp(Sexp),
    Atom(Atom),
    Literal(Literal),
    List(List),
    SymbolCall(SymbolCall),
    Operation(Operation),
    RightSymbol(RightSymbol),
    Unknown(SyntaxNode),
}

#[rustfmt::skip]
ast_assoc!(
    NilAst, [
        Var, Set, Tset, Local, Global, ImportMacros, RequireMacros,
        Macro, Macros, EvalCompiler, Each, For, Lua, Macrodebug,
    ]
);

ast_assoc!(LastReturnAst, [Let, WithOpen, When, Do, Thread, Doto]);

ast_assoc!(CondReturnAst, [Match, If]);

ast_assoc!(MultiReturnAst, [Values, PickValues]);

// Cast anything
impl rowan::ast::AstNode for EvalAst {
    type Language = crate::FennelLanguage;

    fn cast(syntax_node: SyntaxNode) -> Option<Self> {
        if let Some(i) = NilAst::cast(syntax_node.clone()) {
            return Some(Self::NilAst(i));
        }
        if let Some(i) = LastReturnAst::cast(syntax_node.clone()) {
            return Some(Self::LastReturnAst(i));
        }
        if let Some(i) = CondReturnAst::cast(syntax_node.clone()) {
            return Some(Self::CondReturnAst(i));
        }
        if let Some(i) = MultiReturnAst::cast(syntax_node.clone()) {
            return Some(Self::MultiReturnAst(i));
        }
        if let Some(i) = func::FuncAst::cast(syntax_node.clone()) {
            return Some(Self::FuncAst(i));
        }
        if let Some(i) = Sexp::cast(syntax_node.clone()) {
            return Some(Self::Sexp(i));
        }
        if let Some(i) = Atom::cast(syntax_node.clone()) {
            return Some(Self::Atom(i));
        }
        if let Some(i) = Literal::cast(syntax_node.clone()) {
            return Some(Self::Literal(i));
        }
        if let Some(i) = List::cast(syntax_node.clone()) {
            return Some(Self::List(i));
        }
        if let Some(i) = SymbolCall::cast(syntax_node.clone()) {
            return Some(Self::SymbolCall(i));
        }
        if let Some(i) = Operation::cast(syntax_node.clone()) {
            return Some(Self::Operation(i));
        }
        if let Some(i) = RightSymbol::cast(syntax_node.clone()) {
            return Some(Self::RightSymbol(i));
        }
        Some(Self::Unknown(syntax_node))
    }

    fn can_cast(_syntax: SyntaxKind) -> bool {
        true
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::NilAst(n) => n.syntax(),
            Self::LastReturnAst(n) => n.syntax(),
            Self::CondReturnAst(n) => n.syntax(),
            Self::MultiReturnAst(n) => n.syntax(),
            Self::FuncAst(n) => n.syntax(),
            Self::Sexp(n) => n.syntax(),
            Self::Atom(n) => n.syntax(),
            Self::Literal(n) => n.syntax(),
            Self::List(n) => n.syntax(),
            Self::SymbolCall(n) => n.syntax(),
            Self::Operation(n) => n.syntax(),
            Self::RightSymbol(n) => n.syntax(),
            Self::Unknown(n) => n,
        }
    }
}

impl EvalAst {
    pub fn eval_kind(&self) -> models::ValueKind {
        let kind = match self {
            Self::NilAst(_) => Some(models::ValueKind::Nil),
            Self::LastReturnAst(_) => None,
            Self::CondReturnAst(_) => None,
            Self::MultiReturnAst(_) => None,
            Self::FuncAst(_) => Some(models::ValueKind::Func),
            Self::Sexp(n) => n.eval_kind(),
            Self::Atom(n) => n.eval_kind(),
            Self::Literal(n) => n.eval_kind(),
            Self::List(n) => n.eval_kind(),
            Self::SymbolCall(n) => n.eval_kind(),
            Self::Operation(n) => n.eval_kind(),
            Self::RightSymbol(n) => n.eval_kind(),
            Self::Unknown(_) => None,
        };
        kind.unwrap_or(models::ValueKind::Unknown)
    }
}

impl EvalAst {
    pub fn cast_string(&self) -> Option<(String, StringKind)> {
        match self {
            Self::Sexp(n) => n.cast_string(),
            Self::Atom(n) => n.cast_string(),
            Self::Literal(n) => n.cast_string(),
            _ => None,
        }
    }

    pub fn cast_kv_table(&self) -> Option<KvTable> {
        match self {
            Self::Sexp(n) => n.cast_kv_table(),
            Self::Atom(n) => n.cast_kv_table(),
            _ => None,
        }
    }
}

impl Sexp {
    pub fn eval_kind(&self) -> Option<models::ValueKind> {
        EvalAst::cast(self.0.first_child()?).map(|n| n.eval_kind())
    }

    pub fn cast_string(&self) -> Option<(String, StringKind)> {
        EvalAst::cast(self.0.first_child()?).and_then(|n| n.cast_string())
    }

    pub fn cast_kv_table(&self) -> Option<KvTable> {
        EvalAst::cast(self.0.first_child()?).and_then(|n| n.cast_kv_table())
    }
}

impl Atom {
    pub fn eval_kind(&self) -> Option<models::ValueKind> {
        let child = self.0.first_child()?;
        match child.kind() {
            SyntaxKind::N_SEQ_TABLE => Some(models::ValueKind::SeqTable),
            SyntaxKind::N_KV_TABLE => Some(models::ValueKind::KvTable),
            SyntaxKind::N_LITERAL => Literal(child).eval_kind(),
            SyntaxKind::N_R_SYMBOL => RightSymbol(child).eval_kind(),
            _ => None,
        }
    }

    pub fn cast_string(&self) -> Option<(String, StringKind)> {
        let child = self.0.first_child()?;
        match child.kind() {
            SyntaxKind::N_LITERAL => Literal(child).cast_string(),
            _ => None,
        }
    }

    pub fn cast_kv_table(&self) -> Option<KvTable> {
        let child = self.0.first_child()?;
        match child.kind() {
            SyntaxKind::N_KV_TABLE => KvTable::cast(child),
            _ => None,
        }
    }
}

impl Literal {
    pub fn eval_kind(&self) -> Option<models::ValueKind> {
        match self.0.first_token()?.kind() {
            SyntaxKind::FLOAT | SyntaxKind::INTEGER => {
                Some(models::ValueKind::Number)
            }
            SyntaxKind::QUOTE_STRING | SyntaxKind::COLON_STRING => {
                Some(models::ValueKind::String)
            }
            SyntaxKind::BOOL => Some(models::ValueKind::Bool),
            SyntaxKind::NIL => Some(models::ValueKind::Nil),
            _ => None,
        }
    }

    pub(crate) fn cast_string(&self) -> Option<(String, StringKind)> {
        let token = self.syntax().first_token()?;
        match token.kind() {
            SyntaxKind::COLON_STRING => {
                Some((token.text()[1..].to_string(), StringKind::Colon))
            }
            SyntaxKind::QUOTE_STRING => {
                let text = token.text();
                Some((text[1..text.len() - 1].to_string(), StringKind::Quote))
            }
            _ => None,
        }
    }

    pub(crate) fn cast_path(&self) -> Option<PathBuf> {
        let token = self.syntax().first_token()?;
        let s = match token.kind() {
            SyntaxKind::INTEGER => token.text().to_string(),
            SyntaxKind::FLOAT => {
                let token = token.text().to_string();
                // TODO: support hex and exponent
                if token.starts_with('.')
                    || token.ends_with('.')
                    || token.chars().any(|c| !c.is_ascii_digit() && c != '.')
                {
                    return None;
                } else {
                    token
                }
            }
            _ => self.cast_string()?.0,
        };
        if s.starts_with('.') || s.ends_with('.') || s.contains("..") {
            return None;
        }
        let path = PathBuf::from(s.replace('.', "/"));
        Some(path)
    }
}

impl List {
    pub(crate) fn eval_kind(&self) -> Option<models::ValueKind> {
        let sublist = self
            .0
            .children()
            .find(|n| n.kind() == SyntaxKind::N_SUBLIST)?
            .first_child()?;
        match sublist.kind() {
            SyntaxKind::N_SYMBOL_CALL => {
                SymbolCall::cast(sublist)?.eval_kind()
            }
            SyntaxKind::N_INCLUDE => None,
            SyntaxKind::N_PARTIAL | SyntaxKind::N_PICK_ARGS => {
                Some(models::ValueKind::Func)
            }
            SyntaxKind::N_ICOLLECT => Some(models::ValueKind::SeqTable),
            SyntaxKind::N_COLLECT => Some(models::ValueKind::KvTable),
            _ => Some(EvalAst::cast(sublist)?.eval_kind()),
        }
    }
}

impl SymbolCall {
    pub(crate) fn eval_kind(&self) -> Option<models::ValueKind> {
        if self.is_require() {
            Some(
                self.require().map_or(models::ValueKind::Require(None), |p| {
                    models::ValueKind::Require(Some(p))
                }),
            )
        } else {
            None
        }
    }
}

impl Operation {
    pub fn eval_kind(&self) -> Option<models::ValueKind> {
        let first_token = self.0.first_token()?;
        match first_token.kind() {
            SyntaxKind::LENGTH => Some(models::ValueKind::Number),
            SyntaxKind::OPERATOR => match first_token.text() {
                "-" | "/" | "//" | "lshift" | "rshift" | "band" | "bor"
                | "bxor" | "length" => Some(models::ValueKind::Number),
                ">" | "<" | ">=" | "<=" | "~=" | "=" | "not=" => {
                    Some(models::ValueKind::Bool)
                }
                ".." => Some(models::ValueKind::String),
                _ => None,
            },
            SyntaxKind::KEYWORD_OR => None,
            SyntaxKind::COLON => None,
            _ => None,
        }
    }
}

impl RightSymbol {
    // just wrap self
    pub fn eval_kind(&self) -> Option<models::ValueKind> {
        let node = self.syntax();
        if node.children_with_tokens().any(|t| {
            let kind = t.as_token().unwrap().kind();
            kind == SyntaxKind::SYMBOL_FIELD
                || kind == SyntaxKind::SYMBOL_METHOD
        }) {
            None
        } else {
            Some(models::ValueKind::Symbol)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum StringKind {
    Colon,
    Quote,
}
