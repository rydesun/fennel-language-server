use rowan::{ast::AstNode, TextRange};

use crate::{
    ast::{eval::EvalAst, func::FuncAst, macros::ast_assoc, nodes::*},
    models, Error,
    ErrorKind::*,
    SyntaxKind, SyntaxNode,
};

ast_assoc!(Provider, [
    List,
    SubList,
    FuncAst,
    BindingSymbol,
    RightSymbol,
    MatchTry,
    RequireMacros,
    PickArgs,
    Global,
    IntoClause,
    UntilClause,
]);

impl List {
    fn macro_whitespace(&self) -> Option<Error> {
        const MACRO_CHAR: &[SyntaxKind] =
            &[SyntaxKind::HASHFN, SyntaxKind::COMMA, SyntaxKind::BACKTICK];

        let node = self.syntax();
        let first_token = node.first_token()?;
        if !MACRO_CHAR.contains(&first_token.kind()) {
            return None;
        }

        let next = first_token.next_token()?;
        if next.kind() == SyntaxKind::WHITESPACE {
            Some(Error::new(first_token.text_range(), MacroWhitespace))
        } else {
            None
        }
    }

    fn empty_list(&self) -> Option<Error> {
        let node = self.syntax();
        if !node
            .first_token()
            .map(|t| t.kind() == SyntaxKind::L_PAREN)
            .unwrap_or(false)
        {
            return None;
        }
        if !node.children_with_tokens().any(|c| {
            let kind = c.kind();
            kind == SyntaxKind::N_SUBLIST || kind == SyntaxKind::ERROR
        }) {
            Some(Error::new(node.text_range(), EmptyList))
        } else {
            None
        }
    }
}

impl SubList {
    fn literal_call(&self) -> Option<Error> {
        let eval_ast = self.syntax().first_child().and_then(|n| {
            let kind = n.kind();
            if kind == SyntaxKind::N_LIST {
                EvalAst::cast(n)
            } else if kind == SyntaxKind::N_SYMBOL_CALL {
                n.first_child().and_then(EvalAst::cast)
            } else {
                None
            }
        });
        if let Some(eval_ast) = eval_ast {
            let kind = eval_ast.eval_kind();
            if [
                models::ValueKind::Nil,
                models::ValueKind::Bool,
                models::ValueKind::Number,
                models::ValueKind::String,
                models::ValueKind::SeqTable,
                models::ValueKind::KvTable,
            ]
            .contains(&kind)
            {
                return Some(Error::new(
                    eval_ast.syntax().text_range(),
                    LiteralCall(kind),
                ));
            }
            if [models::ValueKind::Module, models::ValueKind::FileHandle]
                .contains(&kind)
            {
                return Some(Error::new(
                    eval_ast.syntax().text_range(),
                    DirectCall(kind),
                ));
            }
        }
        None
    }
}

impl FuncAst {
    fn def_method(&self) -> Option<Error> {
        self.name().and_then(|(node, is_pure)| {
            if is_pure {
                return None;
            }
            node.children_with_tokens()
                .find(|t| {
                    t.as_token().unwrap().kind() == SyntaxKind::SYMBOL_METHOD
                })
                .map(|_| Error::new(node.text_range(), MethodNotAllowed))
        })
    }
}

impl BindingSymbol {
    pub(crate) fn field_and_method(&self) -> Option<Error> {
        let node = self.syntax();
        node.children_with_tokens().find_map(|t| {
            let kind = t.as_token().unwrap().kind();
            match kind {
                SyntaxKind::SYMBOL_FIELD | SyntaxKind::SYMBOL_METHOD => Some(
                    Error::new(node.text_range(), FieldAndMethodNotAllowed),
                ),
                _ => None,
            }
        })
    }
}

impl RightSymbol {
    fn method_call(&self) -> Option<Error> {
        let node = self.syntax();
        if node.parent().unwrap().kind() == SyntaxKind::N_SYMBOL_CALL {
            return None;
        }
        node.children_with_tokens().find_map(|t| {
            let kind = t.as_token().unwrap().kind();
            match kind {
                SyntaxKind::SYMBOL_METHOD => {
                    Some(Error::new(node.text_range(), MethodNotAllowed))
                }
                _ => None,
            }
        })
    }
}

impl MatchTry {
    fn catch(&self) -> Vec<Option<Error>> {
        let node = self.syntax();
        let catchs: Vec<SyntaxNode> = node
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_MATCH_TRY_CLAUSE)
            .filter(|n| {
                n.first_child().unwrap().kind() == SyntaxKind::N_CATCH_LIST
            })
            .collect();
        let mut res = vec![];

        if catchs.len() > 1 {
            res.extend(
                catchs[..catchs.len() - 1]
                    .iter()
                    .map(|n| Some(Error::new(n.text_range(), MultiCatch))),
            );
        }
        if let Some(last_catch) = catchs.last() {
            let last_clause =
                self.syntax().last_child().unwrap().first_child().unwrap();
            if last_clause.kind() != SyntaxKind::N_CATCH_LIST {
                res.push(Some(Error::new(
                    last_catch.text_range(),
                    CatchNotLast,
                )));
            }
        }

        res
    }
}

impl RequireMacros {
    pub(crate) fn depcrated(&self) -> Error {
        Error::new(
            self.syntax().first_token().unwrap().text_range(),
            Depcrated("0.4.0", "import-macros"),
        )
    }
}

impl PickArgs {
    pub(crate) fn depcrated(&self) -> Error {
        Error::new(
            self.syntax().first_token().unwrap().text_range(),
            Depcrated("0.10.0", "pick-values"),
        )
    }
}

impl Global {
    pub(crate) fn depcrated(&self) -> Error {
        Error::new(
            self.syntax().first_token().unwrap().text_range(),
            Depcrated("1.1.0", "_G table"),
        )
    }
}

impl IntoClause {
    pub(crate) fn depcrated(&self) -> Option<Error> {
        let token = self.syntax().first_token().unwrap();
        if token.text().starts_with(':') {
            Some(Error::new(token.text_range(), Depcrated("1.2.0", "&into")))
        } else {
            None
        }
    }
}

impl UntilClause {
    pub(crate) fn depcrated(&self) -> Option<Error> {
        let token = self.syntax().first_token().unwrap();
        if token.text().starts_with(':') {
            Some(Error::new(token.text_range(), Depcrated("1.2.0", "&until")))
        } else {
            None
        }
    }
}

impl Provider {
    pub(crate) fn errors(&self) -> impl Iterator<Item = Option<Error>> {
        match self {
            Self::List(n) => {
                vec![n.macro_whitespace(), n.empty_list()].into_iter()
            }
            Self::SubList(n) => vec![n.literal_call()].into_iter(),
            Self::FuncAst(n) => vec![n.def_method()].into_iter(),
            Self::BindingSymbol(n) => vec![n.field_and_method()].into_iter(),
            Self::RightSymbol(n) => vec![n.method_call()].into_iter(),
            Self::MatchTry(n) => n.catch().into_iter(),
            Self::RequireMacros(n) => vec![Some(n.depcrated())].into_iter(),
            Self::PickArgs(n) => vec![Some(n.depcrated())].into_iter(),
            Self::Global(n) => vec![Some(n.depcrated())].into_iter(),
            Self::IntoClause(n) => vec![n.depcrated()].into_iter(),
            Self::UntilClause(n) => vec![n.depcrated()].into_iter(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum SuppressErrorKind {
    Unused,
    Unterminated,
    Undefined,
    Unexpected,
}

ast_assoc!(Suppress, [MacroQuote]);

impl MacroQuote {
    pub(crate) fn suppress(&self) -> (TextRange, Vec<SuppressErrorKind>) {
        let range = self.syntax().text_range();
        (range, vec![
            SuppressErrorKind::Unused,
            SuppressErrorKind::Unterminated,
            SuppressErrorKind::Undefined,
            SuppressErrorKind::Unexpected,
        ])
    }
}

impl Suppress {
    pub(crate) fn suppress(&self) -> (TextRange, Vec<SuppressErrorKind>) {
        match self {
            Self::MacroQuote(n) => n.suppress(),
        }
    }
}
