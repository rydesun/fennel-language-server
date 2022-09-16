use rowan::ast::AstNode;

use super::eval::EvalAst;
use crate::{
    ast::nodes::*, models, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken,
};

ast_assoc!(FuncAst, [Func, Lambda]);

impl FuncAst {
    pub(crate) fn name(&self) -> Option<(SyntaxNode, bool)> {
        let name = self
            .syntax()
            .children()
            .find(|n| n.kind() == SyntaxKind::N_FN_NAME)?;

        let is_pure = !name.children_with_tokens().any(|t| {
            [
                SyntaxKind::SYMBOL_METHOD,
                SyntaxKind::SYMBOL_FIELD,
                SyntaxKind::ERROR,
            ]
            .contains(&t.kind())
        });

        Some((name, is_pure))
    }

    #[allow(unused)]
    pub(crate) fn l_name(&self) -> Option<models::Token> {
        self.name().and_then(|(name, is_pure)| {
            if is_pure {
                name.first_token().map(models::Token::from)
            } else {
                None
            }
        })
    }

    pub(crate) fn r_name(&self) -> Option<models::Token> {
        self.name().and_then(|(name, is_pure)| {
            if is_pure {
                None
            } else {
                name.first_token().map(models::Token::from)
            }
        })
    }

    pub(crate) fn varargs(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children()
            .find(|n| n.kind() == SyntaxKind::N_PARAM_TABLE)?
            .descendants_with_tokens()
            .collect::<Vec<SyntaxElement>>()
            .into_iter()
            .rev()
            .find(|n| n.kind() == SyntaxKind::VARARG)
            .and_then(|n| n.into_token())
    }

    pub(crate) fn docstring(&self) -> Option<String> {
        let body = self
            .syntax()
            .children()
            .skip(1)
            .find(|n| n.kind() == SyntaxKind::N_BODY)?;
        let mut sexp =
            body.children().filter(|n| n.kind() == SyntaxKind::N_SEXP);

        let docstring = sexp.next()?;
        // docstring can't be last
        sexp.next()?;

        let eval = EvalAst::cast(docstring)?;
        match eval.eval_kind() {
            models::ValueKind::KvTable => eval
                .cast_kv_table()
                .and_then(|t| t.get("fnl/docstring".to_owned()))
                .and_then(|eval| eval.cast_string()),
            models::ValueKind::String => eval.cast_string(),
            _ => None,
        }
    }
}

pub(crate) fn from(token: SyntaxToken) -> Option<FuncAst> {
    token.parent()?.parent().and_then(FuncAst::cast)
}
