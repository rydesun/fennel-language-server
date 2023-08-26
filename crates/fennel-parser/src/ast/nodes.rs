use std::{collections::HashMap, path::PathBuf};

use rowan::{ast::AstNode, TextRange};

use crate::{
    ast::{
        bind::MatchAst,
        error::{Provider, Suppress, SuppressErrorKind},
        eval,
        eval::EvalAst,
        func, models,
    },
    Error,
    ErrorKind::*,
    SyntaxKind, SyntaxNode, SyntaxToken,
};

ast_node!(Root, ROOT);
ast_node!(Sexp, N_SEXP);
ast_node!(Atom, N_ATOM);
ast_node!(Literal, N_LITERAL);
ast_node!(KvTable, N_KV_TABLE);

ast_node!(LeftSymbol, N_L_SYMBOL);
ast_node!(RightSymbol, N_R_SYMBOL);
// Be careful that FuncName does not equal to LeftRightSymbol
ast_node!(LeftRightSymbol, N_L_R_SYMBOL);
ast_node!(LeftOrRightSymbol, N_L_OR_R_SYMBOL);

ast_node!(List, N_LIST);
ast_node!(SubList, N_SUBLIST);
ast_node!(Operation, N_OPERATION);
ast_node!(Func, N_FN);
ast_node!(Lambda, N_LAMBDA);
ast_node!(Var, N_VAR);
ast_node!(Set, N_SET);
ast_node!(Tset, N_TSET);
ast_node!(Local, N_LOCAL);
ast_node!(Global, N_GLOBAL);
ast_node!(Let, N_LET);
ast_node!(Match, N_MATCH);
ast_node!(MatchTry, N_MATCH_TRY);
ast_node!(Catch, N_CATCH);
ast_node!(If, N_IF);
ast_node!(Values, N_VALUES);
ast_node!(PickValues, N_PICK_VALUE);
ast_node!(WithOpen, N_WITH_OPEN);
ast_node!(Each, N_EACH);
ast_node!(For, N_FOR);
ast_node!(Do, N_DO);
ast_node!(Thread, N_THREAD);
ast_node!(Doto, N_DOTO);
ast_node!(When, N_WHEN);
ast_node!(Icollect, N_ICOLLECT);
ast_node!(Fcollect, N_FCOLLECT);
ast_node!(Collect, N_COLLECT);
ast_node!(Accumulate, N_ACCUMULATE);
ast_node!(ImportMacros, N_IMPORT_MACROS);
ast_node!(RequireMacros, N_REQUIRE_MACROS);
ast_node!(PickArgs, N_PICK_ARGS);
ast_node!(Macro, N_MACRO);
ast_node!(Macros, N_MACROS);
ast_node!(EvalCompiler, N_EVAL_COMPILER);
ast_node!(SymbolCall, N_SYMBOL_CALL);
ast_node!(Lua, N_LUA);
ast_node!(Macrodebug, N_MACRODEBUG);
ast_node!(IntoClause, N_INTO_CLAUSE);
ast_node!(UntilClause, N_UNTIL_CLAUSE);
ast_node!(MacroQuote, N_MACRO_QUOTE);
ast_node!(CommentForm, N_COMMENT_FORM);

ast_assoc!(BindingSymbol, [LeftSymbol, LeftRightSymbol]);

// TODO: merge?
ast_assoc!(Symbol, [LeftSymbol, LeftRightSymbol, LeftOrRightSymbol]);

impl Symbol {
    pub(crate) fn id(&self) -> Option<(models::Token, bool, bool)> {
        let mut nodes = self.syntax().children_with_tokens();
        let first_node = nodes.next()?;
        let first_token = first_node.as_token()?;
        let prefix_is_comma = first_token.kind() == SyntaxKind::COMMA;
        if prefix_is_comma {
            let first_token = nodes.next()?;
            Some((
                first_token.as_token()?.to_owned().into(),
                true,
                nodes.next().is_none(),
            ))
        } else {
            Some((
                first_token.to_owned().into(),
                false,
                nodes.next().is_none(),
            ))
        }
    }
}

impl Root {
    pub(crate) fn return_value(&self) -> Option<EvalAst> {
        let last_node = self.syntax().children().last()?;
        eval::EvalAst::cast(last_node)
    }

    pub(crate) fn return_kv_table(
        &self,
    ) -> Option<HashMap<String, eval::EvalAst>> {
        self.return_value()
            .and_then(|eval_ast| eval_ast.cast_kv_table())
            .map(|kv_table| kv_table.cast_hashmap())
    }

    pub(crate) fn resources(&self) -> impl Iterator<Item = PathBuf> {
        self.syntax()
            .descendants()
            .filter_map(SymbolCall::cast)
            .filter_map(|n| n.require())
    }

    pub(crate) fn provide_errors(&self) -> impl Iterator<Item = Error> {
        self.syntax()
            .descendants()
            .filter_map(Provider::cast)
            .flat_map(|n| n.errors())
            .flatten()
    }

    pub(crate) fn suppress_errors(
        &self,
    ) -> impl Iterator<Item = (TextRange, Vec<SuppressErrorKind>)> {
        self.syntax()
            .descendants()
            .filter_map(Suppress::cast)
            .flat_map(|n| n.suppress())
    }

    pub(crate) fn r_symbols(&self) -> Vec<models::RSymbol> {
        let mut count_hashfn = 0;
        let mut count_macro = 0;

        let event_macro = |kind: SyntaxKind| -> bool {
            Macro::can_cast(kind) || Macros::can_cast(kind)
        };
        let event_hashfn =
            |kind: SyntaxKind| -> bool { kind == SyntaxKind::N_MACRO_HASH };
        self.syntax()
            .preorder()
            .flat_map(|event| match event {
                rowan::WalkEvent::Enter(n) => {
                    let n_kind = n.kind();
                    if event_macro(n_kind) {
                        count_macro += 1;
                    }
                    if event_hashfn(n_kind) {
                        count_hashfn += 1;
                    }
                    let refer_symbol = ReferSymbol::cast(n.clone())
                        .and_then(|n| n.name())
                        .map(|(token, starts_with_comma)| {
                            if count_hashfn > 0 && token.text.starts_with('$')
                            {
                                return models::RSymbol {
                                    token,
                                    special: models::SpecialKind::HashArg,
                                };
                            }
                            if count_macro > 0 {
                                if starts_with_comma {
                                    return models::RSymbol {
                                        token,
                                        special:
                                            models::SpecialKind::MacroUnquote,
                                    };
                                } else {
                                    return models::RSymbol {
                                        token,
                                        special:
                                            models::SpecialKind::MacroWrap,
                                    };
                                }
                            }
                            models::RSymbol {
                                token,
                                special: models::SpecialKind::Normal,
                            }
                        });
                    let fn_name = || {
                        func::FuncAst::cast(n).and_then(|f| f.r_name()).map(
                            |s| models::RSymbol {
                                special: models::SpecialKind::Normal,
                                token: s,
                            },
                        )
                    };
                    refer_symbol.or_else(fn_name)
                }
                rowan::WalkEvent::Leave(n) => {
                    let n_kind = n.kind();
                    if event_macro(n_kind) {
                        count_macro -= 1;
                    }
                    if event_hashfn(n_kind) {
                        count_hashfn -= 1;
                    }
                    None
                }
            })
            .collect()
    }

    pub(crate) fn correct_symbols(
        &self,
        l_symbols: &mut models::LSymbols,
    ) -> Vec<models::RSymbol> {
        let mut r_symbols = vec![];
        for r in self
            .syntax()
            .descendants()
            .filter_map(MatchAst::cast)
            .map(|n| n.l_or_r_symbols(l_symbols))
        {
            r_symbols.extend(r);
        }
        r_symbols
    }

    // TODO: refactor
    pub(crate) fn delimiter_whitespace_errors(
        &self,
    ) -> impl Iterator<Item = Error> {
        const DELIMITER: &[SyntaxKind] = &[
            SyntaxKind::L_PAREN,
            SyntaxKind::L_BRACE,
            SyntaxKind::L_BRACKET,
            SyntaxKind::HASHFN,
            SyntaxKind::COMMA,
            SyntaxKind::BACKTICK,
            // FIXME: should fix lexer
            SyntaxKind::LENGTH,
        ];

        self.syntax()
            .children()
            .filter_map(Sexp::cast)
            .map(|n| n.syntax().descendants_with_tokens())
            .flat_map(|n| n.skip_while(|n| n.as_node().is_some()).skip(1))
            .filter_map(|n| n.into_token())
            .filter(|t| DELIMITER.contains(&t.kind()))
            .flat_map(|token| {
                let mut prev_token = token.prev_token()?;
                loop {
                    if prev_token.kind() == SyntaxKind::COMMENT {
                        prev_token = prev_token.prev_token()?;
                    } else {
                        break;
                    }
                }
                if prev_token.kind() != SyntaxKind::WHITESPACE
                    && !DELIMITER.contains(&prev_token.kind())
                {
                    Some(Error::new(token.text_range(), MissingWhitespace))
                } else {
                    None
                }
            })
    }
}

ast_assoc!(ReferSymbol, [RightSymbol, LeftRightSymbol]);

impl ReferSymbol {
    pub(crate) fn name(&self) -> Option<(models::Token, bool)> {
        let mut nodes = self.syntax().children_with_tokens();
        let first_node = nodes.next()?;
        let first_token = first_node.as_token()?;
        let prefix_is_comma = first_token.kind() == SyntaxKind::COMMA;
        let prev_is_comma = first_token
            .prev_token()
            .map(|t| t.kind() == SyntaxKind::COMMA)
            .unwrap_or(false);
        // TODO: improve syntax
        if prefix_is_comma {
            Some((nodes.next()?.as_token()?.to_owned().into(), true))
        } else if prev_is_comma {
            Some((first_token.to_owned().into(), true))
        } else {
            Some((first_token.to_owned().into(), false))
        }
    }
}

impl KvTable {
    pub(crate) fn get(&self, key: String) -> Option<EvalAst> {
        self.syntax()
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_KV_PAIR)
            .map(|n| {
                (
                    n.first_child().unwrap(),
                    n.children()
                        .skip(1)
                        .find(|n| n.kind() == SyntaxKind::N_VALUE),
                )
            })
            .find_map(|(key_node, value_node)| {
                let k_str = key_node
                    .first_child()
                    .and_then(eval::EvalAst::cast)
                    .and_then(|n| n.cast_string());
                if k_str.map_or(false, |(s, _)| s == key) {
                    value_node
                        .and_then(|v| v.first_child())
                        .and_then(EvalAst::cast)
                } else {
                    None
                }
            })
    }

    pub(crate) fn iter(
        &self,
    ) -> impl Iterator<Item = (Option<EvalAst>, Option<EvalAst>)> {
        self.syntax()
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_KV_PAIR)
            .map(|n| {
                (
                    n.first_child()
                        .and_then(|n| n.first_child())
                        .and_then(EvalAst::cast),
                    n.children()
                        .skip(1)
                        .find(|n| n.kind() == SyntaxKind::N_VALUE)
                        .and_then(|n| n.first_child())
                        .and_then(EvalAst::cast),
                )
            })
    }

    // skip non-string key
    pub(crate) fn cast_hashmap(&self) -> HashMap<String, EvalAst> {
        let mut res = HashMap::new();
        for (k, v) in self.iter() {
            if k.is_none() || v.is_none() {
                continue;
            }
            let (k, v) = (k.unwrap(), v.unwrap());
            if let Some((key, _)) = k.cast_string() {
                res.insert(key, v);
            } else if k.syntax().text() == ":" {
                let l_r_symbol = match Symbol::cast(v.syntax().clone()) {
                    Some(n) => n,
                    None => {
                        continue;
                    }
                };
                let (key, _, is_pure) = match l_r_symbol.id() {
                    Some(res) => res,
                    None => {
                        continue;
                    }
                };
                if is_pure {
                    res.insert(key.text, v);
                }
            }
        }
        res
    }
}

impl SymbolCall {
    pub(crate) fn call_name(&self) -> Option<String> {
        self.syntax().first_token().map(|t| t.to_string())
    }

    pub(crate) fn is_require(&self) -> bool {
        // TODO: follow symbol
        self.call_name().map(|name| name == "require").unwrap_or(false)
    }

    pub(crate) fn require(&self) -> Option<PathBuf> {
        if self.is_require() {
            let deepest_node =
                self.syntax().children().nth(1)?.first_token()?.parent()?;
            get_ancestor::<Literal>(&deepest_node)?.cast_path()
        } else {
            None
        }
    }
}

impl TryFrom<SyntaxToken> for Literal {
    type Error = ();

    fn try_from(token: SyntaxToken) -> Result<Self, Self::Error> {
        token.parent_ancestors().find_map(Self::cast).ok_or(())
    }
}

pub(crate) fn get_ancestor<T: AstNode<Language = crate::FennelLanguage>>(
    start: &SyntaxNode,
) -> Option<T> {
    let mut get_sexp = false;
    for n in start.ancestors() {
        if Sexp::can_cast(n.kind()) {
            if get_sexp {
                return None;
            } else {
                get_sexp = true;
                continue;
            }
        }
        if let Some(node) = T::cast(n) {
            return Some(node);
        }
    }
    None
}
