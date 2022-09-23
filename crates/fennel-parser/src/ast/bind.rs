use rowan::{ast::AstNode, TextRange};

use crate::{
    ast::{eval, func::FuncAst, macros::ast_assoc, models, nodes::*},
    SyntaxKind, SyntaxNode,
};

#[rustfmt::skip]
ast_assoc!(
    BindingListAst, [
        // Bindings live in the local scope.
        Let, WithOpen, Each, For, Icollect, Collect, Accumulate,
        // Parameters live in the local scope.
        // Function name is extended to the outer scope.
        FuncAst, Macros, Macro,
        // Bindings are extended to the outer scope.
        Var, Local, Global, ImportMacros,
    ]
);

pub(crate) trait Binding:
    rowan::ast::AstNode<Language = crate::FennelLanguage>
{
    fn bindings(&self) -> Option<Vec<models::LSymbol>>;

    fn target_node(&self, target: SyntaxKind) -> Option<SyntaxNode> {
        self.syntax().children().find(|n| n.kind() == target)
    }

    fn target_nodes(
        &self,
        target: SyntaxKind,
    ) -> Box<dyn Iterator<Item = SyntaxNode>> {
        Box::new(self.syntax().children().filter(move |n| n.kind() == target))
    }

    fn target_child_node(
        &self,
        parent: SyntaxKind,
        target: SyntaxKind,
    ) -> Option<SyntaxNode> {
        self.syntax()
            .children()
            .find(|n| n.kind() == parent)?
            .children()
            .find(|n| n.kind() == target)
    }

    fn target_child_nodes(
        &self,
        parent: SyntaxKind,
        target: SyntaxKind,
    ) -> Option<Vec<SyntaxNode>> {
        let nodes = self
            .syntax()
            .children()
            .find(|n| n.kind() == parent)?
            .children()
            .filter(|n| n.kind() == target)
            .collect();
        Some(nodes)
    }

    fn _bindings(
        &self,
        node: SyntaxNode,
        scope_kind: models::ScopeKind,
        scope_extend: ScopeExtend,
        // override value eval
        override_value_kind: Option<models::ValueKind>,
        // fallback if destructuring occurs
        // TOOD: support destructuring eval
        fallback_value_kind: models::ValueKind,
    ) -> Option<Vec<models::LSymbol>> {
        let scope_range = scope_extend.range(&node);

        // Guess first child is a symbol
        if let Some(symbol) = Symbol::cast(node.first_token()?.parent()?) {
            let (id, ..) = symbol.id()?;
            // NOTE: should not pretend symbol pairs with value
            let value_node = node.last_child();
            let value = if let Some(override_value_kind) = override_value_kind
            {
                models::Value {
                    kind: override_value_kind,
                    range: value_node.map(|n| n.text_range()),
                }
            } else {
                let value_node = value_node?;
                let range = value_node.text_range();
                models::Value {
                    kind: eval::EvalAst::cast(value_node)?.eval_kind(),
                    range: Some(range),
                }
            };
            return Some(vec![models::LSymbol {
                token: id,
                scope: models::Scope { kind: scope_kind, range: scope_range },
                value,
            }]);
        }

        // else do destructuring
        // TODO: support destructuring eval
        let value_kind = fallback_value_kind;

        let tokens = node
            .first_child()?
            .descendants()
            .filter_map(Symbol::cast)
            .filter_map(|n| n.id())
            .map(|(id, ..)| id);

        let symbols = tokens
            .map(|t| models::LSymbol {
                token: t,
                scope: models::Scope { kind: scope_kind, range: scope_range },
                value: models::Value { kind: value_kind, range: None },
            })
            .collect();
        Some(symbols)
    }
}

impl Binding for Var {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        self._bindings(
            self.target_node(SyntaxKind::N_ASSIGN_PAIR)?,
            models::ScopeKind::Var,
            ScopeExtend::Current,
            // TODO: support set
            Some(models::ValueKind::Unknown),
            models::ValueKind::Unknown,
        )
    }
}

impl Binding for Local {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        self._bindings(
            self.target_node(SyntaxKind::N_ASSIGN_PAIR)?,
            models::ScopeKind::Local,
            ScopeExtend::Current,
            None,
            models::ValueKind::Unknown,
        )
    }
}

impl Binding for Global {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        self._bindings(
            self.target_node(SyntaxKind::N_ASSIGN_PAIR)?,
            models::ScopeKind::Global,
            ScopeExtend::Outer,
            None,
            models::ValueKind::Unknown,
        )
    }
}

impl Binding for Let {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let nodes = self.target_child_nodes(
            SyntaxKind::N_ASSIGN_TABLE,
            SyntaxKind::N_ASSIGN_PAIR,
        )?;
        let symbols = nodes
            .into_iter()
            .filter_map(|n| {
                self._bindings(
                    n,
                    models::ScopeKind::Let,
                    ScopeExtend::Current,
                    None,
                    models::ValueKind::Unknown,
                )
            })
            .flatten()
            .collect();
        Some(symbols)
    }
}

impl Binding for ImportMacros {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let nodes = self.target_nodes(SyntaxKind::N_IMPORT_MACROS_PAIR);
        let symbols = nodes
            .filter_map(|n| {
                self._bindings(
                    n,
                    models::ScopeKind::Macro,
                    ScopeExtend::Outer,
                    // TODO: more clear type
                    Some(models::ValueKind::Module),
                    models::ValueKind::Macro,
                )
            })
            .flatten()
            .collect();
        Some(symbols)
    }
}

impl Binding for WithOpen {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let nodes = self.target_child_nodes(
            SyntaxKind::N_NV_PAIR_TABLE,
            SyntaxKind::N_NV_PAIR,
        )?;
        let symbols = nodes
            .into_iter()
            .filter_map(|n| {
                self._bindings(
                    n,
                    models::ScopeKind::WithOpen,
                    ScopeExtend::Current,
                    Some(models::ValueKind::FileHandle),
                    models::ValueKind::Unknown,
                )
            })
            .flatten()
            .collect();
        Some(symbols)
    }
}

impl Binding for Each {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let nodes = self.target_child_nodes(
            SyntaxKind::N_EACH_TABLE,
            SyntaxKind::N_ITERATION,
        )?;
        let symbols = nodes
            .into_iter()
            .filter_map(|n| {
                self._bindings(
                    n,
                    models::ScopeKind::IterValue,
                    ScopeExtend::Current,
                    None,
                    models::ValueKind::Unknown,
                )
            })
            .flatten()
            .collect();
        Some(symbols)
    }
}

impl Binding for For {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        self._bindings(
            self.target_child_node(
                SyntaxKind::N_FOR_TABLE,
                SyntaxKind::N_ITERATION_VALUE,
            )?,
            models::ScopeKind::IterValue,
            ScopeExtend::Current,
            Some(models::ValueKind::Number),
            models::ValueKind::Unknown,
        )
    }
}

impl Binding for Icollect {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let nodes = self.target_child_nodes(
            SyntaxKind::N_ICOLLECT_TABLE,
            SyntaxKind::N_ITERATION,
        )?;
        let symbols = nodes
            .into_iter()
            .filter_map(|n| {
                self._bindings(
                    n,
                    models::ScopeKind::IterValue,
                    ScopeExtend::Current,
                    None,
                    models::ValueKind::Unknown,
                )
            })
            .flatten()
            .collect();
        Some(symbols)
    }
}

impl Binding for Collect {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let nodes = self.target_child_nodes(
            SyntaxKind::N_COLLECT_TABLE,
            SyntaxKind::N_ITERATION,
        )?;
        let symbols = nodes
            .into_iter()
            .filter_map(|n| {
                self._bindings(
                    n,
                    models::ScopeKind::IterValue,
                    ScopeExtend::Current,
                    None,
                    models::ValueKind::Unknown,
                )
            })
            .flatten()
            .collect();
        Some(symbols)
    }
}

impl Binding for Accumulate {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let mut symbols = self._bindings(
            self.target_child_node(
                SyntaxKind::N_ACCUMULATE_TABLE,
                SyntaxKind::N_ACCUMULATOR,
            )?,
            models::ScopeKind::AccuValue,
            ScopeExtend::Current,
            Some(models::ValueKind::Unknown),
            models::ValueKind::Unknown,
        )?;

        let iter_nodes = self.target_child_nodes(
            SyntaxKind::N_ACCUMULATE_TABLE,
            SyntaxKind::N_ITERATION,
        )?;
        let iters = iter_nodes.into_iter().filter_map(|n| {
            self._bindings(
                n,
                models::ScopeKind::IterValue,
                ScopeExtend::Current,
                None,
                models::ValueKind::Unknown,
            )
        });

        symbols.extend(iters.flatten());
        Some(symbols)
    }
}

impl Binding for FuncAst {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let name = self.name().and_then(|(name_node, is_pure)| {
            if !is_pure {
                return None;
            }
            Some(models::LSymbol {
                token: models::Token {
                    text: name_node.first_token().unwrap().to_string(),
                    range: name_node.text_range(),
                },
                scope: models::Scope {
                    kind: match self {
                        Self::Func(_) => models::ScopeKind::Func,
                        Self::Lambda(_) => models::ScopeKind::Lambda,
                    },
                    range: ScopeExtend::Outer.range(&name_node),
                },
                value: models::Value {
                    kind: models::ValueKind::Func,
                    range: Some(self.syntax().text_range()),
                },
            })
        });

        let param_nodes = self
            .target_child_nodes(
                SyntaxKind::N_PARAM_TABLE,
                SyntaxKind::N_ASSIGN_PATTERN,
            )
            .unwrap_or_default();
        let params = param_nodes
            .into_iter()
            .filter_map(|n| {
                self._bindings(
                    n,
                    models::ScopeKind::Param,
                    ScopeExtend::Current,
                    Some(models::ValueKind::Param),
                    models::ValueKind::Param,
                )
            })
            .flatten();

        let varargs = self.varargs().map(|n| models::LSymbol {
            token: models::Token {
                text: "...".to_owned(),
                range: n.text_range(),
            },
            scope: models::Scope {
                kind: models::ScopeKind::Param,
                range: self.syntax().text_range(),
            },
            value: models::Value {
                kind: models::ValueKind::Param,
                range: None,
            },
        });

        let mut symbols: Vec<models::LSymbol> = params.collect();
        if let Some(s) = name {
            symbols.push(s)
        }
        if let Some(s) = varargs {
            symbols.push(s)
        }
        Some(symbols)
    }
}

impl Binding for Macro {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let name_node = self
            .syntax()
            .children()
            .find(|n| n.kind() == SyntaxKind::N_MACRO_NAME);

        let name = name_node.map(|n| models::LSymbol {
            token: models::Token {
                text: n.text().to_string(),
                range: n.text_range(),
            },
            scope: models::Scope {
                kind: models::ScopeKind::Macro,
                range: ScopeExtend::Outer.range(self.syntax()),
            },
            value: models::Value {
                kind: models::ValueKind::Macro,
                range: None,
            },
        });

        let param_nodes = self
            .target_child_nodes(
                SyntaxKind::N_PARAM_TABLE,
                SyntaxKind::N_ASSIGN_PATTERN,
            )
            .unwrap_or_default();
        let params = param_nodes
            .into_iter()
            .filter_map(|n| {
                self._bindings(
                    n,
                    models::ScopeKind::MacroParam,
                    ScopeExtend::Current,
                    Some(models::ValueKind::MacroParam),
                    models::ValueKind::MacroParam,
                )
            })
            .flatten();

        let mut symbols: Vec<models::LSymbol> = params.collect();
        if let Some(name) = name {
            symbols.push(name)
        }
        Some(symbols)
    }
}

impl Binding for Macros {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        let table = self.syntax().first_child().and_then(KvTable::cast)?;

        let symbols = table
            .iter()
            .filter_map(|(k, v)| {
                let key = k?;
                let text = key.cast_string()?;
                Some(models::LSymbol {
                    token: models::Token {
                        text,
                        range: key.syntax().text_range(),
                    },
                    scope: models::Scope {
                        kind: models::ScopeKind::Macro,
                        range: ScopeExtend::Outer.range(self.syntax()),
                    },
                    value: models::Value {
                        kind: models::ValueKind::Macro,
                        range: v.map(|v| v.syntax().text_range()),
                    },
                })
            })
            .collect();

        Some(symbols)
    }
}

impl Binding for BindingListAst {
    fn bindings(&self) -> Option<Vec<models::LSymbol>> {
        match self {
            Self::FuncAst(n) => n.bindings(),
            Self::Let(n) => n.bindings(),
            Self::Var(n) => n.bindings(),
            Self::Local(n) => n.bindings(),
            Self::Global(n) => n.bindings(),
            Self::ImportMacros(n) => n.bindings(),
            Self::WithOpen(n) => n.bindings(),
            Self::Each(n) => n.bindings(),
            Self::For(n) => n.bindings(),
            Self::Icollect(n) => n.bindings(),
            Self::Collect(n) => n.bindings(),
            Self::Accumulate(n) => n.bindings(),
            Self::Macro(n) => n.bindings(),
            Self::Macros(n) => n.bindings(),
        }
    }
}

ast_assoc!(MatchAst, [Match, Catch, MatchTry]);

impl Match {
    fn l_or_r_symbols(
        &self,
        l_symbols: &mut models::LSymbols,
    ) -> Vec<models::RSymbol> {
        let mut r_symbols = vec![];
        for (p, ns) in self
            .syntax()
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_MATCH_CLAUSE)
            .filter_map(|n| {
                let c = n.first_child(); // N_MATCH_PATTERN_TOP
                c.map(|c| (n, c))
            })
            .map(|(p, n)| (p, n.descendants()))
            .map(|(p, ns)| (p, ns.filter(|n| n.kind() != SyntaxKind::N_COND)))
            .map(|(p, ns)| (p, ns.filter_map(LeftOrRightSymbol::cast)))
        {
            for n in ns {
                let node = n.syntax();
                let token = Symbol::cast(
                    node.first_token().unwrap().parent().unwrap(),
                )
                .and_then(|n| n.id());
                if token.is_none() {
                    continue;
                }
                let token = token.unwrap().0;
                if l_symbols.nearest(&token.clone()).is_none() {
                    let scope_range = ScopeExtend::This(p.clone()).range(node);
                    l_symbols.0.insert(
                        token.range.start().into(),
                        models::LSymbol {
                            token,
                            scope: models::Scope {
                                kind: models::ScopeKind::Match,
                                range: scope_range,
                            },
                            value: models::Value {
                                kind: models::ValueKind::Match,
                                range: None,
                            },
                        },
                    );
                } else {
                    r_symbols.push(models::RSymbol {
                        special: models::SpecialKind::Normal,
                        token,
                    });
                }
            }
        }
        r_symbols
    }
}

impl Catch {
    fn l_or_r_symbols(
        &self,
        l_symbols: &mut models::LSymbols,
    ) -> Vec<models::RSymbol> {
        let mut r_symbols = vec![];
        for (p, ns) in self
            .syntax()
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_MATCH_CLAUSE)
            .filter_map(|n| {
                let c = n.first_child(); // N_MATCH_PATTERN_TOP
                c.map(|c| (n, c))
            })
            .map(|(p, n)| (p, n.descendants()))
            .map(|(p, ns)| (p, ns.filter(|n| n.kind() != SyntaxKind::N_COND)))
            .map(|(p, ns)| (p, ns.filter_map(LeftOrRightSymbol::cast)))
        {
            for n in ns {
                let node = n.syntax();
                let token = Symbol::cast(
                    node.first_token().unwrap().parent().unwrap(),
                )
                .and_then(|n| n.id());
                if token.is_none() {
                    continue;
                }
                let token = token.unwrap().0;
                if l_symbols.nearest(&token.clone()).is_none() {
                    let scope_range = ScopeExtend::This(p.clone()).range(node);
                    l_symbols.0.insert(
                        token.range.start().into(),
                        models::LSymbol {
                            token,
                            scope: models::Scope {
                                kind: models::ScopeKind::Catch,
                                range: scope_range,
                            },
                            value: models::Value {
                                kind: models::ValueKind::Match,
                                range: None,
                            },
                        },
                    );
                } else {
                    r_symbols.push(models::RSymbol {
                        special: models::SpecialKind::Normal,
                        token,
                    });
                }
            }
        }
        r_symbols
    }
}

impl MatchTry {
    fn l_or_r_symbols(
        &self,
        l_symbols: &mut models::LSymbols,
    ) -> Vec<models::RSymbol> {
        let mut r_symbols = vec![];
        for (_p, ns) in self
            .syntax()
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_MATCH_TRY_CLAUSE)
            .filter_map(|n| {
                let c = n.first_child();
                if let Some(c) = c {
                    if c.kind() == SyntaxKind::N_CATCH_LIST {
                        None
                    } else {
                        Some((n, c))
                    }
                } else {
                    None
                }
            })
            .map(|(p, n)| (p, n.descendants()))
            .map(|(p, ns)| (p, ns.filter(|n| n.kind() != SyntaxKind::N_COND)))
            .map(|(p, ns)| (p, ns.filter_map(LeftOrRightSymbol::cast)))
        {
            for n in ns {
                let node = n.syntax();
                let token = Symbol::cast(
                    node.first_token().unwrap().parent().unwrap(),
                )
                .and_then(|n| n.id());
                if token.is_none() {
                    continue;
                }
                let token = token.unwrap().0;
                if l_symbols.nearest(&token.clone()).is_none() {
                    let scope_range = ScopeExtend::Current.range(node);
                    l_symbols.0.insert(
                        token.range.start().into(),
                        models::LSymbol {
                            token,
                            scope: models::Scope {
                                kind: models::ScopeKind::MatchTry,
                                range: scope_range,
                            },
                            value: models::Value {
                                kind: models::ValueKind::Match,
                                range: None,
                            },
                        },
                    );
                } else {
                    r_symbols.push(models::RSymbol {
                        special: models::SpecialKind::Normal,
                        token,
                    });
                }
            }
        }
        r_symbols
    }
}

impl MatchAst {
    pub(crate) fn l_or_r_symbols(
        &self,
        l_symbols: &mut models::LSymbols,
    ) -> Vec<models::RSymbol> {
        match self {
            Self::Match(n) => n.l_or_r_symbols(l_symbols),
            Self::Catch(n) => n.l_or_r_symbols(l_symbols),
            Self::MatchTry(n) => n.l_or_r_symbols(l_symbols),
        }
    }
}

#[rustfmt::skip]
ast_assoc!(
    ScopeAst, [
        // Root always exists.
        Root,
        Let, WithOpen, Each, For, Icollect, Collect, Accumulate,
        Func, Lambda,
        ImportMacros,
        Match, MatchTry, Catch,
        Macro, Macros,
    ]
);

impl ScopeAst {
    fn end(&self) -> rowan::TextSize {
        match self {
            Self::MatchTry(n) => n.end(),
            _ => self.syntax().text_range().end(),
        }
    }
}

impl MatchTry {
    fn end(&self) -> rowan::TextSize {
        let clause = self
            .syntax()
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_MATCH_TRY_CLAUSE);
        if let Some(last_clause) = clause.last() {
            if last_clause.first_child().unwrap().kind()
                == SyntaxKind::N_CATCH_LIST
            {
                return last_clause.text_range().start();
            }
        }
        self.syntax().text_range().end()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ScopeExtend {
    Current,
    This(SyntaxNode),
    Outer,
    #[allow(unused)]
    File, // useless?
}

impl ScopeExtend {
    fn range(&self, node: &SyntaxNode) -> TextRange {
        let scope_end = match self {
            Self::Current => {
                node.ancestors().find_map(ScopeAst::cast).unwrap().end()
            }
            Self::This(node) => node.text_range().end(),
            Self::Outer => {
                let mut scopes = node.ancestors().filter_map(ScopeAst::cast);
                let first_scope = scopes.next().unwrap(); // safe
                if let ScopeAst::Root(n) = first_scope {
                    n.syntax().text_range().end()
                } else {
                    scopes.next().unwrap().syntax().text_range().end()
                }
            }
            Self::File => {
                node.ancestors().last().and_then(ScopeAst::cast).unwrap().end()
            }
        };
        let start = node.text_range().end();
        TextRange::new(start, scope_end)
    }
}
