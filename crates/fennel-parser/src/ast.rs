#[macro_use]
mod macros;
mod bind;
mod error;
mod eval;
mod func;
pub mod models;
mod nodes;

use std::collections::HashSet;

use rowan::{ast::AstNode, GreenNode, TextRange, TextSize};

use self::{
    bind::{Binding, BindingListAst},
    nodes::Root,
};
use crate::{
    syntax::validata_symbol, Error, ErrorKind::*, SyntaxKind, SyntaxNode,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
    pub root: rowan::GreenNode,

    pub lua_globals: HashSet<&'static str>,
    pub lua_modules: HashSet<&'static str>,
    pub globals: HashSet<String>,
    l_symbols: models::LSymbols,
    r_symbols: models::RSymbols,

    pub parser_errors: Vec<Error>,
    pub globals_errors: Vec<Error>,
    pub other_errors: Vec<Error>,
}

impl Ast {
    pub fn new(
        green_node: GreenNode,
        parser_errors: Vec<Error>,
        mut user_globals: HashSet<String>,
    ) -> Self {
        let root =
            Root::cast(SyntaxNode::new_root(green_node.clone())).unwrap();

        let lua_globals: HashSet<&'static str> =
            HashSet::from(include!("static/globals"));
        let lua_modules: HashSet<&'static str> =
            HashSet::from(include!("static/globals-module"));
        lua_globals.iter().for_each(|v| {
            user_globals.insert(v.to_string());
        });

        let other_errors = root
            .provide_errors()
            .chain(root.delimiter_whitespace_errors())
            .collect();

        let mut ast = Ast {
            root: green_node,
            l_symbols: models::LSymbols::default(),
            r_symbols: vec![],
            parser_errors,
            globals_errors: vec![],
            other_errors,
            lua_globals,
            lua_modules,
            globals: user_globals,
        };

        ast.update_symbols();
        ast.update_symbols_follow();
        ast.update_definition_errors();
        ast
    }

    pub fn update_globals(&mut self, globals: Vec<String>) {
        self.globals = HashSet::from_iter(globals);
        self.lua_globals.iter().for_each(|v| {
            self.globals.insert(v.to_string());
        });
        self.update_definition_errors();
    }

    pub fn errors(&self) -> impl Iterator<Item = &Error> {
        self.parser_errors
            .iter()
            .chain(self.globals_errors.iter())
            .chain(self.other_errors.iter())
    }

    pub fn definition(&self, offset: u32) -> Option<(models::LSymbol, bool)> {
        let root = SyntaxNode::new_root(self.root.clone());

        let mut token =
            root.token_at_offset(TextSize::from(offset)).right_biased()?;
        if token.kind() == SyntaxKind::COMMA {
            token = token.next_token()?;
        }
        let token_start = token.text_range().start().into();

        if let Some(symbol) = self.l_symbol(token_start) {
            return Some((symbol.clone(), true));
        }

        let r_symbol = self.r_symbol(token_start)?;
        self.l_symbol_by_r(r_symbol).found().map(|s| (s.clone(), false))
    }

    pub fn reference(&self, offset: u32) -> Option<Vec<TextRange>> {
        let root = SyntaxNode::new_root(self.root.clone());

        let token =
            root.token_at_offset(TextSize::from(offset)).right_biased()?;

        let l_symbol = self
            .l_symbol(token.text_range().start().into())
            .cloned()
            .or_else(|| {
                let r_symbol = self.r_symbol(offset)?;
                let l_symbol = self.l_symbol_by_r(r_symbol);
                l_symbol.found().cloned()
            })?;

        let mut ranges = vec![l_symbol.token.range];
        ranges.extend(
            self.r_symbols
                .iter()
                .filter(|s| l_symbol.contains_token(&s.token))
                .map(|s| s.token.range),
        );

        Some(ranges)
    }

    pub fn completion(
        &self,
        offset: u32,
        trigger: Option<String>,
    ) -> (LSymbols, Globals) {
        let root = SyntaxNode::new_root(self.root.clone());
        let token =
            root.token_at_offset(TextSize::from(offset)).right_biased();
        if token.is_none() {
            return (Box::new(vec![].into_iter()), vec![]);
        }
        let token = token.unwrap();

        if trigger.is_some() {
            let token = token
                .prev_sibling_or_token()
                .and_then(|n| n.as_node().cloned())
                .filter(|n| nodes::RightSymbol::can_cast(n.kind()))
                .map(|t| t.text().to_string());

            let res = match token.as_deref() {
                Some("coroutine.") => {
                    Vec::from(include!("static/modules/coroutine"))
                }
                Some("debug.") => Vec::from(include!("static/modules/debug")),
                Some("file:") => Vec::from(include!("static/modules/file")),
                Some("io.") => Vec::from(include!("static/modules/io")),
                Some("math.") => Vec::from(include!("static/modules/math")),
                Some("os.") => Vec::from(include!("static/modules/os")),
                Some("package.") => {
                    Vec::from(include!("static/modules/package"))
                }
                Some("string.") => {
                    Vec::from(include!("static/modules/string"))
                }
                Some("table.") => Vec::from(include!("static/modules/table")),
                _ => vec![],
            };
            return (Box::new(::std::iter::empty()), vec![(
                models::CompletionKind::Field,
                res,
            )]);
        }

        let call_position = || -> Option<bool> {
            if token.kind() == SyntaxKind::L_PAREN {
                return Some(false);
            }
            let under_list = || -> bool {
                for p in token.parent_ancestors() {
                    let kind = p.kind();
                    if nodes::List::can_cast(kind) {
                        return true;
                    }
                    if nodes::Sexp::can_cast(kind) {
                        return false;
                    }
                }
                false
            }();
            if !under_list {
                return Some(false);
            }

            let mut prevs =
                token.siblings_with_tokens(rowan::Direction::Prev).peekable();
            if prevs.peek().unwrap().kind() == SyntaxKind::R_PAREN {
                prevs.next();
            }
            let peek = prevs.peek().unwrap();
            if peek.kind() == SyntaxKind::N_SUBLIST
                && peek
                    .as_node()
                    .unwrap()
                    .first_child()
                    .map(|n| n.children().count() == 1)
                    .unwrap_or(false)
            {
                prevs.next();
            }
            let res = prevs.all(|t| {
                [
                    SyntaxKind::N_R_SYMBOL,
                    SyntaxKind::WHITESPACE,
                    SyntaxKind::COMMENT,
                    SyntaxKind::ERROR,
                    SyntaxKind::L_PAREN,
                ]
                .contains(&t.kind())
            });
            Some(res)
        }();

        let mut globals = vec![(
            models::CompletionKind::Module,
            Vec::from(include!("static/globals-module")),
        )];
        if call_position.unwrap_or(false) {
            globals.push((
                models::CompletionKind::Func,
                Vec::from(include!("static/globals-func")),
            ));
            globals.push((
                models::CompletionKind::Keyword,
                Vec::from(include!("static/keywords")),
            ));
            globals.push((
                models::CompletionKind::Operator,
                Vec::from(include!("static/operator")),
            ));
            if token.parent_ancestors().any(|n| {
                [
                    SyntaxKind::N_MACRO,
                    SyntaxKind::N_MACROS,
                    SyntaxKind::N_EVAL_COMPILER,
                    SyntaxKind::N_IMPORT_MACROS,
                ]
                .contains(&n.kind())
            }) {
                globals.push((
                    models::CompletionKind::Keyword,
                    Vec::from(include!("static/compiler-macro")),
                ))
            }
        } else {
            globals.push((
                models::CompletionKind::Keyword,
                Vec::from(include!("static/literals")),
            ));
            globals.push((
                models::CompletionKind::Var,
                Vec::from(include!("static/globals-var")),
            ));
        }
        (Box::new(self.l_symbols.range(offset)), globals)
    }

    pub fn literal_value(&self, value: models::Value) -> Option<String> {
        if !matches!(
            value.kind,
            models::ValueKind::Bool
                | models::ValueKind::Number
                | models::ValueKind::String
        ) {
            return None;
        }
        let root = SyntaxNode::new_root(self.root.clone());
        let value_range = value.range?;
        let token =
            root.token_at_offset(value_range.start()).right_biased()?;
        if token.text_range() == value_range {
            Some(token.text().to_owned())
        } else {
            None
        }
    }

    pub fn docstring(&self, range: TextRange) -> Option<String> {
        let root = SyntaxNode::new_root(self.root.clone());

        let token = root.token_at_offset(range.start()).right_biased()?;
        func::from(token)?.docstring()
    }

    pub fn validate_name(&self, name: &str) -> bool {
        validata_symbol(name)
    }

    fn update_symbols(&mut self) {
        let root = SyntaxNode::new_root(self.root.clone());
        let mut l_symbols = models::LSymbols::new(
            root.descendants()
                .filter_map(BindingListAst::cast)
                .filter_map(|n| n.bindings())
                .flatten(),
        );
        let mut r_symbols =
            nodes::Root::cast(root.clone()).unwrap().r_symbols();

        let new_r_symbols =
            Root::cast(root).unwrap().correct_symbols(&mut l_symbols);
        r_symbols.extend(new_r_symbols);
        self.l_symbols = l_symbols;
        self.r_symbols = r_symbols;
    }

    fn update_symbols_follow(&mut self) {
        let root = SyntaxNode::new_root(self.root.clone());

        let follows: Vec<(u32, TextRange)> = self
            .l_symbols
            .0
            .iter()
            .filter_map(|(key, l_symbol)| {
                if l_symbol.value.kind == models::ValueKind::Symbol {
                    l_symbol.value.range.map(|range| (*key, range))
                } else {
                    None
                }
            })
            .collect();

        follows.iter().for_each(|(key, r_symbol_range)| {
            let token = root
                .token_at_offset(r_symbol_range.start())
                .right_biased()
                .unwrap();
            let text = token.text().to_owned();
            let ref_l_symbol = self
                .l_symbols
                .nearest(&models::Token {
                    text: text.clone(),
                    range: *r_symbol_range,
                })
                .cloned();
            let v = self.l_symbols.0.get_mut(key).unwrap();
            if let Some(l_symbol) = ref_l_symbol {
                v.value = l_symbol.value;
            } else if self.lua_modules.contains(text.as_str()) {
                v.value.kind = models::ValueKind::Module;
            }
        })
    }

    fn update_definition_errors(&mut self) {
        self.globals_errors = self
            .r_symbols
            .iter()
            .filter(|r_symbol| {
                self.l_symbol_by_r(r_symbol) == FindLSymbol::NotFound
            })
            .filter_map(|r_symbol| {
                let text = r_symbol.token.text.as_str();
                if self.globals.contains(text)
                    || Vec::from(include!("static/compiler-macro"))
                        .contains(&text)
                {
                    None
                } else {
                    Some(Error::new(r_symbol.token.range, Undefined))
                }
            })
            .collect()
    }

    fn l_symbol(&self, offset: u32) -> Option<&models::LSymbol> {
        self.l_symbols.get(offset)
    }

    fn l_symbol_by_r(&self, r_symbol: &models::RSymbol) -> FindLSymbol {
        match r_symbol.special {
            models::SpecialKind::Normal
            | models::SpecialKind::MacroUnquote => self
                .l_symbols
                .nearest(&r_symbol.token)
                .map_or(FindLSymbol::NotFound, FindLSymbol::Found),
            models::SpecialKind::HashArg => FindLSymbol::Skip,
            models::SpecialKind::MacroWrap => self
                .l_symbols
                .nearest(&r_symbol.token)
                .map_or(FindLSymbol::Skip, FindLSymbol::Found),
        }
    }

    fn r_symbol(&self, offset: u32) -> Option<&models::RSymbol> {
        self.r_symbols
            .iter()
            .find(|s| s.token.range.contains_inclusive(TextSize::from(offset)))
    }
}

type LSymbols<'a> = Box<dyn Iterator<Item = &'a models::LSymbol> + 'a>;
type Globals = Vec<(models::CompletionKind, Vec<&'static str>)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum FindLSymbol<'a> {
    NotFound,
    Found(&'a models::LSymbol),
    Skip,
}

impl FindLSymbol<'_> {
    // I don't care about skip!
    fn found(&self) -> Option<&models::LSymbol> {
        if let Self::Found(symbol) = self {
            Some(symbol)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{models::LSymbol, parse};

    #[test]
    fn l_symbols() {
        let text = "(var x 1) (var [y z] [2 3])";
        let ast = parse(text, HashSet::new());
        assert!(ast.errors().next().is_none());
        let res = [
            models::LSymbol {
                token: models::Token {
                    text: "z".to_owned(),
                    range: TextRange::new(18.into(), 19.into()),
                },
                scope: models::Scope {
                    kind: models::ScopeKind::Var,
                    range: TextRange::new(15.into(), 27.into()),
                },
                value: models::Value {
                    kind: models::ValueKind::Unknown,
                    range: None,
                },
            },
            models::LSymbol {
                token: models::Token {
                    text: "y".to_owned(),
                    range: TextRange::new(16.into(), 17.into()),
                },
                scope: models::Scope {
                    kind: models::ScopeKind::Var,
                    range: TextRange::new(15.into(), 27.into()),
                },
                value: models::Value {
                    kind: models::ValueKind::Unknown,
                    range: None,
                },
            },
            models::LSymbol {
                token: models::Token {
                    text: "x".to_owned(),
                    range: TextRange::new(5.into(), 6.into()),
                },
                scope: models::Scope {
                    kind: models::ScopeKind::Var,
                    range: TextRange::new(5.into(), 27.into()),
                },
                value: models::Value {
                    kind: models::ValueKind::Unknown,
                    range: Some(TextRange::new(7.into(), 8.into())),
                },
            },
        ];
        assert_eq!(ast.l_symbols, models::LSymbols::new(res.into_iter()));
    }

    #[test]
    fn l_symbols_macro() {
        let text = "(macro x [] (+))";
        let ast = parse(text, HashSet::new());
        assert_eq!(
            ast.errors().collect::<Vec<&Error>>(),
            Vec::<&Error>::new()
        );
        let res = [models::LSymbol {
            token: models::Token {
                text: "x".to_owned(),
                range: TextRange::new(7.into(), 8.into()),
            },
            scope: models::Scope {
                kind: models::ScopeKind::Macro,
                range: TextRange::new(1.into(), 16.into()),
            },
            value: models::Value {
                kind: models::ValueKind::Macro,
                range: None,
            },
        }];
        assert_eq!(ast.l_symbols, models::LSymbols::new(res.into_iter()));
    }

    #[test]
    fn l_symbols_match() {
        let text = "(local x 1) (match 1 x 3 y 4)";
        let ast = parse(text, HashSet::new());
        assert_eq!(
            ast.errors().collect::<Vec<&Error>>(),
            Vec::<&Error>::new()
        );
        let l_symbols = [
            models::LSymbol {
                token: models::Token {
                    text: "x".to_owned(),
                    range: TextRange::new(7.into(), 8.into()),
                },
                scope: models::Scope {
                    kind: models::ScopeKind::Local,
                    range: TextRange::new(7.into(), 29.into()),
                },
                value: models::Value {
                    kind: models::ValueKind::Number,
                    range: Some(TextRange::new(9.into(), 10.into())),
                },
            },
            models::LSymbol {
                token: models::Token {
                    text: "y".to_owned(),
                    range: TextRange::new(25.into(), 26.into()),
                },
                scope: models::Scope {
                    kind: models::ScopeKind::Match,
                    range: TextRange::new(25.into(), 28.into()),
                },
                value: models::Value {
                    kind: models::ValueKind::Match,
                    range: None,
                },
            },
        ];
        let r_symbols = [models::RSymbol {
            token: models::Token {
                text: "x".to_owned(),
                range: TextRange::new(21.into(), 22.into()),
            },
            special: models::SpecialKind::Normal,
        }];
        assert_eq!(
            ast.l_symbols,
            models::LSymbols::new(l_symbols.into_iter())
        );
        assert_eq!(ast.r_symbols, r_symbols,);
    }

    #[test]
    fn check_undefined() {
        let text = "(x) (fn x [] (+))";
        let mut ast = parse(text, HashSet::new());
        assert_eq!(ast.errors().collect::<Vec<&Error>>(), vec![&Error::new(
            TextRange::new(1.into(), 2.into()),
            Undefined
        )],);
        ast.update_globals(vec!["x".to_owned()]);
        assert_eq!(ast.errors().collect::<Vec<&Error>>(), Vec::<&Error>::new())
    }

    #[test]
    fn check_undefined_fn_name() {
        let text = "(fn x.a [] (+))(fn x [] (+))";
        let ast = parse(text, HashSet::new());
        assert_eq!(ast.errors().collect::<Vec<&Error>>(), vec![&Error::new(
            TextRange::new(4.into(), 5.into()),
            Undefined
        )],);
    }

    #[test]
    fn check_undefined_destruct() {
        let text = "(let [ {abc abc} {:name 3} ] (print abc))";
        let ast = parse(text, HashSet::new());
        assert_eq!(ast.errors().collect::<Vec<&Error>>(), vec![&Error::new(
            TextRange::new(8.into(), 11.into()),
            Undefined
        )],);
    }

    #[test]
    fn check_symbol_ok() {
        let text = "(var a {}) (a) (a.b) (a.b:c)";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            Vec::<&Error>::new()
        );
    }

    #[test]
    fn check_symbol_error() {
        let text = "(var a {}) (print a.b:c)";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            vec![&Error::new(
                TextRange::new(18.into(), 23.into()),
                MethodNotAllowed
            )]
        );

        let text = "(var a {}) (var a.b 1) (var a:b 1)";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            vec![
                &Error::new(
                    TextRange::new(16.into(), 19.into()),
                    FieldAndMethodNotAllowed,
                ),
                &Error::new(
                    TextRange::new(28.into(), 31.into()),
                    FieldAndMethodNotAllowed,
                ),
            ]
        );

        let text = "(var x {})(lambda x:a [])";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            vec![&Error::new(
                TextRange::new(18.into(), 21.into()),
                MethodNotAllowed,
            ),]
        );
    }

    #[test]
    fn check_whitespace_ok() {
        let text = "(+)(+)";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            Vec::<&Error>::new()
        );
    }

    #[test]
    fn check_whitespace_error() {
        let text = "(fn x[] (+))";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            vec![&Error::new(
                TextRange::new(5.into(), 6.into()),
                MissingWhitespace
            )],
        );
        let text = "(, 1)";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            vec![&Error::new(
                TextRange::new(1.into(), 2.into()),
                MacroWhitespace,
            )],
        );
    }

    #[test]
    fn check_empty_list_error() {
        let text = "( ) (1)";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            vec![
                &Error::new(
                    TextRange::new(5.into(), 6.into()),
                    Unexpected(SyntaxKind::INTEGER)
                ),
                &Error::new(TextRange::new(0.into(), 3.into()), EmptyList),
            ],
        );
    }

    #[test]
    fn check_literal_call() {
        let text = "((-)) ((var x 1)) (table) (local z :s) (z)";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            vec![
                &Error::new(
                    TextRange::new(1.into(), 4.into()),
                    LiteralCall(models::ValueKind::Number)
                ),
                &Error::new(
                    TextRange::new(7.into(), 16.into()),
                    LiteralCall(models::ValueKind::Nil)
                ),
                // TOOD
            ],
        );
    }

    #[ignore = "FIXME"]
    #[test]
    fn check_varargs() {
        let text = "... (fn a [] (print ...))";
        assert_eq!(
            parse(text, HashSet::new()).errors().collect::<Vec<&Error>>(),
            vec![&Error::new(
                TextRange::new(20.into(), 23.into()),
                UnexpectedVarargs,
            )]
        );
    }

    fn definition(text: &str, offset: u32) -> Option<LSymbol> {
        parse(text, HashSet::new()).definition(offset).map(|(s, _)| s)
    }

    #[test]
    fn definition_inner() {
        let text = "(let [xy 1] (var xy 2) (print xy))";
        assert_eq!(
            definition(text, 30).map(|s| s.token.range),
            Some(TextRange::new(17.into(), 19.into()))
        );
    }

    #[test]
    fn definition_outer() {
        let text = "(let [xy 1] (global z 2) (print xy)) (print z)";
        assert_eq!(
            definition(text, 44).map(|s| s.token.range),
            Some(TextRange::new(20.into(), 21.into()))
        );
    }

    #[test]
    fn definition_l_symbol() {
        let text = "(local xxx 1) (let [xxx 1] (+ xxx))";
        assert_eq!(
            definition(text, 21).map(|s| s.token.range),
            Some(TextRange::new(20.into(), 23.into()))
        );
    }

    #[test]
    fn definition_fn() {
        let text = "(fn x [] (+)) (x)";
        assert_eq!(
            definition(text, 15).map(|s| s.token.range),
            Some(TextRange::new(4.into(), 5.into()))
        );
        // skip x.a
        let text = "(local x {}) (fn x.a [] (+)) (x)";
        assert_eq!(
            definition(text, 30).map(|s| s.token.range),
            Some(TextRange::new(7.into(), 8.into()))
        );
    }

    #[ignore = "FIXME"]
    #[test]
    fn definition_nest() {
        let text = "(local z (local z 3)) (print z)";
        assert_eq!(
            definition(text, 29).map(|s| s.token.range),
            Some(TextRange::new(7.into(), 8.into()))
        );
    }

    // seems a fennel compiler error
    #[ignore]
    #[test]
    fn definition_undefined() {
        let text = "(local z (fn x [] (print z)))";
        assert_eq!(definition(text, 25).map(|s| s.token.range), None);
    }

    #[test]
    fn value() {
        let text = "(local y 1) (fn x [] (+)) (x y)";
        assert_eq!(
            definition(text, 27).map(|s| s.value),
            Some(models::Value {
                kind: models::ValueKind::Func,
                range: Some(TextRange::new(13.into(), 24.into()))
            }),
        );
        assert_eq!(
            definition(text, 29).map(|s| s.value),
            Some(models::Value {
                kind: models::ValueKind::Number,
                range: Some(TextRange::new(9.into(), 10.into()))
            }),
        );
    }

    #[test]
    fn reference() {
        let text = "(fn xyz [] (+)) (xyz) (xyz 1)";
        let ast = parse(text, HashSet::new());
        assert!(ast.errors().next().is_none());
        assert_eq!(
            ast.reference(18),
            Some(vec![
                TextRange::new(4.into(), 7.into()),
                TextRange::new(17.into(), 20.into()),
                TextRange::new(23.into(), 26.into()),
            ]),
        );
        let text = "(fn xyz [] (+)) (xyz) (xyz 1)";
        let ast = parse(text, HashSet::new());
        assert_eq!(ast.errors().next(), None);
        assert_eq!(
            ast.reference(5),
            Some(vec![
                TextRange::new(4.into(), 7.into()),
                TextRange::new(17.into(), 20.into()),
                TextRange::new(23.into(), 26.into()),
            ]),
        )
    }

    #[test]
    fn completion() {
        let text = "(local abc 2)(l)";
        let ast = parse(text, HashSet::new());

        let (mut symbols, reserved) = ast.completion(15, None);
        assert!(symbols.any(|symbol| symbol.token.text == "abc"));
        assert!(reserved
            .into_iter()
            .find_map(|(kind, words)| {
                if kind == models::CompletionKind::Keyword {
                    Some(words)
                } else {
                    None
                }
            })
            .unwrap()
            .contains(&"lambda"));
    }

    #[test]
    fn docstring() {
        let text = "(fn a [] :a-func (print))";
        assert_eq!(
            parse(text, HashSet::new())
                .docstring(TextRange::new(4.into(), 5.into())),
            Some("a-func".to_string()),
        );
        let text = "(fn a [] {:fnl/docstring :helloAgain} (+))";
        assert_eq!(
            parse(text, HashSet::new())
                .docstring(TextRange::new(4.into(), 5.into())),
            Some("helloAgain".to_string()),
        );
    }
}
