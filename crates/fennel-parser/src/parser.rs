#[macro_use]
mod bnf;
#[macro_use]
mod helper;
mod sets;

use std::collections::VecDeque;

use bnf::{Notation::*, Rule};
use helper::*;
use rowan::{GreenNode, GreenNodeBuilder};
use sets::TokenSet;

use crate::{
    errors::{Error, ErrorKind::*},
    lexer::{Lexer, Token},
    syntax::lists::*,
    SyntaxKind::{self, *},
};

pub struct Parser<'p> {
    rule_stack: Vec<Rule>,
    token_queue: VecDeque<Token>,

    lexer: Lexer<'p>,
    builder: GreenNodeBuilder<'p>,
    errors: Vec<Error>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parsed {
    pub green_node: GreenNode,
    pub errors: Vec<Error>,
}

impl<'p> Parser<'p> {
    pub fn new(source: impl Iterator<Item = char> + 'p) -> Self {
        Parser {
            rule_stack: vec![],
            token_queue: VecDeque::<Token>::new(),

            lexer: Lexer::new(Box::new(source)),
            builder: Default::default(),
            errors: Default::default(),
        }
    }

    fn lex_next(&mut self) -> Token {
        let span = self.lexer.next().unwrap();
        Token { kind: span.kind, text: span.text, range: span.range }
    }

    fn step(&mut self) {
        self.token_queue.pop_front();
        self.token_queue.is_empty().then(|| {
            let token = self.lex_next();
            self.token_queue.push_back(token)
        });
    }

    fn lookahead(&self) -> &Token {
        self.token_queue.front().unwrap()
    }

    fn peek(&mut self) -> Token {
        if let Some(token) = self
            .token_queue
            .iter()
            .skip(1)
            .find(|token| !OUTBAND.contains(&token.kind))
        {
            return token.to_owned();
        }
        loop {
            let token = self.lex_next();
            self.token_queue.push_back(token.clone());
            if !OUTBAND.contains(&token.kind) {
                return token;
            }
        }
    }

    fn eat(&mut self, token: &Token, pop: bool) {
        self.builder.token(token.kind.into(), &token.text);
        if pop {
            self.rule_stack.pop();
        }
        self.step();
    }

    fn cur_rule(&self) -> &Rule {
        self.rule_stack.last().unwrap()
    }

    fn skip(&mut self) {
        self.rule_stack.pop();
    }

    pub fn parse(mut self) -> Parsed {
        self.step();

        // Must expand ROOT manually.
        self.builder.start_node(ROOT.into());
        self.rule_stack = vec![
            Rule { expect: ROOT, notation: Close(None) },
            notation!((END)),
            notation!((N_SEXP * TokenSet::END)),
        ];

        while !self.rule_stack.is_empty() {
            let cur_rule = *self.cur_rule();
            let cur_token = self.lookahead().to_owned();
            self.parse_inner(cur_rule, cur_token);
        }

        let green_node = self.builder.finish();
        assert!(self.rule_stack.is_empty());

        Parsed { green_node, errors: self.errors }
    }

    fn parse_inner(&mut self, cur_rule: Rule, cur_token: Token) {
        // Must close node as soon as possible.
        // It ensures the last child of syntax node is
        // not a comment or whitespace.
        if let Close(_) = cur_rule.notation {
            self.builder.finish_node();
            self.rule_stack.pop();
            return;
        }

        // Eat whitespace or comment
        if cur_token.kind == WHITESPACE || cur_token.kind == COMMENT {
            self.eat(&cur_token, false);
            return;
        }

        // Terminals token matched
        if cur_rule == (Rule { notation: Once, expect: cur_token.kind }) {
            // Need some extra works to parse symbol
            if cur_token.kind == SYMBOL {
                self.parse_symbol(cur_token);
                self.rule_stack.pop();
                self.step();
            } else {
                self.eat(&cur_token, true);
            }
            return;
        }

        // Stop repeat and optional
        match cur_rule {
            Rule { notation: Repeat(ends) | Optional(ends), .. } => {
                if ends.contains(cur_token.kind) {
                    self.skip();
                    return;
                }
            }
            Rule { notation: RepeatPeek(ends, follows), .. } => {
                if ends.contains(cur_token.kind) {
                    self.skip();
                    return;
                }
                let peek = &self.peek();
                if follows.contains(peek.kind) {
                    self.skip();
                    return;
                }
            }
            _ => {} // Don't touch Repeat1 Repeat1AndPeek
        }

        expand_rules!(
            self, expand, (cur_rule, cur_token),

            { N_SEXP ::= (N_ATOM) },
            { N_SEXP ::= (N_LIST) },

            { N_ATOM ::= (N_R_SYMBOL) },
            { N_ATOM ::= (VARARG) },
            { N_ATOM ::= (N_LITERAL) },
            { N_ATOM ::= (N_SEQ_TABLE) },
            { N_ATOM ::= (N_KV_TABLE) },

            { N_LITERAL ::= (FLOAT) },
            { N_LITERAL ::= (INTEGER) },
            { N_LITERAL ::= (QUOTE_STRING) },
            { N_LITERAL ::= (COLON_STRING) },
            { N_LITERAL ::= (BOOL) },
            { N_LITERAL ::= (NIL) },

            { N_STRING_LITERAL ::= (QUOTE_STRING) },
            { N_STRING_LITERAL ::= (COLON_STRING) },

            { N_SEQ_TABLE ::= (L_BRACKET),
                              (N_SEXP * TokenSet::R_BRACKET),
                              (R_BRACKET) },

            { N_KV_TABLE ::= (L_BRACE),
                             (N_KV_PAIR * TokenSet::R_BRACE),
                             (R_BRACE) },

            { N_KV_PAIR ::= (N_KEY), (N_VALUE) },
            { N_KV_PAIR ::= (COLON), (N_L_R_SYMBOL) },

            { N_L_SYMBOL ::= (SYMBOL) },
            { N_L_SYMBOL ::= (COMMA),
                             (SYMBOL) },

            { N_R_SYMBOL ::= (SYMBOL) },
            { N_R_SYMBOL ::= (COMMA),
                             (SYMBOL) },

            { N_L_R_SYMBOL ::= (SYMBOL) },
            { N_L_R_SYMBOL ::= (COMMA),
                               (SYMBOL) },

            { N_L_OR_R_SYMBOL ::= (SYMBOL) },
            { N_L_OR_R_SYMBOL ::= (COMMA),
                                  (SYMBOL) },


            { N_KEY ::= (N_SEXP) },

            { N_VALUE ::= (N_SEXP) },

            { N_COND ::= (N_SEXP) },

            { N_NV_PAIR ::= (N_L_SYMBOL),
                            (N_VALUE) },

            { N_ARGS ::= (N_SEXP + TokenSet::R_PAREN) },

            { N_BODY ::= (N_SEXP + TokenSet::R_PAREN) },

            { N_ITERATOR ::= (N_R_SYMBOL) },
            { N_ITERATOR ::= (N_LIST) },

            { N_LIST ::= (L_PAREN),
                         (N_SUBLIST ? TokenSet::R_PAREN),
                         (R_PAREN) },
            { N_LIST ::= (N_MACRO_LIST) },

            { N_MACRO_LIST ::= (N_MACRO_HASH) },
            { N_MACRO_LIST ::= (N_MACRO_UNQUOTE) },
            { N_MACRO_LIST ::= (N_MACRO_QUOTE) },

            { N_SUBLIST ::= (N_LIST),
                            (N_ARGS ? TokenSet::R_PAREN) },
            { N_SUBLIST ::= (N_SYMBOL_CALL),
                            (N_ARGS ? TokenSet::R_PAREN) },
            { N_SUBLIST ::= (N_VAR) },
            { N_SUBLIST ::= (N_SET) },
            { N_SUBLIST ::= (N_TSET) },
            { N_SUBLIST ::= (N_LOCAL) },
            { N_SUBLIST ::= (N_VALUES) },
            { N_SUBLIST ::= (N_GLOBAL) },
            { N_SUBLIST ::= (N_IMPORT_MACROS) },
            { N_SUBLIST ::= (N_REQUIRE_MACROS) },
            { N_SUBLIST ::= (N_MACRO) },
            { N_SUBLIST ::= (N_MACROS) },
            { N_SUBLIST ::= (N_EVAL_COMPILER) },
            { N_SUBLIST ::= (N_FN) },
            { N_SUBLIST ::= (N_LAMBDA) },
            { N_SUBLIST ::= (N_PARTIAL) },
            { N_SUBLIST ::= (N_LET) },
            { N_SUBLIST ::= (N_MATCH) },
            { N_SUBLIST ::= (N_MATCH_TRY) },
            { N_SUBLIST ::= (N_WITH_OPEN) },
            { N_SUBLIST ::= (N_PICK_VALUE) },
            { N_SUBLIST ::= (N_IF) },
            { N_SUBLIST ::= (N_WHEN) },
            { N_SUBLIST ::= (N_EACH) },
            { N_SUBLIST ::= (N_FOR) },
            { N_SUBLIST ::= (N_COLLECT) },
            { N_SUBLIST ::= (N_ICOLLECT) },
            { N_SUBLIST ::= (N_ACCUMULATE) },
            { N_SUBLIST ::= (N_WHILE) },
            { N_SUBLIST ::= (N_DO) },
            { N_SUBLIST ::= (N_OPERATION) }, // Not contains # : or
            { N_SUBLIST ::= (N_THREAD) },
            { N_SUBLIST ::= (N_DOTO) },
            { N_SUBLIST ::= (N_INCLUDE) },
            { N_SUBLIST ::= (N_LUA) },
            { N_SUBLIST ::= (N_PICK_ARGS) },
            { N_SUBLIST ::= (N_MACRODEBUG) },

            { N_SYMBOL_CALL ::= (N_R_SYMBOL),
                                (N_ARGS ? TokenSet::R_PAREN) },

            { N_SET ::= (KEYWORD_SET),
                        (N_SET_PAIR) },

            { N_SET_PAIR ::= (N_SET_PATTERN_LIST),
                             (N_SEXP) },
            { N_SET_PAIR ::= (N_SET_PATTERN),
                             (N_SEXP) },

            { N_SET_PATTERN_LIST ::= (L_PAREN),
                                     (N_SET_PATTERN + TokenSet::R_PAREN),
                                     (R_PAREN) },

            { N_SET_PATTERN ::= (N_R_SYMBOL) },
            { N_SET_PATTERN ::= (CAPTURE) },
            { N_SET_PATTERN ::= (N_SET_PATTERN_TABLE) },
            { N_SET_PATTERN ::= (N_SET_PATTERN_KV_TABLE) },

            { N_SET_PATTERN_TABLE ::= (L_BRACKET),
                                      (N_SET_PATTERN * TokenSet::R_BRACKET),
                                      (R_BRACKET) },

            { N_SET_PATTERN_KV_TABLE ::= (L_BRACE),
                                         (N_SET_PATTERN_KV * TokenSet::R_BRACE),
                                         (R_BRACE) },

            { N_SET_PATTERN_KV ::= (N_SET_PATTERN_KEY),
                                   (N_SET_PATTERN) },
            { N_SET_PATTERN_KV ::= (COLON),
                                   (N_R_SYMBOL) },

            { N_SET_PATTERN_KEY ::= (N_R_SYMBOL) },
            { N_SET_PATTERN_KEY ::= (N_LITERAL) },
            { N_SET_PATTERN_KEY ::= (N_SET_PATTERN_KEY_NEST) },
            { N_SET_PATTERN_KEY ::= (CAPTURE) },

            { N_SET_PATTERN_KEY_NEST ::= (L_PAREN),
                                         (N_SET_PATTERN_KEY),
                                         (R_PAREN) },

            { N_VAR ::= (KEYWORD_VAR),
                        (N_ASSIGN_PAIR) },


            { N_LOCAL ::= (KEYWORD_LOCAL),
                          (N_ASSIGN_PAIR) },

            // FIXME: DOTO macro
            { N_TSET ::= (KEYWORD_TSET),
                         (N_SEXP),
                         (N_SEXP),
                         (N_SEXP * TokenSet::R_PAREN) },

            { N_WITH_OPEN ::= (KEYWORD_WITH_OPEN),
                              (N_NV_PAIR_TABLE),
                              (N_BODY ? TokenSet::R_PAREN) },

            { N_NV_PAIR_TABLE ::= (L_BRACKET),
                                  (N_NV_PAIR * TokenSet::R_BRACKET),
                                  (R_BRACKET) },

            { N_LET ::= (KEYWORD_LET),
                        (N_ASSIGN_TABLE),
                        (N_BODY) },

            { N_VALUES ::= (KEYWORD_VALUES),
                           (N_ARGS ? TokenSet::R_PAREN) },

            { N_GLOBAL ::= (KEYWORD_GLOBAL),
                           (N_ASSIGN_PAIR) },

            { N_IMPORT_MACROS ::= (KEYWORD_IMPORT_MACROS),
                                  (N_IMPORT_MACROS_PAIR + TokenSet::R_PAREN) },

            { N_IMPORT_MACROS_PAIR ::= (N_IMPORT_MACROS_DESTRUCT),
                                       (N_SEXP) }, // must eval to string_literal

            { N_IMPORT_MACROS_DESTRUCT ::= (N_IMPORT_MACROS_KV_TABLE) },
            { N_IMPORT_MACROS_DESTRUCT ::= (N_L_SYMBOL) },

            { N_IMPORT_MACROS_KV_TABLE ::= (L_BRACE),
                                           (N_IMPORT_MACROS_KV_PAIR + TokenSet::R_BRACE),
                                           (R_BRACE) },

            { N_IMPORT_MACROS_KV_PAIR ::= (COLON),
                                          (N_L_SYMBOL) },
            { N_IMPORT_MACROS_KV_PAIR ::= (N_STRING_LITERAL),
                                          (N_L_SYMBOL) },

            { N_REQUIRE_MACROS ::= (KEYWORD_REQUIRE_MACROS),
                                   (N_STRING_LITERAL) },

            { N_ASSIGN_TABLE ::= (L_BRACKET),
                                 (N_ASSIGN_PAIR * TokenSet::R_BRACKET),
                                 (R_BRACKET) },

            { N_ASSIGN_PAIR ::= (N_ASSIGN_PATTERN_LIST),
                                (N_SEXP) },
            { N_ASSIGN_PAIR ::= (N_ASSIGN_PATTERN),
                                (N_SEXP) },

            { N_ASSIGN_PATTERN_LIST ::= (L_PAREN),
                                        (N_ASSIGN_PATTERN * TokenSet::R_PAREN),
                                        (R_PAREN) },

            { N_ASSIGN_PATTERN ::= (N_L_SYMBOL) },
            { N_ASSIGN_PATTERN ::= (CAPTURE) },
            { N_ASSIGN_PATTERN ::= (N_ASSIGN_PATTERN_TABLE) },
            { N_ASSIGN_PATTERN ::= (N_ASSIGN_PATTERN_KV_TABLE) },

            { N_ASSIGN_PATTERN_TABLE ::= (L_BRACKET),
                                         (N_ASSIGN_PATTERN * TokenSet::R_BRACKET),
                                         (R_BRACKET) },

            { N_ASSIGN_PATTERN_KV_TABLE ::= (L_BRACE),
                                            (N_ASSIGN_PATTERN_KV * TokenSet::R_BRACE),
                                            (R_BRACE) },

            { N_ASSIGN_PATTERN_KV ::= (N_ASSIGN_PATTERN_KEY), // Only R_SYMBOL
                                      (N_ASSIGN_PATTERN) }, // L_SYMBOL occurs
            { N_ASSIGN_PATTERN_KV ::= (COLON),
                                      (N_L_SYMBOL) },

            { N_ASSIGN_PATTERN_KEY ::= (N_R_SYMBOL) },
            { N_ASSIGN_PATTERN_KEY ::= (N_LITERAL) },
            { N_ASSIGN_PATTERN_KEY ::= (N_ASSIGN_PATTERN_KEY_NEST) },
            { N_ASSIGN_PATTERN_KEY ::= (CAPTURE) },

            { N_ASSIGN_PATTERN_KEY_NEST ::= (L_PAREN),
                                            (N_ASSIGN_PATTERN_KEY),
                                            (R_PAREN) },

            { N_MACRO ::= (KEYWORD_MACRO),
                          (N_MACRO_NAME),
                          (N_PARAM_TABLE),
                          (N_BODY ? TokenSet::R_PAREN) },

            { N_MACRO_NAME ::= (SYMBOL) },

            { N_MACROS ::= (KEYWORD_MACROS),
                           (N_KV_TABLE) },

            { N_EVAL_COMPILER ::= (KEYWORD_EVAL_COMPILER),
                                  (N_SEXP) },

            { N_FN ::= (KEYWORD_FN),
                       (N_FN_NAME ? TokenSet::L_BRACKET),
                       (N_PARAM_TABLE),
                       (N_BODY ? TokenSet::R_PAREN) },
            { N_LAMBDA ::= (KEYWORD_LAMBDA),
                           (N_FN_NAME ? TokenSet::L_BRACKET),
                           (N_PARAM_TABLE),
                           (N_BODY ? TokenSet::R_PAREN) },
            { N_FN_NAME ::= (SYMBOL) },

            { N_PARAM_TABLE ::= (L_BRACKET),
                                (N_ASSIGN_PATTERN * TokenSet::VARARG_BRACKET),
                                (N_VARARG ? TokenSet::R_BRACKET),
                                (R_BRACKET) },

            { N_VARARG ::= (VARARG) },

            { N_MACRO_HASH ::= (HASHFN),
                               (N_SEXP) },

            { N_MACRO_UNQUOTE ::= (COMMA),
                                  (N_SEXP) },

            { N_MACRO_QUOTE ::= (BACKTICK),
                                (N_SEXP) },

            { N_PARTIAL ::= (KEYWORD_PARTIAL),
                            (N_CALL_FN),
                            (N_ARGS ? TokenSet::R_PAREN) },

            { N_PICK_VALUE ::= (KEYWORD_PICK_VALUES),
                               (N_NUMBER),
                               (N_BODY ? TokenSet::R_PAREN) },

            { N_NUMBER ::= (FLOAT) },
            { N_NUMBER ::= (INTEGER) },

            { N_DO ::= (KEYWORD_DO),
                       (N_BODY ? TokenSet::R_PAREN) },

            { N_IF ::= (KEYWORD_IF),
                       (N_COND),
                       (N_BODY) },

            { N_WHEN ::= (KEYWORD_WHEN),
                         (N_COND),
                         (N_BODY) },

            { N_WHILE ::= (KEYWORD_WHILE),
                          (N_COND),
                          (N_BODY ? TokenSet::R_PAREN) },

            { N_EACH ::= (KEYWORD_EACH),
                         (N_EACH_TABLE),
                         (N_BODY) },

            { N_EACH_TABLE ::= (L_BRACKET),
                               (N_ITERATION ++ TokenSet::ITERATOR, TokenSet::UNTIL),
                               (N_ITERATOR),
                               (N_UNTIL_CLAUSE ? TokenSet::R_BRACKET),
                               (R_BRACKET) },

            { N_ITERATION ::= (N_ASSIGN_PATTERN) },
            { N_ITERATION ::= (N_ASSIGN_PATTERN_LIST) },

            { N_FOR ::= (KEYWORD_FOR),
                        (N_FOR_TABLE),
                        (N_BODY) },

            { N_FOR_TABLE ::= (L_BRACKET),
                              (N_ITERATION_VALUE),
                              (N_RANGE),
                              (N_UNTIL_CLAUSE ? TokenSet::R_BRACKET),
                              (R_BRACKET) },

            { N_ITERATION_VALUE ::= (N_L_SYMBOL) },

            // Must eval to number
            { N_RANGE ::= (N_RANGE_START),
                          (N_RANGE_STOP),
                          (N_RANGE_STEP ? TokenSet::UNTIL) },

            { N_RANGE_START ::= (N_SEXP) },

            { N_RANGE_STOP ::= (N_SEXP) },

            { N_RANGE_STEP ::= (N_SEXP) },

            { N_ICOLLECT ::= (KEYWORD_ICOLLECT),
                             (N_ICOLLECT_TABLE),
                             (N_SEXP) },

            { N_ICOLLECT_TABLE ::= (L_BRACKET),
                                   (N_ITERATION ++ TokenSet::ITERATOR, TokenSet::COLLECT),
                                   (N_ITERATOR),
                                   (N_COLLECT_CLAUSE ? TokenSet::R_BRACKET),
                                   (R_BRACKET) },

            { N_COLLECT ::= (KEYWORD_COLLECT),
                            (N_COLLECT_TABLE),
                            (N_SEXP),
                            (N_SEXP ? TokenSet::R_PAREN) },

            { N_COLLECT_TABLE ::= (L_BRACKET),
                                  (N_ITERATION ++ TokenSet::ITERATOR, TokenSet::COLLECT),
                                  (N_ITERATOR),
                                  (N_COLLECT_CLAUSE ? TokenSet::R_BRACKET),
                                  (R_BRACKET) },

            { N_COLLECT_CLAUSE ::= (N_UNTIL_CLAUSE),
                                   (N_INTO_CLAUSE ? TokenSet::R_BRACKET) },
            { N_COLLECT_CLAUSE ::= (N_INTO_CLAUSE),
                                   (N_UNTIL_CLAUSE ? TokenSet::R_BRACKET) },

            { N_ACCUMULATE ::= (KEYWORD_ACCUMULATE),
                               (N_ACCUMULATE_TABLE),
                               (N_SEXP) },

            { N_ACCUMULATE_TABLE ::= (L_BRACKET),
                                     (N_ACCUMULATOR),
                                     (N_INITIAL),
                                     (N_ITERATION ++ TokenSet::ITERATOR, TokenSet::UNTIL),
                                     (N_ITERATOR),
                                     (N_UNTIL_CLAUSE ? TokenSet::R_BRACKET),
                                     (R_BRACKET) },

            { N_ACCUMULATOR ::= (N_L_SYMBOL) },

            { N_INITIAL ::= (N_SEXP) },

            { N_UNTIL_CLAUSE ::= (KEYWORD_UNTIL),
                                 (N_SEXP) },

            { N_INTO_CLAUSE ::= (KEYWORD_INTO),
                                (N_SEXP) },

            { N_OPERATION ::= (OPERATOR),
                              (N_ARGS ? TokenSet::R_PAREN) },
            { N_OPERATION ::= (KEYWORD_OR),
                              (N_ARGS ? TokenSet::R_PAREN) },
            { N_OPERATION ::= (LENGTH),
                              (N_SEXP ? TokenSet::R_PAREN) },
            { N_OPERATION ::= (COLON),
                              (N_ARGS ? TokenSet::R_PAREN) },

            { N_MATCH ::= (KEYWORD_MATCH),
                          (N_SEXP),
                          (N_MATCH_CLAUSE + TokenSet::R_PAREN) },

            { N_MATCH_CLAUSE ::= (N_MATCH_PATTERN_TOP),
                                 (N_SEXP) },

            { N_MATCH_PATTERN_TOP ::= (N_LITERAL) },
            { N_MATCH_PATTERN_TOP ::= (N_L_OR_R_SYMBOL) },
            { N_MATCH_PATTERN_TOP ::= (N_MATCH_PATTERN_TABLE) },
            { N_MATCH_PATTERN_TOP ::= (N_MATCH_PATTERN_KV_TABLE) },
            { N_MATCH_PATTERN_TOP ::= (N_MATCH_PATTERN_LIST) },

            { N_MATCH_PATTERN ::= (N_LITERAL) },
            { N_MATCH_PATTERN ::= (N_L_OR_R_SYMBOL) },
            { N_MATCH_PATTERN ::= (CAPTURE) },
            { N_MATCH_PATTERN ::= (N_MATCH_PATTERN_TABLE) },
            { N_MATCH_PATTERN ::= (N_MATCH_PATTERN_KV_TABLE) },

            { N_MATCH_PATTERN_LIST ::= (L_PAREN),
                                       (N_MATCH_PATTERN_LIST_REAL),
                                       (R_PAREN) },

            { N_MATCH_PATTERN_LIST_REAL ::= (N_MATCH_PATTERN),
                                            (N_MATCH_PATTERN_LIST_REST ? TokenSet::R_PAREN) },
            { N_MATCH_PATTERN_LIST_REAL ::= (N_WHERE_CLAUSE) },

            { N_MATCH_PATTERN_LIST_REST ::= (N_MATCH_PATTERN + TokenSet::R_PAREN) },
            { N_MATCH_PATTERN_LIST_REST ::= (N_GUARD_CLAUSE) },

            { N_GUARD_CLAUSE ::= (QUESTION),
                                 (N_COND * TokenSet::R_PAREN) },

            { N_MATCH_PATTERN_TABLE ::= (L_BRACKET),
                                        (N_MATCH_PATTERN * TokenSet::R_BRACKET),
                                        (R_BRACKET) },

            { N_MATCH_PATTERN_KV_TABLE ::= (L_BRACE),
                                           (N_MATCH_PATTERN_KV * TokenSet::R_BRACE),
                                           (R_BRACE) },

            { N_MATCH_PATTERN_KV ::= (N_MATCH_PATTERN),
                                     (N_MATCH_PATTERN) },
            { N_MATCH_PATTERN_KV ::= (COLON),
                                     (N_L_OR_R_SYMBOL) },

            { N_WHERE_CLAUSE ::= (KEYWORD_WHERE),
                                 (N_WHERE_PATTERN),
                                 (N_COND * TokenSet::R_PAREN) },

            { N_WHERE_PATTERN ::= (N_MATCH_PATTERN) },
            { N_WHERE_PATTERN ::= (N_OR_CLAUSE_LIST) },

            { N_OR_CLAUSE_LIST ::= (L_PAREN), (N_OR_CLAUSE), (R_PAREN) },

            { N_OR_CLAUSE ::= (KEYWORD_OR),
                              (N_MATCH_PATTERN ? TokenSet::R_PAREN) },

            { N_MATCH_TRY ::= (KEYWORD_MATCH_TRY),
                              (N_SEXP),
                              (N_MATCH_TRY_CLAUSE + TokenSet::R_PAREN) },

            // NOTE: OncePeek notations don't work here
            // { N_MATCH_TRY_CLAUSE ::= (N_MATCH_TRY_LIST > (TokenSet::MATCH_PATTERN,
            //                                               TokenSet::WHERE)),
            //                          (N_SEXP) },
            // { N_MATCH_TRY_CLAUSE ::= (N_CATCH_LIST > TokenSet::CATCH) },
            { N_MATCH_TRY_CLAUSE ::= (N_MATCH_PATTERN),
                                     (N_SEXP) },

            { N_MATCH_TRY_LIST ::= (L_PAREN),
                                   (N_MATCH_TRY_LIST_REAL),
                                   (R_PAREN) },

            { N_MATCH_TRY_LIST_REAL ::= (N_MATCH_PATTERN),
                                        (N_MATCH_PATTERN_LIST_REST ? TokenSet::R_PAREN) },
            { N_MATCH_TRY_LIST_REAL ::= (N_WHERE_CLAUSE) },

            { N_CATCH_LIST ::= (L_PAREN),
                               (N_CATCH + TokenSet::R_PAREN),
                               (R_PAREN) },

            { N_CATCH ::= (KEYWORD_CATCH),
                          (N_MATCH_CLAUSE + TokenSet::R_PAREN) },

            { N_THREAD ::= (THREAD),
                           (N_SEXP),
                           (N_CALL_FN * TokenSet::R_PAREN) },

            { N_DOTO ::= (KEYWORD_DOTO),
                         (N_SEXP),
                         (N_CALL_FN * TokenSet::R_PAREN) },

            { N_CALL_FN ::= (N_LIST) },
            { N_CALL_FN ::= (N_R_SYMBOL) }, // must be fn

            { N_INCLUDE ::= (KEYWORD_INCLUDE),
                            (N_STRING_LITERAL) },

            { N_LUA ::= (KEYWORD_LUA),
                        (N_STRING_LITERAL) },

            { N_PICK_ARGS ::= (KEYWORD_PICK_ARGS),
                              (N_SEXP),
                              (N_SEXP),
                              (N_SEXP * TokenSet::R_PAREN) },

            { N_MACRODEBUG ::= (KEYWORD_MACRODEBUG),
                               (N_SEXP + TokenSet::R_PAREN) },
        );

        // TODO: refactor
        match cur_rule.expect {
            N_MATCH_TRY_CLAUSE
                if sets::first_set(N_MATCH_TRY_LIST, cur_token.kind)
                    && (sets::TokenSet::MATCH_PATTERN
                        .contains(self.peek().kind)
                        || self.peek().kind == KEYWORD_WHERE) =>
            {
                self.expand(
                    cur_token,
                    vec![notation!((N_MATCH_TRY_LIST)), notation!((N_SEXP))]
                        .into_iter(),
                );
                return;
            }
            N_MATCH_TRY_CLAUSE
                if sets::first_set(N_CATCH_LIST, cur_token.kind)
                    && sets::TokenSet::CATCH.contains(self.peek().kind) =>
            {
                self.expand(
                    cur_token,
                    vec![notation!((N_CATCH_LIST))].into_iter(),
                );
                return;
            }
            _ => {}
        }

        // if every match fails
        self.recover_error(cur_token)
    }

    fn expand(
        &mut self,
        cur_token: Token,
        rules: impl Iterator<Item = Rule> + DoubleEndedIterator,
    ) {
        let cur_rule = self.rule_stack.pop().unwrap();
        if let Some(r) = cur_rule.expand() {
            self.rule_stack.push(r)
        }
        self.builder.start_node(cur_rule.expect.into());

        let mut rules = rules.peekable();
        let first_rule = rules.peek().unwrap();
        let range = if L_DELIMITERS.contains(&first_rule.expect) {
            Some(text_range(&cur_token.range))
        } else {
            None
        };
        self.rule_stack.push(Rule { notation: Close(range), ..cur_rule });
        self.rule_stack.extend(rules.rev());
    }

    fn recover_error(&mut self, cur_token: Token) {
        // if early close
        if let Some(list) = match cur_token.kind {
            R_PAREN => Some(LIST),
            R_BRACE => Some(KV_TABLE),
            R_BRACKET => Some(TABLE),
            _ => None,
        } {
            if self.early_close(list, &cur_token).is_some() {
                return;
            };
        }

        // We got something fun.
        if (cur_token.kind == SyntaxKind::KEYWORD_INTO
            || cur_token.kind == SyntaxKind::KEYWORD_UNTIL)
            && cur_token.text.starts_with(':')
        {
            self.token_queue.get_mut(0).unwrap().kind =
                SyntaxKind::COLON_STRING;
            // Check it again.
            return;
        }

        // So just treat it as a input error.
        self.builder.token(ERROR.into(), &cur_token.text);

        if cur_token.kind == END {
            // will clear rule_stack
            self.missing_close(cur_token);
        } else {
            self.errors.push(Error::new(
                text_range(&cur_token.range),
                Unexpected(cur_token.kind),
            ));
            self.step();
        }
    }

    fn early_close(
        &mut self,
        matched: &[SyntaxKind],
        cur_token: &Token,
    ) -> Option<()> {
        let (range, _total) = self.find_close(matched)?;
        let mut count = 0;
        for _i in range.rev() {
            if let Some(Rule { expect: n, notation: Close(prev) }) =
                self.rule_stack.pop()
            {
                self.builder.finish_node();
                count += 1;
                if LIST.iter().any(|n| *n != N_LIST && *n == cur_token.kind)
                    || TABLE.contains(&cur_token.kind)
                    || KV_TABLE.contains(&cur_token.kind)
                    // the first one
                    || count == 1
                {
                    self.errors.push(Error::new(
                        text_range(&cur_token.range),
                        Unterminated(n),
                    ));
                    if let Some(range) = prev {
                        self.errors.push(Error::new(range, Unterminated(n)))
                    }
                }
            }
        }
        self.builder.token(cur_token.kind.into(), &cur_token.text);
        self.step();
        Some(())
    }

    fn missing_close(&mut self, cur_token: Token) {
        while let Some(rule) = self.rule_stack.pop() {
            if let Rule { notation: Close(range), .. } = rule {
                self.builder.finish_node();
                if let Some(range) = range {
                    self.errors.push(Error::new(range, Dismatched))
                }
            }
        }
        self.errors.push(Error::new(
            text_range_with_offset(&cur_token.range, (-1, -1)),
            UnexpectedEof,
        ));
    }

    fn parse_symbol(&mut self, cur_token: Token) {
        let cur_text = cur_token.text.to_string();

        let (symbol, method) = if let Some((s, m)) = cur_text.split_once(':') {
            (s, Some(m))
        } else {
            (cur_text.as_str(), None)
        };

        let mut split = symbol.split('.').peekable();
        let symbol_alone = split.next().unwrap();
        self.builder.token(SYMBOL.into(), symbol_alone);

        let mut length = symbol_alone.len();
        while let Some(s) = split.next_if(|s| !s.is_empty()) {
            let field = &(".".to_owned() + s);
            length += field.len();
            self.builder.token(SYMBOL_FIELD.into(), field);
        }
        if split.peek().is_some() {
            self.builder.token(ERROR.into(), &cur_text[length..]);
            self.errors.push(Error::new(
                text_range_with_offset(&cur_token.range, (length as i32, 0)),
                InvalidSymbol,
            ));
            return;
        }

        if let Some(str) = method {
            if str.is_empty() || str[1..].contains(':') || str.contains('.') {
                let offset = symbol.len();
                self.builder.token(ERROR.into(), &cur_text[offset..]);
                self.errors.push(Error::new(
                    text_range_with_offset(
                        &cur_token.range,
                        (offset as i32, 0),
                    ),
                    InvalidSymbol,
                ));
            } else {
                self.builder
                    .token(SYMBOL_METHOD.into(), &(":".to_string() + str));
            }
        }
    }

    fn find_close(
        &self,
        kinds: &[SyntaxKind],
    ) -> Option<(std::ops::Range<usize>, usize)> {
        let mut total = 0;
        self.rule_stack
            .iter()
            .rposition(|x| {
                if let Rule { expect: kind, notation: Close(_) } = x {
                    total += 1;
                    kinds.contains(kind)
                } else {
                    false
                }
            })
            .map(|index| (index..self.rule_stack.len(), total))
    }
}

#[cfg(test)]
mod tests {
    use rowan::TextRange;

    use super::*;
    use crate::SyntaxNode;

    #[test]
    fn parse_chunk() {
        let text = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/testdata/parser/raw.fnl"
        ));
        let parser = Parser::new(text.chars());
        let syntax_node = SyntaxNode::new_root(parser.parse().green_node);
        assert_eq!(
            format!("{:#?}", syntax_node),
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/testdata/parser/cst"
            )),
        )
    }

    #[test]
    fn symbol_ok() {
        let text = "x x.1 x.1.2 x.1.2:3 x:3";
        let parser = Parser::new(text.chars());
        assert_eq!(parser.parse().errors, vec![]);
    }

    #[test]
    fn symbol_error() {
        let text = "x. x:";
        let parser = Parser::new(text.chars());
        assert_eq!(parser.parse().errors, vec![
            Error::new(TextRange::new(1.into(), 2.into()), InvalidSymbol),
            Error::new(TextRange::new(4.into(), 5.into()), InvalidSymbol),
        ]);
        let text = "x.1:2.3 x.1..2:3 x.1:1:2";
        let parser = Parser::new(text.chars());
        assert_eq!(parser.parse().errors, vec![
            Error::new(TextRange::new(3.into(), 7.into()), InvalidSymbol),
            Error::new(TextRange::new(11.into(), 16.into()), InvalidSymbol),
            Error::new(TextRange::new(20.into(), 24.into()), InvalidSymbol),
        ]);
    }

    #[test]
    fn let_destruct() {
        // Who would go crazy for this grammar?
        let text = "(local x :name)
            (let [ {(((x))) {:name value}} { :name {x 3} } ] (print value))";
        let parser = Parser::new(text.chars());
        assert!(parser.parse().errors.is_empty())
    }

    #[test]
    fn r#macro() {
        let text = "(: (. vim.opt ,(name:sub 1 -2)) :append)";
        let parser = Parser::new(text.chars());
        assert!(parser.parse().errors.is_empty())
    }

    #[test]
    fn errors() {
        let text = "(with-open 1.1] [a])";
        let parser = Parser::new(text.chars());
        let parsed = parser.parse();
        assert_eq!(parsed.errors, vec![
            Error::new(
                TextRange::new(11.into(), 14.into()),
                Unexpected(FLOAT)
            ),
            Error::new(
                TextRange::new(14.into(), 15.into()),
                Unexpected(R_BRACKET)
            ),
            Error::new(
                TextRange::new(18.into(), 19.into()),
                Unterminated(N_NV_PAIR)
            ),
        ]);
    }

    #[test]
    fn incomplete() {
        let text = "(for [i 1 128 &until ] (set x (+ x i)))";
        let parser = Parser::new(text.chars());
        let parsed = parser.parse();
        assert_eq!(parsed.errors, vec![Error::new(
            TextRange::new(21.into(), 22.into()),
            Unterminated(N_UNTIL_CLAUSE)
        ),]);
        let text = "(let [{: x} {:x :3} (print x))";
        let parser = Parser::new(text.chars());
        let parsed = parser.parse();
        assert_eq!(parsed.errors, vec![Error::new(
            TextRange::new(29.into(), 30.into()),
            Unterminated(N_ASSIGN_PAIR)
        ),]);
    }

    #[test]
    fn missing_delimiter() {
        let text = "(x";
        let parser = Parser::new(text.chars());
        let parsed = parser.parse();
        assert_eq!(parsed.errors, vec![
            Error::new(TextRange::new(0.into(), 1.into()), Dismatched),
            Error::new(TextRange::new(1.into(), 1.into()), UnexpectedEof),
        ]);
    }
}
