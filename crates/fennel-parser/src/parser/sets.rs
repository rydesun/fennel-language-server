use crate::SyntaxKind::{self, *};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
pub(crate) enum TokenSet {
    END,
    R_PAREN,
    L_BRACKET,
    R_BRACKET,
    R_BRACE,
    VARARG_BRACKET,
    ITERATOR,
    UNTIL,
    COLLECT,
    MATCH_PATTERN,
    CATCH,
}

impl TokenSet {
    pub(crate) fn contains(self, k: SyntaxKind) -> bool {
        match self {
            Self::END => k == END,
            Self::R_PAREN => k == R_PAREN,
            Self::L_BRACKET => k == L_BRACKET,
            Self::R_BRACKET => k == R_BRACKET,
            Self::R_BRACE => k == R_BRACE,
            Self::VARARG_BRACKET => [VARARG, R_BRACKET].contains(&k),
            Self::ITERATOR => [L_PAREN, BACKTICK, COMMA, HASHFN].contains(&k),
            Self::UNTIL => [KEYWORD_UNTIL, R_BRACKET].contains(&k),
            Self::COLLECT => [KEYWORD_COLLECT, R_BRACKET].contains(&k),
            Self::MATCH_PATTERN => [
                FLOAT,
                INTEGER,
                QUOTE_STRING,
                COLON_STRING,
                BOOL,
                NIL,
                SYMBOL,
                L_BRACKET,
                L_BRACE,
            ]
            .contains(&k),
            Self::CATCH => k == KEYWORD_CATCH,
        }
    }
}

// That handwrite first set is ridiculous, but it just works.
#[rustfmt::skip]
pub(crate) fn first_set(syntax: &SyntaxKind) -> Option<Vec<SyntaxKind>> {
    match syntax {
        N_SEXP => Some(vec![
            SYMBOL, COMMA, VARARG, FLOAT, INTEGER, QUOTE_STRING, COLON_STRING,
            BOOL, NIL, L_BRACKET, L_BRACE, L_PAREN, BACKTICK,
            HASHFN,
        ]),
        // No COMMA.
        // COMMA -> N_LIST -> N_MACRO_UNQUOTE
        N_ATOM => Some(vec![
            SYMBOL, VARARG, FLOAT, INTEGER, QUOTE_STRING, COLON_STRING,
            BOOL, NIL, L_BRACKET, L_BRACE,
        ]),
        N_LIST => Some(vec![
            L_PAREN, BACKTICK, COMMA, HASHFN,
        ]),
        N_LITERAL => Some(vec![
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
        ]),
        N_STRING_LITERAL => Some(vec![
            QUOTE_STRING, COLON_STRING,
        ]),
        N_NUMBER => Some(vec![FLOAT, INTEGER]),
        N_SEQ_TABLE => Some(vec![L_BRACKET]),
        N_KV_TABLE => Some(vec![L_BRACE]),
        N_L_SYMBOL => Some(vec![SYMBOL, COMMA]),
        N_R_SYMBOL => Some(vec![SYMBOL, COMMA]),
        N_L_R_SYMBOL => Some(vec![SYMBOL, COMMA]),
        N_L_OR_R_SYMBOL => Some(vec![SYMBOL, COMMA]),
        N_KEY => Some(vec![
            SYMBOL, COMMA, VARARG, FLOAT, INTEGER, QUOTE_STRING,
            COLON_STRING, BOOL, NIL, L_BRACKET, L_BRACE, L_PAREN,
            BACKTICK, HASHFN,
        ]),
        N_MACRO_LIST => Some(vec![BACKTICK, COMMA, HASHFN]),
        N_MACRO_HASH => Some(vec![HASHFN]),
        N_MACRO_UNQUOTE => Some(vec![COMMA]),
        N_MACRO_QUOTE => Some(vec![BACKTICK]),
        N_SYMBOL_CALL => Some(vec![SYMBOL, COMMA]),
        N_VAR => Some(vec![KEYWORD_VAR]),
        N_SET => Some(vec![KEYWORD_SET]),
        N_TSET => Some(vec![KEYWORD_TSET]),
        N_LOCAL => Some(vec![KEYWORD_LOCAL]),

        N_VALUES => Some(vec![KEYWORD_VALUES]),
        N_GLOBAL => Some(vec![KEYWORD_GLOBAL]),
        N_IMPORT_MACROS => Some(vec![KEYWORD_IMPORT_MACROS]),
        N_REQUIRE_MACROS => Some(vec![KEYWORD_REQUIRE_MACROS]),
        N_MACRO => Some(vec![KEYWORD_MACRO]),
        N_MACROS => Some(vec![KEYWORD_MACROS]),
        N_EVAL_COMPILER => Some(vec![KEYWORD_EVAL_COMPILER]),
        N_FN => Some(vec![KEYWORD_FN]),
        N_LAMBDA => Some(vec![KEYWORD_LAMBDA]),
        N_PARTIAL => Some(vec![KEYWORD_PARTIAL]),
        N_LET => Some(vec![KEYWORD_LET]),
        N_MATCH => Some(vec![KEYWORD_MATCH]),
        N_MATCH_TRY => Some(vec![KEYWORD_MATCH_TRY]),
        N_WITH_OPEN => Some(vec![KEYWORD_WITH_OPEN]),
        N_PICK_VALUE => Some(vec![KEYWORD_PICK_VALUES]),
        N_IF => Some(vec![KEYWORD_IF]),
        N_WHEN => Some(vec![KEYWORD_WHEN]),
        N_EACH => Some(vec![KEYWORD_EACH]),
        N_FOR => Some(vec![KEYWORD_FOR]),
        N_COLLECT => Some(vec![KEYWORD_COLLECT]),
        N_ICOLLECT => Some(vec![KEYWORD_ICOLLECT]),
        N_ACCUMULATE => Some(vec![KEYWORD_ACCUMULATE]),
        N_DO => Some(vec![KEYWORD_DO]),
        N_WHILE => Some(vec![KEYWORD_WHILE]),

        N_OPERATION => Some(vec![OPERATOR, KEYWORD_OR, COLON, LENGTH]),

        N_THREAD => Some(vec![THREAD]),
        N_CALL_FN => Some(vec![
            SYMBOL, COMMA, L_PAREN, BACKTICK, HASHFN,
        ]),

        N_DOTO => Some(vec![KEYWORD_DOTO]),
        N_INCLUDE => Some(vec![KEYWORD_INCLUDE]),
        N_LUA => Some(vec![KEYWORD_LUA]),
        N_PICK_ARGS => Some(vec![KEYWORD_PICK_ARGS]),
        N_MACRODEBUG => Some(vec![KEYWORD_MACRODEBUG]),

        N_ASSIGN_PATTERN => Some(vec![SYMBOL, COMMA, L_BRACKET, L_BRACE, CAPTURE]),
        N_SET_PATTERN => Some(vec![SYMBOL, COMMA, L_BRACKET, L_BRACE, CAPTURE]),
        N_ASSIGN_PATTERN_LIST => Some(vec![L_PAREN]),
        N_SET_PATTERN_LIST => Some(vec![L_PAREN]),
        N_ASSIGN_PATTERN_TABLE => Some(vec![L_BRACKET]),
        N_SET_PATTERN_TABLE => Some(vec![L_BRACKET]),
        N_ASSIGN_PATTERN_KV_TABLE => Some(vec![L_BRACE]),
        N_SET_PATTERN_KV_TABLE => Some(vec![L_BRACE]),
        N_ASSIGN_PATTERN_KEY => Some(vec![SYMBOL, COMMA, L_PAREN,
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            CAPTURE,
        ]),
        N_SET_PATTERN_KEY => Some(vec![SYMBOL, COMMA, L_PAREN,
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            CAPTURE,
        ]),
        N_ASSIGN_PATTERN_KEY_NEST => Some(vec![L_PAREN]),
        N_SET_PATTERN_KEY_NEST => Some(vec![L_PAREN]),
        N_RANGE_START => Some(vec![
            SYMBOL, COMMA, VARARG, FLOAT, INTEGER, QUOTE_STRING,
            COLON_STRING, BOOL, NIL, L_BRACKET, L_BRACE, L_PAREN,
            BACKTICK, HASHFN,
        ]),
        N_UNTIL_CLAUSE => Some(vec![KEYWORD_UNTIL]),
        N_INTO_CLAUSE => Some(vec![KEYWORD_INTO]),
        N_MATCH_PATTERN_TOP => Some(vec![
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            SYMBOL, COMMA, L_BRACKET, L_BRACE, L_PAREN,
        ]),
        N_MATCH_PATTERN => Some(vec![
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            SYMBOL, COMMA, L_BRACKET, L_BRACE, CAPTURE,
        ]),
        N_WHERE_CLAUSE => Some(vec![KEYWORD_WHERE]),
        N_MATCH_CLAUSE => Some(vec![
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            SYMBOL, COMMA, L_BRACKET, L_BRACE, L_PAREN,
        ]),
        N_MATCH_TRY_LIST => Some(vec![L_PAREN]),
        N_MATCH_TRY_CLAUSE => Some(vec![
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            SYMBOL, COMMA, L_BRACKET, L_BRACE, L_PAREN,
        ]),
        N_CATCH_LIST => Some(vec![L_PAREN]),
        N_CATCH => Some(vec![KEYWORD_CATCH]),
        N_GUARD_CLAUSE => Some(vec![QUESTION]),
        N_OR_CLAUSE_LIST => Some(vec![L_PAREN]),
        N_MATCH_PATTERN_TABLE => Some(vec![L_BRACKET]),
        N_MATCH_PATTERN_KV_TABLE => Some(vec![L_BRACE]),
        N_MATCH_PATTERN_LIST => Some(vec![L_PAREN]),
        N_IMPORT_MACROS_KV_TABLE => Some(vec![L_BRACE]),
        N_IMPORT_MACROS_DESTRUCT => Some(vec![L_BRACE, SYMBOL, COMMA]),
        _ => Some(vec![*syntax]),
    }
}
