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
pub(crate) fn first_set(syntax: SyntaxKind, cur: SyntaxKind) -> bool {
    match syntax {
        N_SEXP => [
            SYMBOL, COMMA, VARARG, FLOAT, INTEGER, QUOTE_STRING, COLON_STRING,
            BOOL, NIL, L_BRACKET, L_BRACE, L_PAREN, BACKTICK,
            HASHFN,
        ].contains(&cur),
        // No COMMA.
        // COMMA -> N_LIST -> N_MACRO_UNQUOTE
        N_ATOM => [
            SYMBOL, VARARG, FLOAT, INTEGER, QUOTE_STRING, COLON_STRING,
            BOOL, NIL, L_BRACKET, L_BRACE,
        ].contains(&cur),
        N_LIST => [
            L_PAREN, BACKTICK, COMMA, HASHFN,
        ].contains(&cur),
        N_LITERAL => [
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
        ].contains(&cur),
        N_STRING_LITERAL => [
            QUOTE_STRING, COLON_STRING,
        ].contains(&cur),
        N_NUMBER => [FLOAT, INTEGER].contains(&cur),
        N_SEQ_TABLE => [L_BRACKET].contains(&cur),
        N_KV_TABLE => [L_BRACE].contains(&cur),
        N_L_SYMBOL => [SYMBOL, COMMA].contains(&cur),
        N_R_SYMBOL => [SYMBOL, COMMA].contains(&cur),
        N_L_R_SYMBOL => [SYMBOL, COMMA].contains(&cur),
        N_L_OR_R_SYMBOL => [SYMBOL, COMMA].contains(&cur),
        N_KEY => [
            SYMBOL, COMMA, VARARG, FLOAT, INTEGER, QUOTE_STRING,
            COLON_STRING, BOOL, NIL, L_BRACKET, L_BRACE, L_PAREN,
            BACKTICK, HASHFN,
        ].contains(&cur),
        N_MACRO_LIST => [BACKTICK, COMMA, HASHFN].contains(&cur),
        N_MACRO_HASH => [HASHFN].contains(&cur),
        N_MACRO_UNQUOTE => [COMMA].contains(&cur),
        N_MACRO_QUOTE => [BACKTICK].contains(&cur),
        N_SYMBOL_CALL => [SYMBOL, COMMA].contains(&cur),
        N_VAR => [KEYWORD_VAR].contains(&cur),
        N_SET => [KEYWORD_SET].contains(&cur),
        N_TSET => [KEYWORD_TSET].contains(&cur),
        N_LOCAL => [KEYWORD_LOCAL].contains(&cur),

        N_VALUES => [KEYWORD_VALUES].contains(&cur),
        N_GLOBAL => [KEYWORD_GLOBAL].contains(&cur),
        N_IMPORT_MACROS => [KEYWORD_IMPORT_MACROS].contains(&cur),
        N_REQUIRE_MACROS => [KEYWORD_REQUIRE_MACROS].contains(&cur),
        N_MACRO => [KEYWORD_MACRO].contains(&cur),
        N_MACROS => [KEYWORD_MACROS].contains(&cur),
        N_EVAL_COMPILER => [KEYWORD_EVAL_COMPILER].contains(&cur),
        N_FN => [KEYWORD_FN].contains(&cur),
        N_LAMBDA => [KEYWORD_LAMBDA].contains(&cur),
        N_PARTIAL => [KEYWORD_PARTIAL].contains(&cur),
        N_LET => [KEYWORD_LET].contains(&cur),
        N_MATCH => [KEYWORD_MATCH].contains(&cur),
        N_MATCH_TRY => [KEYWORD_MATCH_TRY].contains(&cur),
        N_WITH_OPEN => [KEYWORD_WITH_OPEN].contains(&cur),
        N_PICK_VALUE => [KEYWORD_PICK_VALUES].contains(&cur),
        N_IF => [KEYWORD_IF].contains(&cur),
        N_WHEN => [KEYWORD_WHEN].contains(&cur),
        N_EACH => [KEYWORD_EACH].contains(&cur),
        N_FOR => [KEYWORD_FOR].contains(&cur),
        N_COLLECT => [KEYWORD_COLLECT].contains(&cur),
        N_ICOLLECT => [KEYWORD_ICOLLECT].contains(&cur),
        N_ACCUMULATE => [KEYWORD_ACCUMULATE].contains(&cur),
        N_DO => [KEYWORD_DO].contains(&cur),
        N_WHILE => [KEYWORD_WHILE].contains(&cur),

        N_OPERATION => [OPERATOR, KEYWORD_OR, COLON, LENGTH].contains(&cur),

        N_THREAD => [THREAD].contains(&cur),
        N_CALL_FN => [
            SYMBOL, COMMA, L_PAREN, BACKTICK, HASHFN,
        ].contains(&cur),

        N_DOTO => [KEYWORD_DOTO].contains(&cur),
        N_INCLUDE => [KEYWORD_INCLUDE].contains(&cur),
        N_LUA => [KEYWORD_LUA].contains(&cur),
        N_PICK_ARGS => [KEYWORD_PICK_ARGS].contains(&cur),
        N_MACRODEBUG => [KEYWORD_MACRODEBUG].contains(&cur),

        N_ASSIGN_PATTERN => [SYMBOL, COMMA, L_BRACKET, L_BRACE, CAPTURE].contains(&cur),
        N_SET_PATTERN => [SYMBOL, COMMA, L_BRACKET, L_BRACE, CAPTURE].contains(&cur),
        N_ASSIGN_PATTERN_LIST => [L_PAREN].contains(&cur),
        N_SET_PATTERN_LIST => [L_PAREN].contains(&cur),
        N_ASSIGN_PATTERN_TABLE => [L_BRACKET].contains(&cur),
        N_SET_PATTERN_TABLE => [L_BRACKET].contains(&cur),
        N_ASSIGN_PATTERN_KV_TABLE => [L_BRACE].contains(&cur),
        N_SET_PATTERN_KV_TABLE => [L_BRACE].contains(&cur),
        N_ASSIGN_PATTERN_KEY => [SYMBOL, COMMA, L_PAREN,
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            CAPTURE,
        ].contains(&cur),
        N_SET_PATTERN_KEY => [SYMBOL, COMMA, L_PAREN,
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            CAPTURE,
        ].contains(&cur),
        N_ASSIGN_PATTERN_KEY_NEST => [L_PAREN].contains(&cur),
        N_SET_PATTERN_KEY_NEST => [L_PAREN].contains(&cur),
        N_RANGE_START => [
            SYMBOL, COMMA, VARARG, FLOAT, INTEGER, QUOTE_STRING,
            COLON_STRING, BOOL, NIL, L_BRACKET, L_BRACE, L_PAREN,
            BACKTICK, HASHFN,
        ].contains(&cur),
        N_UNTIL_CLAUSE => [KEYWORD_UNTIL].contains(&cur),
        N_INTO_CLAUSE => [KEYWORD_INTO].contains(&cur),
        N_MATCH_PATTERN_TOP => [
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            SYMBOL, COMMA, L_BRACKET, L_BRACE, L_PAREN,
        ].contains(&cur),
        N_MATCH_PATTERN => [
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            SYMBOL, COMMA, L_BRACKET, L_BRACE, CAPTURE,
        ].contains(&cur),
        N_WHERE_CLAUSE => [KEYWORD_WHERE].contains(&cur),
        N_MATCH_CLAUSE => [
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            SYMBOL, COMMA, L_BRACKET, L_BRACE, L_PAREN,
        ].contains(&cur),
        N_MATCH_TRY_LIST => [L_PAREN].contains(&cur),
        N_MATCH_TRY_CLAUSE => [
            FLOAT, INTEGER, QUOTE_STRING, COLON_STRING, BOOL, NIL,
            SYMBOL, COMMA, L_BRACKET, L_BRACE, L_PAREN,
        ].contains(&cur),
        N_CATCH_LIST => [L_PAREN].contains(&cur),
        N_CATCH => [KEYWORD_CATCH].contains(&cur),
        N_GUARD_CLAUSE => [QUESTION].contains(&cur),
        N_OR_CLAUSE_LIST => [L_PAREN].contains(&cur),
        N_MATCH_PATTERN_TABLE => [L_BRACKET].contains(&cur),
        N_MATCH_PATTERN_KV_TABLE => [L_BRACE].contains(&cur),
        N_MATCH_PATTERN_LIST => [L_PAREN].contains(&cur),
        N_IMPORT_MACROS_KV_TABLE => [L_BRACE].contains(&cur),
        N_IMPORT_MACROS_DESTRUCT => [L_BRACE, SYMBOL, COMMA].contains(&cur),
        _ => syntax == cur,
    }
}
