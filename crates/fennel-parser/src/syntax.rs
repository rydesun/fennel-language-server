use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
pub enum SyntaxKind {
    SYMBOL,
    SYMBOL_FIELD,
    SYMBOL_METHOD,

    FLOAT,
    INTEGER,
    BOOL,
    NIL,
    QUOTE_STRING,
    COLON_STRING,
    KEYWORD_OR,
    OPERATOR,
    COLON,
    HASHFN,
    CAPTURE,
    LENGTH,
    THREAD,
    L_PAREN,
    R_PAREN,
    L_BRACKET,
    R_BRACKET,
    L_BRACE,
    R_BRACE,
    COMMA,
    VARARG,
    QUESTION,
    KEYWORD_FN,
    KEYWORD_LAMBDA,
    KEYWORD_LOCAL,
    KEYWORD_LET,
    KEYWORD_SET,
    KEYWORD_INCLUDE,
    KEYWORD_ACCUMULATE,
    BACKTICK,
    KEYWORD_CATCH,
    KEYWORD_COLLECT,
    KEYWORD_DO,
    KEYWORD_DOTO,
    KEYWORD_EACH,
    KEYWORD_EVAL_COMPILER,
    KEYWORD_FOR,
    KEYWORD_GLOBAL,
    KEYWORD_ICOLLECT,
    KEYWORD_FCOLLECT,
    KEYWORD_IF,
    KEYWORD_IMPORT_MACROS,
    KEYWORD_INTO,
    KEYWORD_LUA,
    KEYWORD_MACRO,
    KEYWORD_MACRODEBUG,
    KEYWORD_MACROS,
    KEYWORD_MATCH,
    KEYWORD_MATCH_TRY,
    KEYWORD_PARTIAL,
    KEYWORD_PICK_ARGS,
    KEYWORD_VALUES,
    KEYWORD_PICK_VALUES,
    KEYWORD_REQUIRE_MACROS,
    KEYWORD_TSET,
    KEYWORD_UNTIL,
    KEYWORD_VAR,
    KEYWORD_WHEN,
    KEYWORD_WHERE,
    KEYWORD_WHILE,
    KEYWORD_WITH_OPEN,
    KEYWORD_COMMENT,

    COMMENT,
    WHITESPACE,
    ERROR,

    N_LIST,
    N_SUBLIST,
    N_MACRO_LIST,
    N_SEXP,
    N_ATOM,
    N_SYMBOL_CALL,
    N_DOT_SYMBOL,
    N_COLON_SYMBOL,
    N_LITERAL,
    N_STRING_LITERAL,
    N_NUMBER,
    N_PARAM,
    N_SEQ_TABLE,
    N_KV_TABLE,
    N_KV_PAIR,
    N_KEY,
    N_VALUE,
    N_L_SYMBOL,
    N_R_SYMBOL,
    N_L_R_SYMBOL,
    N_L_OR_R_SYMBOL,
    N_FN_NAME,
    N_ARGS,
    N_BODY,
    N_COND,
    N_ITERATOR,
    N_LEAF_LIST,
    N_NODE_LIST,
    N_TSET,
    N_LOCAL,
    N_VALUES,
    N_GLOBAL,
    N_IMPORT_MACROS,
    N_REQUIRE_MACROS,
    N_MACRO,
    N_MACRO_NAME,
    N_MACROS,
    N_EVAL_COMPILER,
    N_VAR,
    N_SET,
    N_FOR,
    N_FOR_TABLE,
    N_ITERATION,
    N_ITERATION_VALUE,
    N_FN,
    N_LAMBDA,
    N_HASHFN,
    N_MACRO_HASH,
    N_MACRO_UNQUOTE,
    N_MACRO_QUOTE,
    N_PARTIAL,
    N_LET,
    N_MATCH,
    N_WITH_OPEN,
    N_PICK_VALUE,
    N_IF,
    N_WHEN,
    N_WHILE,
    N_EACH,
    N_EACH_TABLE,
    N_ICOLLECT,
    N_ICOLLECT_TABLE,
    N_COLLECT,
    N_COLLECT_TABLE,
    N_FCOLLECT,
    N_FCOLLECT_TABLE,
    N_ACCUMULATE,
    N_ACCUMULATE_TABLE,
    N_DO,
    N_OPERATION,
    N_THREAD,
    N_DOTO,
    N_CALL_FN,
    N_INCLUDE,
    N_LUA,
    N_PICK_ARGS,
    N_MACRODEBUG,
    N_NV_PAIR,
    N_NV_PAIR_TABLE,
    N_ASSIGN_TABLE,
    N_ASSIGN_PAIR,
    N_SET_PAIR,
    N_ASSIGN_PATTERN,
    N_SET_PATTERN,
    N_ASSIGN_PATTERN_LIST,
    N_SET_PATTERN_LIST,
    N_ASSIGN_PATTERN_KV_TABLE,
    N_SET_PATTERN_KV_TABLE,
    N_ASSIGN_PATTERN_TABLE,
    N_SET_PATTERN_TABLE,
    N_ASSIGN_PATTERN_KV,
    N_SET_PATTERN_KV,
    N_ASSIGN_PATTERN_KEY,
    N_SET_PATTERN_KEY,
    N_ASSIGN_PATTERN_KEY_NEST,
    N_SET_PATTERN_KEY_NEST,
    N_PARAM_TABLE,
    N_RANGE,
    N_RANGE_START,
    N_RANGE_STOP,
    N_RANGE_STEP,
    N_COLLECT_CLAUSE,
    N_ACCUMULATOR,
    N_INITIAL,
    N_UNTIL_CLAUSE,
    N_GUARD_CLAUSE,
    N_INTO_CLAUSE,
    N_MATCH_CLAUSE,
    N_MATCH_PATTERN,
    N_MATCH_PATTERN_TOP,
    N_MATCH_PATTERN_LIST,
    N_MATCH_PATTERN_LIST_REAL,
    N_MATCH_PATTERN_LIST_REST,
    N_MATCH_CLAUSE_LIST,
    N_MATCH_CLAUSE_LIST_REAL,
    N_MATCH_CLAUSE_LIST_REST,
    N_MATCH_TRY,
    N_MATCH_TRY_CLAUSE,
    N_MATCH_TRY_LIST,
    N_MATCH_TRY_LIST_REAL,
    N_MATCH_PATTERN_TABLE,
    N_MATCH_PATTERN_KV,
    N_MATCH_PATTERN_KV_TABLE,
    N_IMPORT_MACROS_PAIR,
    N_IMPORT_MACROS_DESTRUCT,
    N_IMPORT_MACROS_KV_TABLE,
    N_IMPORT_MACROS_KV_PAIR,
    N_WHERE_PATTERN,
    N_WHERE_CLAUSE,
    N_OR_CLAUSE,
    N_OR_CLAUSE_LIST,
    N_CATCH,
    N_CATCH_LIST,
    N_VARARG,
    N_COMMENT_FORM,

    END,
    ROOT,
}

impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SyntaxKind::*;
        let name = match self {
            L_PAREN => "`(`".into(),
            R_PAREN => "`)`".into(),
            L_BRACE => "`{`".into(),
            R_BRACE => "`}`".into(),
            L_BRACKET => "`[`".into(),
            R_BRACKET => "`]`".into(),

            N_PARAM_TABLE => "parameter table".into(),
            N_ASSIGN_PATTERN_LIST
            | N_MATCH_PATTERN_LIST
            | N_ASSIGN_PATTERN_TABLE
            | N_MATCH_PATTERN_TABLE
            | N_MATCH_TRY_LIST => "pattern table".into(),
            N_OR_CLAUSE_LIST => "`or` clause".into(),
            N_CATCH_LIST => "`catch` clause".into(),

            N_KV_TABLE
            | N_ASSIGN_PATTERN_KV_TABLE
            | N_MATCH_PATTERN_KV_TABLE
            | N_IMPORT_MACROS_KV_TABLE => "key/value table".into(),

            N_NV_PAIR => "name/value pair".into(),
            N_KV_PAIR => "key/value pair".into(),
            N_IMPORT_MACROS_PAIR | N_IMPORT_MACROS_KV_PAIR | N_ASSIGN_PAIR => {
                "binding pair".into()
            }

            N_SEQ_TABLE => "sequential table".into(),

            N_ASSIGN_TABLE => "binding table".into(),

            N_ACCUMULATE => "`accumulate` expression".into(),
            N_FOR => "`for` expression".into(),
            N_EACH => "`each` expression".into(),
            N_ICOLLECT => "`icollect` expression".into(),
            N_COLLECT => "`collect` expression".into(),

            N_ACCUMULATE_TABLE => "`accumulate` binding table".into(),
            N_FOR_TABLE => "`for` binding table".into(),
            N_EACH_TABLE => "`each` binding table".into(),
            N_ICOLLECT_TABLE => "`icollect` binding table".into(),
            N_COLLECT_TABLE => "`collect` binding table".into(),

            _ => {
                let raw = format!("{:?}", self);
                if raw.starts_with("N_") {
                    format!(
                        "node `{}`",
                        raw.strip_prefix("N_")
                            .unwrap()
                            .to_lowercase()
                            .replace('_', " ")
                    )
                } else if raw.starts_with("KEYWORD_") {
                    format!(
                        "keyword `{}`",
                        raw.strip_prefix("KEYWORD_")
                            .unwrap()
                            .to_lowercase()
                            .replace('_', "-")
                    )
                } else {
                    raw
                }
            }
        };
        write!(f, "{}", name)
    }
}

pub(crate) mod lists {
    use super::SyntaxKind::{self, *};

    pub(crate) const L_DELIMITERS: &[SyntaxKind] =
        &[L_BRACE, L_PAREN, L_BRACKET];
    pub(crate) const OUTBAND: &[SyntaxKind] = &[WHITESPACE, COMMENT];
    pub(crate) const LIST: &[SyntaxKind] = &[
        N_LIST,
        N_ASSIGN_PATTERN_LIST,
        N_MATCH_PATTERN_LIST,
        N_OR_CLAUSE_LIST,
        N_MATCH_TRY_LIST,
        N_CATCH_LIST,
    ];
    pub(crate) const KV_TABLE: &[SyntaxKind] = &[
        N_KV_TABLE,
        N_ASSIGN_PATTERN_KV_TABLE,
        N_MATCH_PATTERN_KV_TABLE,
        N_IMPORT_MACROS_KV_TABLE,
    ];
    pub(crate) const TABLE: &[SyntaxKind] = &[
        N_SEQ_TABLE,
        N_NV_PAIR_TABLE,
        N_ASSIGN_TABLE,
        N_PARAM_TABLE,
        N_ASSIGN_PATTERN_TABLE,
        N_MATCH_PATTERN_TABLE,
        N_FOR_TABLE,
        N_EACH_TABLE,
        N_ICOLLECT_TABLE,
        N_COLLECT_TABLE,
        N_ACCUMULATE_TABLE,
    ];
}

pub(crate) const TOEKN: &[(&str, SyntaxKind)] = &[
    ("true", SyntaxKind::BOOL),
    ("false", SyntaxKind::BOOL),
    ("nil", SyntaxKind::NIL),
    ("or", SyntaxKind::KEYWORD_OR),
    ("and", SyntaxKind::OPERATOR),
    ("+", SyntaxKind::OPERATOR),
    ("-", SyntaxKind::OPERATOR),
    ("*", SyntaxKind::OPERATOR),
    ("/", SyntaxKind::OPERATOR),
    ("//", SyntaxKind::OPERATOR),
    ("%", SyntaxKind::OPERATOR),
    ("^", SyntaxKind::OPERATOR),
    (">", SyntaxKind::OPERATOR),
    ("<", SyntaxKind::OPERATOR),
    (">=", SyntaxKind::OPERATOR),
    ("<=", SyntaxKind::OPERATOR),
    ("~=", SyntaxKind::OPERATOR),
    ("=", SyntaxKind::OPERATOR),
    ("not=", SyntaxKind::OPERATOR),
    ("lshift", SyntaxKind::OPERATOR),
    ("rshift", SyntaxKind::OPERATOR),
    ("band", SyntaxKind::OPERATOR),
    ("bor", SyntaxKind::OPERATOR),
    ("bxor", SyntaxKind::OPERATOR),
    ("..", SyntaxKind::OPERATOR),
    ("not", SyntaxKind::OPERATOR),
    ("bnot", SyntaxKind::OPERATOR),
    ("length", SyntaxKind::OPERATOR),
    (".", SyntaxKind::OPERATOR),
    ("?.", SyntaxKind::OPERATOR),
    (":", SyntaxKind::COLON),
    ("#", SyntaxKind::HASHFN),
    ("&", SyntaxKind::CAPTURE),
    ("&as", SyntaxKind::CAPTURE),
    ("->", SyntaxKind::THREAD),
    ("->>", SyntaxKind::THREAD),
    ("-?>", SyntaxKind::THREAD),
    ("-?>>", SyntaxKind::THREAD),
    ("...", SyntaxKind::VARARG),
    ("?", SyntaxKind::QUESTION),
    ("fn", SyntaxKind::KEYWORD_FN),
    ("lambda", SyntaxKind::KEYWORD_LAMBDA),
    ("λ", SyntaxKind::KEYWORD_LAMBDA),
    ("local", SyntaxKind::KEYWORD_LOCAL),
    ("let", SyntaxKind::KEYWORD_LET),
    ("set", SyntaxKind::KEYWORD_SET),
    ("include", SyntaxKind::KEYWORD_INCLUDE),
    ("accumulate", SyntaxKind::KEYWORD_ACCUMULATE),
    ("catch", SyntaxKind::KEYWORD_CATCH),
    ("collect", SyntaxKind::KEYWORD_COLLECT),
    ("do", SyntaxKind::KEYWORD_DO),
    ("doto", SyntaxKind::KEYWORD_DOTO),
    ("each", SyntaxKind::KEYWORD_EACH),
    ("eval-compiler", SyntaxKind::KEYWORD_EVAL_COMPILER),
    ("for", SyntaxKind::KEYWORD_FOR),
    ("global", SyntaxKind::KEYWORD_GLOBAL),
    ("icollect", SyntaxKind::KEYWORD_ICOLLECT),
    ("fcollect", SyntaxKind::KEYWORD_FCOLLECT),
    ("if", SyntaxKind::KEYWORD_IF),
    ("import-macros", SyntaxKind::KEYWORD_IMPORT_MACROS),
    (":into", SyntaxKind::KEYWORD_INTO),
    ("&into", SyntaxKind::KEYWORD_INTO),
    ("lua", SyntaxKind::KEYWORD_LUA),
    ("macro", SyntaxKind::KEYWORD_MACRO),
    ("macrodebug", SyntaxKind::KEYWORD_MACRODEBUG),
    ("macros", SyntaxKind::KEYWORD_MACROS),
    ("match", SyntaxKind::KEYWORD_MATCH),
    ("match-try", SyntaxKind::KEYWORD_MATCH_TRY),
    ("partial", SyntaxKind::KEYWORD_PARTIAL),
    ("pick-args", SyntaxKind::KEYWORD_PICK_ARGS),
    ("values", SyntaxKind::KEYWORD_VALUES),
    ("pick-values", SyntaxKind::KEYWORD_PICK_VALUES),
    ("require-macros", SyntaxKind::KEYWORD_REQUIRE_MACROS),
    ("tset", SyntaxKind::KEYWORD_TSET),
    (":until", SyntaxKind::KEYWORD_UNTIL),
    ("&until", SyntaxKind::KEYWORD_UNTIL),
    ("var", SyntaxKind::KEYWORD_VAR),
    ("when", SyntaxKind::KEYWORD_WHEN),
    ("where", SyntaxKind::KEYWORD_WHERE),
    ("while", SyntaxKind::KEYWORD_WHILE),
    ("with-open", SyntaxKind::KEYWORD_WITH_OPEN),
    ("comment", SyntaxKind::KEYWORD_COMMENT),
];

impl From<u16> for SyntaxKind {
    #[inline]
    fn from(d: u16) -> Self {
        assert!(d <= (Self::ROOT as u16));
        unsafe { std::mem::transmute::<u16, Self>(d) }
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
