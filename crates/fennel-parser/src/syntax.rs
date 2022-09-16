use std::fmt;

use logos::{Lexer, Logos};

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
pub enum SyntaxKind {
    #[regex(
        r#"[^.:#(){}\[\]"'~;,@`&\s][^(){}\[\]"'~;,@`&\s]*"#,
        priority = 1
    )]
    SYMBOL,

    SYMBOL_FIELD,
    SYMBOL_METHOD,

    #[regex(r"([+-]?(0x)?\.[0-9][0-9_]*|[+-]?[0-9]+[0-9_]*\.?[0-9_]*|[+-]?0x[0-9a-f]+[0-9a-f_]*\.?[0-9_]*)([eE][+-]?[0-9_]+)?",
        priority = 4)]
    FLOAT,

    #[regex(r"[+-]?(0x)?[0-9][0-9_]*", priority = 5)]
    INTEGER,

    #[token("true")]
    #[token("false")]
    BOOL,

    #[token("nil")]
    NIL,

    #[token("\"", lex_quote_string)]
    QUOTE_STRING,

    #[regex(r#":[^(){}\[\]"'~;,@`\s]+"#)]
    COLON_STRING,

    #[token("or")]
    KEYWORD_OR,

    #[token("and")]
    #[token("+")]
    #[token("-")]
    #[token("*")]
    #[token("/")]
    #[token("//")]
    #[token("%")]
    #[token("^")]
    #[token(">")]
    #[token("<")]
    #[token(">=")]
    #[token("<=")]
    #[token("~=")]
    #[token("=")]
    #[token("not=")]
    #[token("lshift")]
    #[token("rshift")]
    #[token("band")]
    #[token("bor")]
    #[token("bxor")]
    #[token("..")]
    #[token("not")]
    #[token("bnot")]
    #[token("length")]
    #[token(".")]
    #[token("?.")]
    OPERATOR,

    #[token(":")]
    COLON,

    #[token("#")]
    HASHFN,

    #[token("&")]
    #[token("&as")]
    CAPTURE,

    // TODO: not match whitespace
    // FIXME: (#)
    #[regex(r"#\s+")]
    LENGTH,

    #[token("->")]
    #[token("->>")]
    #[token("-?>")]
    #[token("-?>>")]
    THREAD,

    #[token("(")]
    L_PAREN,

    #[token(")")]
    R_PAREN,

    #[token("[")]
    L_BRACKET,

    #[token("]")]
    R_BRACKET,

    #[token("{")]
    L_BRACE,

    #[token("}")]
    R_BRACE,

    #[token(",")]
    COMMA,

    #[token("...")]
    VARARG,

    #[token("?")]
    QUESTION,

    #[token("fn")]
    KEYWORD_FN,

    #[token("lambda")]
    #[token("λ")]
    KEYWORD_LAMBDA,

    #[token("local")]
    KEYWORD_LOCAL,

    #[token("let")]
    KEYWORD_LET,

    #[token("set")]
    KEYWORD_SET,

    #[token("include")]
    KEYWORD_INCLUDE,

    #[token("accumulate")]
    KEYWORD_ACCUMULATE,

    #[token("`")]
    BACKTICK,

    #[token("catch")]
    KEYWORD_CATCH,

    #[token("collect")]
    KEYWORD_COLLECT,

    #[token("do")]
    KEYWORD_DO,

    #[token("doto")]
    KEYWORD_DOTO,

    #[token("each")]
    KEYWORD_EACH,

    #[token("eval-compiler")]
    KEYWORD_EVAL_COMPILER,

    #[token("for")]
    KEYWORD_FOR,

    #[token("global")]
    KEYWORD_GLOBAL,

    #[token("icollect")]
    KEYWORD_ICOLLECT,

    #[token("if")]
    KEYWORD_IF,

    #[token("import-macros")]
    KEYWORD_IMPORT_MACROS,

    #[token(":into")]
    #[token("&into")]
    KEYWORD_INTO,

    #[token("lua")]
    KEYWORD_LUA,

    #[token("macro")]
    KEYWORD_MACRO,

    #[token("macrodebug")]
    KEYWORD_MACRODEBUG,

    #[token("macros")]
    KEYWORD_MACROS,

    #[token("match")]
    KEYWORD_MATCH,

    #[token("match-try")]
    KEYWORD_MATCH_TRY,

    #[token("partial")]
    KEYWORD_PARTIAL,

    #[token("pick-args")]
    KEYWORD_PICK_ARGS,

    #[token("values")]
    KEYWORD_VALUES,

    #[token("pick-values")]
    KEYWORD_PICK_VALUES,

    #[token("require-macros")]
    KEYWORD_REQUIRE_MACROS,

    #[token("tset")]
    KEYWORD_TSET,

    #[token(":until")]
    #[token("&until")]
    KEYWORD_UNTIL,

    #[token("var")]
    KEYWORD_VAR,

    #[token("when")]
    KEYWORD_WHEN,

    #[token("where")]
    KEYWORD_WHERE,

    #[token("while")]
    KEYWORD_WHILE,

    #[token("with-open")]
    KEYWORD_WITH_OPEN,

    #[regex(r";[^\n\r]*(\r\n?|\n)")]
    COMMENT,

    #[regex(r"\s+")]
    WHITESPACE,

    #[error]
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

fn lex_quote_string(lex: &mut Lexer<SyntaxKind>) -> bool {
    let remainder: &str = lex.remainder();
    let mut total_len = 0;
    let mut escaped = false;

    for c in remainder.chars() {
        total_len += c.len_utf8();
        if c == '\\' {
            escaped = !escaped;
            continue;
        }
        if c == '"' && !escaped {
            lex.bump(remainder[0..total_len].as_bytes().len());
            return true;
        }
        escaped = false;
    }
    false
}

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

pub(crate) fn validata_symbol(symbol: &str) -> bool {
    let mut lexer = SyntaxKind::lexer(symbol);
    if let Some(token) = lexer.next() {
        if token != SyntaxKind::SYMBOL {
            return false;
        }
        let text = lexer.slice();
        for c in text.chars() {
            if c == '.' || c == ':' {
                return false;
            }
        }
        if lexer.next().is_some() {
            return false;
        }
        return !Vec::from(include!("static/reserved")).contains(&text);
    }
    false
}

#[cfg(test)]
mod tests {
    use core::ops::Range;

    use super::*;

    fn assert_lex(text: &str, tokens: &[(SyntaxKind, &str, Range<usize>)]) {
        let mut lex = SyntaxKind::lexer(text);
        tokens.iter().for_each(|token| {
            let mut n = lex.next();
            if n == Some(SyntaxKind::WHITESPACE) {
                n = lex.next();
            }
            assert_eq!(n, Some(token.0));
            assert_eq!(lex.slice(), token.1);
            assert_eq!(lex.span(), token.2);
        });
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn symbol() {
        assert_lex(
            "?我❤️logos#$%^*-+=/|\\ local locals :into :intobreach logos:into \
             _",
            &[
                (SyntaxKind::SYMBOL, "?我❤️logos#$%^*-+=/|\\", 00..26),
                (SyntaxKind::KEYWORD_LOCAL, "local", 27..32),
                (SyntaxKind::SYMBOL, "locals", 33..39),
                (SyntaxKind::KEYWORD_INTO, ":into", 40..45),
                (SyntaxKind::COLON_STRING, ":intobreach", 46..57),
                (SyntaxKind::SYMBOL, "logos:into", 58..68),
                (SyntaxKind::SYMBOL, "_", 69..70),
            ],
        );
    }

    #[test]
    fn number() {
        assert_lex("+123 1.23 -1_._2__3E2_ .1", &[
            (SyntaxKind::INTEGER, "+123", 0..4),
            (SyntaxKind::FLOAT, "1.23", 5..9),
            (SyntaxKind::FLOAT, "-1_._2__3E2_", 10..22),
            (SyntaxKind::FLOAT, ".1", 23..25),
        ]);
    }

    #[ignore = "FIXME"]
    #[test]
    fn _number() {
        assert_lex("+_1.", &[(SyntaxKind::FLOAT, "+_1.", 0..4)]);
    }

    #[test]
    fn string() {
        assert_lex(
            r#" "logos\"" "multi
                          lines" :https://github.com/maciejhirsz/logos"#,
            &[
                (SyntaxKind::QUOTE_STRING, r#""logos\"""#, 1..10),
                (
                    SyntaxKind::QUOTE_STRING,
                    "\"multi
                          lines\"",
                    11..50,
                ),
                (
                    SyntaxKind::COLON_STRING,
                    ":https://github.com/maciejhirsz/logos",
                    51..88,
                ),
            ],
        );
    }

    #[test]
    fn list() {
        assert_lex("(-> false (not) (#$) ((fn [x] (# x))))", &[
            (SyntaxKind::L_PAREN, "(", 0..1),
            (SyntaxKind::THREAD, "->", 1..3),
            (SyntaxKind::BOOL, "false", 4..9),
            (SyntaxKind::L_PAREN, "(", 10..11),
            (SyntaxKind::OPERATOR, "not", 11..14),
            (SyntaxKind::R_PAREN, ")", 14..15),
            (SyntaxKind::L_PAREN, "(", 16..17),
            (SyntaxKind::HASHFN, "#", 17..18),
            (SyntaxKind::SYMBOL, "$", 18..19),
            (SyntaxKind::R_PAREN, ")", 19..20),
            (SyntaxKind::L_PAREN, "(", 21..22),
            (SyntaxKind::L_PAREN, "(", 22..23),
            (SyntaxKind::KEYWORD_FN, "fn", 23..25),
            (SyntaxKind::L_BRACKET, "[", 26..27),
            (SyntaxKind::SYMBOL, "x", 27..28),
            (SyntaxKind::R_BRACKET, "]", 28..29),
            (SyntaxKind::L_PAREN, "(", 30..31),
            (SyntaxKind::LENGTH, "# ", 31..33),
            (SyntaxKind::SYMBOL, "x", 33..34),
            (SyntaxKind::R_PAREN, ")", 34..35),
            (SyntaxKind::R_PAREN, ")", 35..36),
            (SyntaxKind::R_PAREN, ")", 36..37),
            (SyntaxKind::R_PAREN, ")", 37..38),
        ]);
    }

    #[test]
    fn comment() {
        assert_lex("(print :logos; this is comment)\n\n)", &[
            (SyntaxKind::L_PAREN, "(", 0..1),
            (SyntaxKind::SYMBOL, "print", 1..6),
            (SyntaxKind::COLON_STRING, ":logos", 7..13),
            (SyntaxKind::COMMENT, "; this is comment)\n", 13..32),
            (SyntaxKind::R_PAREN, ")", 33..34),
        ]);
    }
}
