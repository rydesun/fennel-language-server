use std::{collections::HashMap, ops::Range};

use once_cell::sync::Lazy;
use regex::Regex;

use crate::syntax::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Token {
    pub(crate) kind: SyntaxKind,
    pub(crate) text: String,
    pub(crate) range: Range<usize>,
}

impl Token {
    #[allow(unused)]
    pub(crate) fn new(
        kind: SyntaxKind,
        text: &str,
        range: Range<usize>,
    ) -> Self {
        Self { kind, text: text.to_string(), range }
    }
}

pub(crate) struct Lexer<'a> {
    source: Box<dyn Iterator<Item = char> + 'a>,
    offset: usize,

    current: String,
    peek: Option<char>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(
        mut source: Box<dyn Iterator<Item = char> + 'a>,
    ) -> Self {
        let peek = source.next();

        Self { source, offset: 0, current: String::new(), peek }
    }
}

static TABLE: Lazy<HashMap<String, SyntaxKind>> = Lazy::new(|| {
    let mut table: HashMap<String, SyntaxKind> = HashMap::new();
    crate::syntax::TOEKN.iter().for_each(|(t, s)| {
        table.insert(t.to_string(), *s);
    });
    table
});

static RE_FLOAT: Lazy<Regex> = Lazy::new(|| {
    Regex::new(&format!(
        "^({}|{}|{})({})?$",
        r"[+-]?(0x)?\.[0-9][0-9_]*",
        r"[+-]?[0-9]+[0-9_]*\.?[0-9_]*",
        r"[+-]?0x[0-9a-f]+[0-9a-f_]*\.?[0-9_]*",
        r"[eE][+-]?[0-9_]+"
    ))
    .unwrap()
});

const BOUND: [char; 8] = ['(', ')', '[', ']', '{', '}', ',', '`'];

static RE_INTEGER: Lazy<Regex> = Lazy::new(|| {
    Regex::new(&format!("^{}$", r#"[+-]?(0x)?[0-9][0-9_]*"#)).unwrap()
});

static RE_COLON_STRING: Lazy<Regex> =
    Lazy::new(|| Regex::new(&format!("^{}$", r#":[^"'~;@]+"#)).unwrap());

static RE_SYMBOL: Lazy<Regex> = Lazy::new(|| {
    Regex::new(&format!("^{}$", r#"[^.:#"'~;,@`&][^"'~;,@`&]*"#)).unwrap()
});

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.is_empty() {
            if let Some(p) = self.peek.take() {
                self.current.push(p);
            } else if let Some(next) = self.source.next() {
                self.current.push(next);
            } else {
                return Some(Token {
                    kind: SyntaxKind::END,
                    text: "".to_string(),
                    range: self.offset..self.offset,
                });
            }
        }

        let head = self.current.chars().next().unwrap();
        let head_kind = kind_by_char(head);
        let token_kind = match head_kind {
            TokenKind::Word => {
                for c in self.source.by_ref() {
                    let cur_kind = kind_by_char(c);
                    if head_kind == cur_kind || cur_kind == TokenKind::Hash {
                        self.current.push(c);
                    } else {
                        self.peek = Some(c);
                        break;
                    }
                }
                if let Some(kind) = TABLE.get(&self.current) {
                    *kind
                } else if RE_INTEGER.is_match(&self.current) {
                    SyntaxKind::INTEGER
                } else if RE_FLOAT.is_match(&self.current) {
                    SyntaxKind::FLOAT
                } else if RE_SYMBOL.is_match(&self.current) {
                    SyntaxKind::SYMBOL
                } else if RE_COLON_STRING.is_match(&self.current) {
                    SyntaxKind::COLON_STRING
                } else {
                    SyntaxKind::ERROR
                }
            }
            TokenKind::Hash => {
                self.peek = self.source.next();
                if self
                    .peek
                    .map(|p| p.is_whitespace() || p == ')')
                    .unwrap_or(true)
                {
                    SyntaxKind::LENGTH
                } else {
                    SyntaxKind::HASHFN
                }
            }
            TokenKind::QuoteString => {
                let mut escaped = false;
                let mut found = false;

                for c in self.source.by_ref() {
                    self.current.push(c);
                    if c == '\\' {
                        escaped = !escaped;
                        continue;
                    }
                    if c == '"' && !escaped {
                        found = true;
                        break;
                    }
                    escaped = false;
                }
                if found {
                    SyntaxKind::QUOTE_STRING
                } else {
                    SyntaxKind::ERROR
                }
            }
            TokenKind::Comment => {
                while let Some(c) = self.source.next() {
                    self.current.push(c);
                    if c == '\r' {
                        if let Some(c) = self.source.next() {
                            if c == '\n' {
                                self.current.push(c);
                            } else {
                                self.peek = Some(c)
                            }
                        }
                        break;
                    } else if c == '\n' {
                        break;
                    }
                }
                SyntaxKind::COMMENT
            }
            TokenKind::Whitespace => {
                for c in self.source.by_ref() {
                    if head_kind == kind_by_char(c) {
                        self.current.push(c);
                    } else {
                        self.peek = Some(c);
                        break;
                    }
                }
                SyntaxKind::WHITESPACE
            }
            TokenKind::Bound => match self.current.as_str() {
                "(" => SyntaxKind::L_PAREN,
                ")" => SyntaxKind::R_PAREN,
                "[" => SyntaxKind::L_BRACKET,
                "]" => SyntaxKind::R_BRACKET,
                "{" => SyntaxKind::L_BRACE,
                "}" => SyntaxKind::R_BRACE,
                "`" => SyntaxKind::BACKTICK,
                "," => SyntaxKind::COMMA,
                _ => SyntaxKind::ERROR,
            },
        };

        let text = std::mem::take(&mut self.current);
        let start = self.offset;
        self.offset += text.len();

        Some(Token { kind: token_kind, text, range: start..self.offset })
    }
}

pub(crate) fn validata_symbol(symbol: &str) -> bool {
    let mut lexer = Lexer::new(Box::new(symbol.chars()));
    if let Some(token) = lexer.next() {
        if token.kind != SyntaxKind::SYMBOL {
            return false;
        }
        for c in token.text.chars() {
            if c == '.' || c == ':' {
                return false;
            }
        }
        if lexer.next().is_some() {
            return false;
        }
        return !Vec::from(include!("static/reserved"))
            .contains(&token.text.as_str());
    }
    false
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TokenKind {
    Word,
    Whitespace,
    Comment,
    QuoteString,
    Bound,
    Hash,
}

fn kind_by_char(c: char) -> TokenKind {
    if c.is_whitespace() {
        TokenKind::Whitespace
    } else if c == ';' {
        TokenKind::Comment
    } else if c == '"' {
        TokenKind::QuoteString
    } else if c == '#' {
        TokenKind::Hash
    } else if BOUND.contains(&c) {
        TokenKind::Bound
    } else {
        TokenKind::Word
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_lex(text: &str, tokens: &[Token]) {
        let mut lex = Lexer::new(Box::new(text.chars()));
        tokens.iter().for_each(|token| {
            let n = match lex.next() {
                Some(n) if n.kind == SyntaxKind::WHITESPACE => lex.next(),
                n => n,
            };
            assert_eq!(n, Some(token.to_owned()));
        });
        assert_eq!(lex.next().unwrap().kind, SyntaxKind::END);
    }

    #[test]
    fn symbol() {
        assert_lex(
            "?我❤️logos#$%^*-+=/|\\ local locals :into :intobreach logos:into \
             _",
            &[
                Token::new(
                    SyntaxKind::SYMBOL,
                    "?我❤️logos#$%^*-+=/|\\",
                    00..26,
                ),
                Token::new(SyntaxKind::KEYWORD_LOCAL, "local", 27..32),
                Token::new(SyntaxKind::SYMBOL, "locals", 33..39),
                Token::new(SyntaxKind::KEYWORD_INTO, ":into", 40..45),
                Token::new(SyntaxKind::COLON_STRING, ":intobreach", 46..57),
                Token::new(SyntaxKind::SYMBOL, "logos:into", 58..68),
                Token::new(SyntaxKind::SYMBOL, "_", 69..70),
            ],
        );
    }

    #[test]
    fn number() {
        assert_lex("+123 1.23 -1_._2__3E2_ .1", &[
            Token::new(SyntaxKind::INTEGER, "+123", 0..4),
            Token::new(SyntaxKind::FLOAT, "1.23", 5..9),
            Token::new(SyntaxKind::FLOAT, "-1_._2__3E2_", 10..22),
            Token::new(SyntaxKind::FLOAT, ".1", 23..25),
        ]);
    }

    #[ignore = "FIXME"]
    #[test]
    fn _number() {
        assert_lex("+_1.", &[Token::new(SyntaxKind::FLOAT, "+_1.", 0..4)]);
    }

    #[test]
    fn string() {
        assert_lex(
            r#" "logos\"" "multi
                          lines" :https://github.com/maciejhirsz/logos"#,
            &[
                Token::new(SyntaxKind::QUOTE_STRING, r#""logos\"""#, 1..10),
                Token::new(
                    SyntaxKind::QUOTE_STRING,
                    "\"multi
                          lines\"",
                    11..50,
                ),
                Token::new(
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
            Token::new(SyntaxKind::L_PAREN, "(", 0..1),
            Token::new(SyntaxKind::THREAD, "->", 1..3),
            Token::new(SyntaxKind::BOOL, "false", 4..9),
            Token::new(SyntaxKind::L_PAREN, "(", 10..11),
            Token::new(SyntaxKind::OPERATOR, "not", 11..14),
            Token::new(SyntaxKind::R_PAREN, ")", 14..15),
            Token::new(SyntaxKind::L_PAREN, "(", 16..17),
            Token::new(SyntaxKind::HASHFN, "#", 17..18),
            Token::new(SyntaxKind::SYMBOL, "$", 18..19),
            Token::new(SyntaxKind::R_PAREN, ")", 19..20),
            Token::new(SyntaxKind::L_PAREN, "(", 21..22),
            Token::new(SyntaxKind::L_PAREN, "(", 22..23),
            Token::new(SyntaxKind::KEYWORD_FN, "fn", 23..25),
            Token::new(SyntaxKind::L_BRACKET, "[", 26..27),
            Token::new(SyntaxKind::SYMBOL, "x", 27..28),
            Token::new(SyntaxKind::R_BRACKET, "]", 28..29),
            Token::new(SyntaxKind::L_PAREN, "(", 30..31),
            Token::new(SyntaxKind::LENGTH, "#", 31..32),
            Token::new(SyntaxKind::SYMBOL, "x", 33..34),
            Token::new(SyntaxKind::R_PAREN, ")", 34..35),
            Token::new(SyntaxKind::R_PAREN, ")", 35..36),
            Token::new(SyntaxKind::R_PAREN, ")", 36..37),
            Token::new(SyntaxKind::R_PAREN, ")", 37..38),
        ]);
    }

    #[test]
    fn comment() {
        assert_lex("(print :logos; this is comment)\n\n)", &[
            Token::new(SyntaxKind::L_PAREN, "(", 0..1),
            Token::new(SyntaxKind::SYMBOL, "print", 1..6),
            Token::new(SyntaxKind::COLON_STRING, ":logos", 7..13),
            Token::new(SyntaxKind::COMMENT, "; this is comment)\n", 13..32),
            Token::new(SyntaxKind::R_PAREN, ")", 33..34),
        ]);
    }
}
