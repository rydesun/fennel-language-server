use fennel_parser::{models, ErrorKind};
use tower_lsp::lsp_types::CompletionItemKind;

pub(crate) fn scope_kind(kind: models::ScopeKind) -> &'static str {
    match kind {
        models::ScopeKind::Param => "[function parameter]",
        models::ScopeKind::MacroParam => "[macro parameter]",
        models::ScopeKind::Global => "global",
        models::ScopeKind::Let => "let",
        models::ScopeKind::WithOpen => "with-open",
        models::ScopeKind::Local => "local",
        models::ScopeKind::Macro => "macro",
        models::ScopeKind::Func => "function",
        models::ScopeKind::Lambda => "lambda",
        models::ScopeKind::Var => "var",
        models::ScopeKind::Match => "<match pattern>",
        models::ScopeKind::MatchTry => "<match-try pattern>",
        models::ScopeKind::Catch => "<match-try-catch pattern>",
        models::ScopeKind::IterValue => "<iteration pattern>",
        models::ScopeKind::AccuValue => "<accumulation value>",
    }
}

pub(crate) fn value_kind(kind: models::ValueKind) -> &'static str {
    match kind {
        models::ValueKind::Nil => "nil",
        models::ValueKind::Number => "number",
        models::ValueKind::Module => "module",
        models::ValueKind::String => "string",
        models::ValueKind::Bool => "bool",
        models::ValueKind::SeqTable => "[...]",
        models::ValueKind::KvTable => "{...}",
        models::ValueKind::FileHandle => "<file handle>",
        models::ValueKind::Func
        | models::ValueKind::Macro
        | models::ValueKind::Param
        | models::ValueKind::MacroParam
        | models::ValueKind::Match
        | models::ValueKind::Unknown => "",
        models::ValueKind::Symbol => "unknown",
    }
}

pub(crate) fn completion_scope_kind(
    kind: models::ScopeKind,
) -> CompletionItemKind {
    match kind {
        models::ScopeKind::Local
        | models::ScopeKind::Let
        | models::ScopeKind::IterValue
        | models::ScopeKind::AccuValue
        | models::ScopeKind::Param
        | models::ScopeKind::Global
        | models::ScopeKind::WithOpen
        | models::ScopeKind::Macro
        | models::ScopeKind::MacroParam
        | models::ScopeKind::Match
        | models::ScopeKind::MatchTry
        | models::ScopeKind::Catch
        | models::ScopeKind::Var => CompletionItemKind::VARIABLE,
        models::ScopeKind::Func | models::ScopeKind::Lambda => {
            CompletionItemKind::FUNCTION
        }
    }
}

pub(crate) fn completion_value_kind(
    kind: models::CompletionKind,
) -> CompletionItemKind {
    match kind {
        models::CompletionKind::Func => CompletionItemKind::FUNCTION,
        models::CompletionKind::Module => CompletionItemKind::MODULE,
        models::CompletionKind::Field => CompletionItemKind::FIELD,
        models::CompletionKind::Keyword => CompletionItemKind::KEYWORD,
        models::CompletionKind::Operator => CompletionItemKind::OPERATOR,
        models::CompletionKind::Var => CompletionItemKind::VARIABLE,
    }
}

pub(crate) fn error(e: ErrorKind) -> String {
    match e {
        ErrorKind::Unterminated(kind) => format!("Incomplete {}", kind),
        ErrorKind::Unexpected(kind) => {
            format!("Unexpected {} in input", kind)
        }
        ErrorKind::UnexpectedEof => {
            "Unexpected end of file. Expected closing delimiter".into()
        }
        ErrorKind::EmptyList => "Found empty list".into(),
        ErrorKind::DirectCall(kind) => {
            format!("Cannot directly call value {}", kind)
        }
        ErrorKind::LiteralCall(kind) => {
            format!("Cannot call literal value {}", kind)
        }
        ErrorKind::GlobalConflict => "Global conflicts with local".into(),
        ErrorKind::Dismatched => "Closing delimiter is missing".into(),
        ErrorKind::Undefined => "Undefined identifier".into(),
        ErrorKind::MissingWhitespace => {
            "Expected whitespace before opening delimiter".into()
        }
        ErrorKind::MacroWhitespace => "Invalid macro".into(),

        ErrorKind::InvalidSymbol => "Invalid symbol".into(),

        ErrorKind::MethodNotAllowed => "Unexpected symbol method".into(),

        ErrorKind::FieldAndMethodNotAllowed => {
            "Unexpected multi symbol".into()
        }
        ErrorKind::UnexpectedVarargs => {
            "Varargs not found in parameter table".into()
        }
        ErrorKind::MultiCatch => "Only one catch clause permitted".into(),
        ErrorKind::CatchNotLast => "Catch clause must be at the end".into(),
    }
}
