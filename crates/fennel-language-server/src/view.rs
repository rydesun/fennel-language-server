use fennel_parser::{models, ErrorKind};
use tower_lsp::lsp_types::{CompletionItemKind, DiagnosticSeverity};

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

pub(crate) fn value_kind(kind: &models::ValueKind) -> &'static str {
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
        | models::ValueKind::Require(_)
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

pub(crate) fn error(e: ErrorKind) -> (String, DiagnosticSeverity) {
    match e {
        ErrorKind::Unterminated(kind) => {
            (format!("Incomplete {}", kind), DiagnosticSeverity::ERROR)
        }
        ErrorKind::Unexpected(kind) => (
            format!("Unexpected {} in input", kind),
            DiagnosticSeverity::ERROR,
        ),
        ErrorKind::UnexpectedEof => (
            "Unexpected end of file. Expected closing delimiter".into(),
            DiagnosticSeverity::ERROR,
        ),
        ErrorKind::EmptyList => {
            ("Found empty list".into(), DiagnosticSeverity::ERROR)
        }
        ErrorKind::DirectCall(kind) => (
            format!("Cannot directly call value {}", kind),
            DiagnosticSeverity::ERROR,
        ),
        ErrorKind::LiteralCall(kind) => (
            format!("Cannot call literal value {}", kind),
            DiagnosticSeverity::ERROR,
        ),
        ErrorKind::GlobalConflict => {
            ("Global conflicts with local".into(), DiagnosticSeverity::ERROR)
        }
        ErrorKind::Dismatched => {
            ("Closing delimiter is missing".into(), DiagnosticSeverity::ERROR)
        }
        ErrorKind::Undefined => {
            ("Undefined identifier".into(), DiagnosticSeverity::ERROR)
        }
        ErrorKind::Unused => {
            ("Unused identifier".into(), DiagnosticSeverity::HINT)
        }
        ErrorKind::MissingWhitespace => (
            "Expected whitespace before opening delimiter".into(),
            DiagnosticSeverity::ERROR,
        ),
        ErrorKind::MacroWhitespace => {
            ("Invalid macro".into(), DiagnosticSeverity::ERROR)
        }

        ErrorKind::InvalidSymbol => {
            ("Invalid symbol".into(), DiagnosticSeverity::ERROR)
        }

        ErrorKind::MethodNotAllowed => {
            ("Unexpected symbol method".into(), DiagnosticSeverity::ERROR)
        }

        ErrorKind::FieldAndMethodNotAllowed => {
            ("Unexpected multi symbol".into(), DiagnosticSeverity::ERROR)
        }
        ErrorKind::MultiVarargs => (
            "Multiple varargs are not allowed".into(),
            DiagnosticSeverity::WARNING,
        ),
        ErrorKind::UnexpectedVarargs => (
            "Varargs not found in parameter table".into(),
            DiagnosticSeverity::ERROR,
        ),
        ErrorKind::MultiCatch => (
            "Only one catch clause permitted".into(),
            DiagnosticSeverity::ERROR,
        ),
        ErrorKind::CatchNotLast => (
            "Catch clause must be at the end".into(),
            DiagnosticSeverity::ERROR,
        ),
        ErrorKind::Deprecated(version, recommendation) => (
            format!(
                "Deprecated in version `{}`. Use `{}` instead",
                version, recommendation
            ),
            DiagnosticSeverity::HINT,
        ),
    }
}
