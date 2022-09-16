macro_rules! ast_node {
    ($ast:ident, $syntax:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub(crate) struct $ast(pub(crate) SyntaxNode);

        impl rowan::ast::AstNode for $ast {
            type Language = crate::FennelLanguage;

            fn cast(syntax_node: SyntaxNode) -> Option<Self> {
                syntax_node
                    .kind()
                    .eq(&SyntaxKind::$syntax)
                    .then(|| Self(syntax_node))
            }

            fn can_cast(syntax: SyntaxKind) -> bool {
                syntax == SyntaxKind::$syntax
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    };
}

macro_rules! ast_assoc {
    ( $name:ident, [$($item:ident),* $(,)?] ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub(crate) enum $name {
            $($item($item),)*
        }
        impl rowan::ast::AstNode for $name {
            type Language = crate::FennelLanguage;
            fn cast(syntax_node: SyntaxNode) -> Option<Self> {
                $(
                    if let Some(i) = $item::cast(syntax_node.clone()) {
                        return Some(Self::$item(i));
                    }
                )*
                None
            }
            fn can_cast(syntax: SyntaxKind) -> bool {
                $(
                    if $item::can_cast(syntax) {
                        return true;
                    }
                )*
                false
            }
            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$item(n) => n.syntax(),)*
                }
            }
        }
    }
}

pub(crate) use ast_assoc;
