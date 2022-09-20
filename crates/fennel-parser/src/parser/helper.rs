use rowan::{TextRange, TextSize};

pub(crate) fn text_range(range: &std::ops::Range<usize>) -> TextRange {
    TextRange::new(
        TextSize::from(range.start as u32),
        TextSize::from(range.end as u32),
    )
}

pub(crate) fn text_range_with_offset(
    range: &std::ops::Range<usize>,
    offset: (i32, i32),
) -> TextRange {
    TextRange::new(
        TextSize::from((range.start as i32 + offset.0) as u32),
        TextSize::from((range.end as i32 + offset.1) as u32),
    )
}

macro_rules! expand_rules {
    (
        $self:ident, $callback:ident,
        ($cur_rule:ident, $cur_token:ident),
        $({$node:ident ::= ($kind:ident $($notation:tt)*)
        $(, $rest_rules:tt )* },)*
    ) => {
        match $cur_rule.expect {
            $( $node if crate::parser::sets::first_set(
                    $kind, $cur_token.kind) => {
                $self.$callback(
                    $cur_token,
                    vec![
                        notation!(($kind $($notation)*)),
                        $(notation!($rest_rules)),*
                    ].into_iter());
                return;
            }, )*
            _ => {},
        }
    };
}
