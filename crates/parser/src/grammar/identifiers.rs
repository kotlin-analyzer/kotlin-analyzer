use syntax::{SyntaxKind::*, T};

use crate::ra::{CompletedMarker, Parser};

pub(crate) fn simple_identifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_simple_identifier(parser) {
        let m = parser.start();
        parser.bump_any();
        Some(m.complete(parser, SIMPLE_IDENTIFIER))
    } else {
        None
    }
}

/// Returns true if the current token is an identifier or a soft keyword that can be used as an identifier.
pub(crate) fn is_simple_identifier(parser: &mut Parser<'_>) -> bool {
    is_simple_ident_at(parser, 0)
}

pub(crate) fn is_simple_ident_at(parser: &mut Parser<'_>, n: usize) -> bool {
    let current = parser.nth(n);
    matches!(
        (current.is_soft_keyword(), current),
        (true, _) | (_, IDENTIFIER_TOKEN)
    )
}

pub(crate) fn identifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();

    if simple_identifier(parser).is_none() {
        m.abandon(parser);
        return None;
    };
    while parser.at(T![.]) && is_simple_ident_at(parser, 1) {
        parser.eat(T![.]);
        simple_identifier(parser);
    }
    Some(m.complete(parser, IDENTIFIER))
}
