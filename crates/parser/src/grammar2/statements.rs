use syntax::{SyntaxKind::*, T};

use crate::{
    grammar2::identifiers::is_simple_identifier,
    ra::{CompletedMarker, Parser},
};

pub(crate) fn semi(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![;]) {
        let m = parser.start();
        parser.eat(T![;]);
        Some(m.complete(parser, SEMI))
    } else {
        None
    }
}

pub(crate) fn semis(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    let mut found = false;
    while parser.eat(T![;]) {
        found = true;
    }
    if found {
        Some(m.complete(parser, SEMIS))
    } else {
        m.abandon(parser);
        None
    }
}

pub(crate) fn statements(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    todo!()
}

pub(crate) fn label(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_simple_identifier(parser) && (parser.nth_at(1, T![@]) || parser.nth_at(1, AT_POST_WS)) {
        let m = parser.start();
        parser.bump(SIMPLE_IDENTIFIER);
        parser.bump_any(); // either T![@] or AT_POST_WS
        Some(m.complete(parser, LABEL))
    } else {
        None
    }
}

pub(crate) fn control_structure_body(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    block(parser)
        .or_else(|| statements(parser))
        .map(|cm| cm.precede(parser).complete(parser, CONTROL_STRUCTURE_BODY))
}

pub(crate) fn block(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T!['{']) {
        let m = parser.start();
        parser.eat(T!['{']);

        while statements(parser).is_some() {}

        if !parser.eat(T!['}']) {
            parser.error("expected '}'");
        }
        Some(m.complete(parser, BLOCK))
    } else {
        None
    }
}
