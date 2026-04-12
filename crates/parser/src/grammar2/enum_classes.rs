use syntax::{SyntaxKind::*, T};

use crate::{
    grammar2::{
        class_members::class_member_declarations, classes::class_body,
        expressions::value_arguments, identifiers::simple_identifier, modifiers::modifiers,
    },
    ra::{CompletedMarker, Marker, Parser},
};

pub(crate) enum BodyResult {
    None(Marker),
    EnumClassBody(CompletedMarker),
}

pub(crate) fn enum_class_body(
    parser: &mut Parser<'_>,
    opening_brace: Marker,
    modifiers_marker: Option<CompletedMarker>,
) -> BodyResult {
    let m = opening_brace;

    if enum_entries(parser, modifiers_marker.clone()).is_none() {
        return BodyResult::None(m); // so classBody can handle it
    }

    if !parser.at(T!['}']) {
        if !parser.eat(T![;]) {
            parser.error("expected ';' after enum variants");
        }
        class_member_declarations(parser, modifiers_marker);
    }

    if !parser.eat(T!['}']) {
        parser.error("expected '}'");
    }
    BodyResult::EnumClassBody(m.complete(parser, ENUM_CLASS_BODY))
}

fn enum_entries(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if let Some(cm) = enum_entry(parser, modifiers_marker) {
        let m = cm.precede(parser);
        while parser.eat(T![,]) {
            if enum_entry(parser, None).is_none() {
                break;
            }
        }
        Some(m.complete(parser, ENUM_ENTRIES))
    } else {
        None
    }
}
fn enum_entry(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    let has_modifiers = modifiers_marker.is_some();
    let m = modifiers_marker
        .map(|cm| cm.precede(parser))
        .unwrap_or_else(|| parser.start());

    if !has_modifiers {
        modifiers(parser);
    }
    if simple_identifier(parser).is_none() {
        m.abandon(parser);
        return None;
    }
    value_arguments(parser);
    class_body(parser, None, None);
    Some(m.complete(parser, ENUM_ENTRY))
}
