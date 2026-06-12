use crate::{SyntaxKind::*, T};

use super::class_members::class_member_declarations;
use super::classes::class_body;
use super::expressions::value_arguments;
use super::identifiers::simple_identifier;
use crate::{CompletedMarker, Marker, Parser};

pub(crate) enum BodyResult {
    None { opening_brace: Marker },
    EnumClassBody(CompletedMarker),
}

pub(crate) fn enum_class_body(parser: &mut Parser<'_>, opening_brace: Marker) -> BodyResult {
    let m = opening_brace;

    if enum_entries(parser).is_none() {
        return BodyResult::None { opening_brace: m };
    }

    if !parser.at(T!['}']) {
        if !parser.eat(T![;]) {
            parser.error("expected ';' after enum variants");
        }
        class_member_declarations(parser);
    }

    if !parser.eat(T!['}']) {
        parser.error("expected '}'");
    }
    BodyResult::EnumClassBody(m.complete(parser, ENUM_CLASS_BODY))
}

fn enum_entries(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = enum_entry(parser) {
        let m = cm.precede(parser);
        while parser.eat(T![,]) {
            if enum_entry(parser).is_none() {
                break;
            }
        }
        Some(m.complete(parser, ENUM_ENTRIES))
    } else {
        None
    }
}
fn enum_entry(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();

    if simple_identifier(parser).is_none() {
        m.abandon(parser);
        return None;
    }
    value_arguments(parser);
    class_body(parser, None);
    Some(m.complete(parser, ENUM_ENTRY))
}
