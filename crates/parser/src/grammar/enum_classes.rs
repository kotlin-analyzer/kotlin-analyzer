use crate::{SyntaxKind::*, T};

use super::class_members::class_member_declarations;
use super::classes::class_body;
use super::expressions::value_arguments;
use super::identifiers::simple_identifier;
use super::modifiers::modifiers;
use crate::{CompletedMarker, Marker, Parser};

pub(crate) enum BodyResult {
    None { opening_brace: Marker, first_entry: Marker },
    EnumClassBody(CompletedMarker),
}

pub(crate) fn enum_class_body(
    parser: &mut Parser<'_>,
    opening_brace: Marker,
    first_entry: Marker,
) -> BodyResult {
    let m = opening_brace;

    if let Err(fm) = enum_entries(parser, Some(first_entry)) {
        return BodyResult::None { opening_brace: m, first_entry: fm };
    }

    if !parser.at(T!['}']) {
        if !parser.eat(T![;]) {
            parser.error("expected ';' after enum variants");
        }
        class_member_declarations(parser, None);
    }

    if !parser.eat(T!['}']) {
        parser.error("expected '}'");
    }
    BodyResult::EnumClassBody(m.complete(parser, ENUM_CLASS_BODY))
}

fn enum_entries(
    parser: &mut Parser<'_>,
    first_entry: Option<Marker>,
) -> Result<CompletedMarker, Marker> {
    match enum_entry(parser, first_entry) {
        Ok(cm) => {
            let m = cm.precede(parser);
            while parser.eat(T![,]) {
                if enum_entry(parser, None).is_err() {
                    break;
                }
            }
            Ok(m.complete(parser, ENUM_ENTRIES))
        }
        Err(m) => Err(m),
    }
}
fn enum_entry(parser: &mut Parser<'_>, start: Option<Marker>) -> Result<CompletedMarker, Marker> {
    let has_modifiers = start.is_some();
    let m = start.unwrap_or_else(|| parser.start());

    if !has_modifiers {
        modifiers(parser);
    }
    if simple_identifier(parser).is_none() {
        return Err(m);
    }
    value_arguments(parser);
    class_body(parser, None, None);
    Ok(m.complete(parser, ENUM_ENTRY))
}
