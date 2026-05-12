use syntax::SyntaxKind::*;

use super::annotations::annotation;
use super::class_members::context_parameter_list;
use crate::ra::{CompletedMarker, Parser};

pub(crate) fn modifiers(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = annotation(parser).or_else(|| modifier(parser)) {
        let m = cm.precede(parser);
        while annotation(parser).or_else(|| modifier(parser)).is_some() {}
        Some(m.complete(parser, MODIFIERS))
    } else {
        if let Some(cm) = context_parameter_list(parser) {
            let m = cm.precede(parser);
            while context_parameter_list(parser).is_some() {}
            Some(m.complete(parser, MODIFIERS))
        } else {
            None
        }
    }
}

pub(crate) fn parameter_modifiers(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = annotation(parser).or_else(|| parameter_modifier(parser)) {
        let m = cm.precede(parser);
        while annotation(parser)
            .or_else(|| parameter_modifier(parser))
            .is_some()
        {}
        Some(m.complete(parser, PARAMETER_MODIFIERS))
    } else {
        None
    }
}

pub(crate) fn type_parameter_modifiers(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = type_parameter_modifier(parser) {
        let m = cm.precede(parser);
        while type_parameter_modifier(parser).is_some() {}
        Some(m.complete(parser, TYPE_PARAMETER_MODIFIERS))
    } else {
        None
    }
}

pub(crate) fn modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    class_modifier(parser)
        .or_else(|| member_modifier(parser))
        .or_else(|| visibility_modifier(parser))
        .or_else(|| function_modifier(parser))
        .or_else(|| property_modifier(parser))
        .or_else(|| inheritance_modifier(parser))
        .or_else(|| parameter_modifier(parser))
        .or_else(|| platform_modifier(parser))
        .map(|cm| cm.precede(parser).complete(parser, MODIFIER))
}

fn type_parameter_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    reification_modifier(parser)
        .or_else(|| variance_modifier(parser))
        .or_else(|| annotation(parser))
        .map(|cm| cm.precede(parser).complete(parser, TYPE_PARAMETER_MODIFIER))
}

fn class_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        ENUM | SEALED | ANNOTATION | DATA | INNER | VALUE => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, CLASS_MODIFIER))
        }
        _ => None,
    }
}

fn member_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        OVERRIDE | LATEINIT => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, MEMBER_MODIFIER))
        }
        _ => None,
    }
}

fn visibility_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        PUBLIC | PRIVATE | INTERNAL | PROTECTED => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, VISIBILITY_MODIFIER))
        }
        _ => None,
    }
}

fn function_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        TAILREC | OPERATOR | INFIX | INLINE | EXTERNAL | SUSPEND => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, FUNCTION_MODIFIER))
        }
        _ => None,
    }
}

fn property_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(CONST) {
        let m = parser.start();
        parser.bump(CONST);
        Some(m.complete(parser, PROPERTY_MODIFIER))
    } else {
        None
    }
}

fn inheritance_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        ABSTRACT | FINAL | OPEN => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, INHERITANCE_MODIFIER))
        }
        _ => None,
    }
}

fn parameter_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        VAR_ARG | NO_INLINE | CROSS_INLINE => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, PARAMETER_MODIFIER))
        }
        _ => None,
    }
}

fn reification_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(REIFIED) {
        let m = parser.start();
        parser.bump(REIFIED);
        Some(m.complete(parser, REIFICATION_MODIFIER))
    } else {
        None
    }
}

fn variance_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        IN | OUT => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, VARIANCE_MODIFIER))
        }
        _ => None,
    }
}

fn platform_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if matches!(parser.current(), EXPECT | ACTUAL) {
        let m = parser.start();
        parser.bump_any();
        Some(m.complete(parser, PLATFORM_MODIFIER))
    } else {
        None
    }
}
