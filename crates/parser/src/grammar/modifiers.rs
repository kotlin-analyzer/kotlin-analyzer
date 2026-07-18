use crate::SyntaxKind::*;
use crate::T;

use super::annotations::annotation;
use super::class_members::context_parameter_list;
use crate::{CompletedMarker, Parser};

pub(crate) fn modifiers(parser: &mut Parser<'_>) -> Option<(CompletedMarker, bool)> {
    let mut has_only_annotations = false;
    if let Some(cm) = annotation(parser)
        .inspect(|_| {
            has_only_annotations = true;
        })
        .or_else(|| modifier(parser))
        .or_else(|| context_parameter_list(parser))
    {
        let m = cm.precede(parser);
        let mut seen = 0;
        let mut seen_annotations = 0;
        while annotation(parser)
            .inspect(|_| {
                seen_annotations += 1;
            })
            .or_else(|| modifier(parser))
            .or_else(|| context_parameter_list(parser))
            .is_some()
        {
            seen += 1;
        }
        Some((m.complete(parser, MODIFIERS), has_only_annotations && seen == seen_annotations))
    } else {
        None
    }
}

pub(crate) fn parameter_modifiers(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = annotation(parser).or_else(|| parameter_modifier(parser)) {
        let m = cm.precede(parser);
        while annotation(parser).or_else(|| parameter_modifier(parser)).is_some() {}
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
}

fn type_parameter_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    reification_modifier(parser)
        .or_else(|| variance_modifier(parser))
        .or_else(|| annotation(parser))
        .map(|cm| cm.precede(parser).complete(parser, TYPE_PARAMETER_MODIFIER))
}

fn class_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![enum] | SEALED_KW | ANNOTATION_KW | DATA_KW | INNER_KW | VALUE_KW => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, CLASS_MODIFIER))
        }
        _ => None,
    }
}

fn member_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        OVERRIDE_KW | LATEINIT_KW => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, MEMBER_MODIFIER))
        }
        _ => None,
    }
}

fn visibility_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![public] | T![private] | T![internal] | T![protected] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, VISIBILITY_MODIFIER))
        }
        _ => None,
    }
}

fn function_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![tailrec] | T![operator] | T![infix] | T![inline] | T![external] | T![suspend] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, FUNCTION_MODIFIER))
        }
        _ => None,
    }
}

fn property_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![const]) {
        let m = parser.start();
        parser.bump(T![const]);
        Some(m.complete(parser, PROPERTY_MODIFIER))
    } else {
        None
    }
}

fn inheritance_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![abstract] | T![final] | T![open] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, INHERITANCE_MODIFIER))
        }
        _ => None,
    }
}

fn parameter_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![vararg]) || parser.at(T![noinline]) || parser.at(T![crossinline]) {
        let m = parser.start();
        parser.bump_any();
        Some(m.complete(parser, PARAMETER_MODIFIER))
    } else {
        None
    }
}

fn reification_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![reified]) {
        let m = parser.start();
        parser.bump(T![reified]);
        Some(m.complete(parser, REIFICATION_MODIFIER))
    } else {
        None
    }
}

fn variance_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![in]) || parser.at(T![out]) {
        let m = parser.start();
        parser.bump_any();
        Some(m.complete(parser, VARIANCE_MODIFIER))
    } else {
        None
    }
}

fn platform_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![expect]) || parser.at(T![actual]) {
        let m = parser.start();
        parser.bump_any();
        Some(m.complete(parser, PLATFORM_MODIFIER))
    } else {
        None
    }
}
