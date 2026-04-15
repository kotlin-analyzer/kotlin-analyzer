use syntax::{SyntaxKind::*, T};

use super::classes::{class_body, delegation_specifiers, type_constraints};
use super::general::declaration;
use super::identifiers::is_simple_identifier;
use super::statements::{block, semi, semis};
use super::types::{RecvType, receiver_type, user_type};
use crate::ra::{CompletedMarker, Parser, TokenSet};

use super::annotations::annotation;
use super::classes::type_parameters;
use super::expressions::{expression, value_arguments};
use super::identifiers::simple_identifier;
use super::modifiers::{modifiers, parameter_modifiers};
use super::types::ty;

pub(crate) fn class_member_declarations(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    let m = parser.start();
    if class_member_declaration(parser, modifiers_marker).is_some() {
        semis(parser);
        while class_member_declaration(parser, None).is_some() {
            semis(parser);
        }
    }
    Some(m.complete(parser, CLASS_MEMBER_DECLARATIONS))
}

fn class_member_declaration(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if let Some(cm) = anonymous_initializer(parser) {
        return Some(
            cm.precede(parser)
                .complete(parser, CLASS_MEMBER_DECLARATION),
        );
    }
    let m = parser.start();
    let modifiers_marker = modifiers_marker.or_else(|| modifiers(parser));

    if parser.at(T![companion]) {
        companion_object(parser, modifiers_marker);
    } else if parser.at(T![constructor]) {
        secondary_constructor(parser, modifiers_marker);
    } else if declaration(parser, modifiers_marker).is_some() {
    } else {
        m.abandon(parser);
        return None;
    }
    Some(m.complete(parser, CLASS_MEMBER_DECLARATION))
}

fn anonymous_initializer(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![init]) {
        let m = parser.start();
        parser.eat(T![init]);
        block(parser);
        Some(m.complete(parser, ANONYMOUS_INITIALIZER))
    } else {
        None
    }
}

fn companion_object(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if !parser.at(T![companion]) {
        return None;
    }

    let m = modifiers_marker
        .map(|cm| cm.precede(parser))
        .unwrap_or_else(|| parser.start());

    parser.eat(T![companion]);
    parser.eat(T![data]);

    if !parser.eat(T![object]) {
        parser.error("expected 'object' keyword");
    }

    simple_identifier(parser);

    if parser.eat(T![:]) && delegation_specifiers(parser).is_none() {
        parser.error("expected delegation specifiers");
    }
    class_body(parser, None, None);
    Some(m.complete(parser, COMPANION_OBJECT))
}

pub(crate) fn starts_fn_declaration(parser: &mut Parser<'_>) -> bool {
    parser.at(T![fun]) && !parser.nth_at(1, T![interface])
}

pub(crate) fn function_declaration(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if !starts_fn_declaration(parser) {
        return None;
    }

    let m = modifiers_marker
        .map(|cm| cm.precede(parser))
        .unwrap_or_else(|| parser.start());

    parser.bump(T![fun]);
    type_parameters(parser);
    receiver_type(parser, RecvType::Dotted);

    if simple_identifier(parser).is_none() {
        parser.error("expected an identifier");
    }
    if function_value_parameters(parser).is_none() {
        parser.error("expected (");
    }
    parameters_with_opt_type(parser);

    if parser.eat(T![:]) && ty(parser).is_none() {
        parser.error("expected a type");
    }
    type_constraints(parser);
    function_body(parser);
    Some(m.complete(parser, FUNCTION_DECLARATION))
}

pub(crate) fn function_body(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = block(parser) {
        Some(cm.precede(parser).complete(parser, FUNCTION_BODY))
    } else {
        if parser.eat(T![=]) {
            if expression(parser).is_none() {
                parser.error("expected an expression");
            }
            Some(parser.start().complete(parser, FUNCTION_BODY))
        } else {
            None
        }
    }
}

pub(crate) fn object_declaration(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if !parser.at(T![object]) {
        return None;
    }
    let m = modifiers_marker
        .map(|cm| cm.precede(parser))
        .unwrap_or_else(|| parser.start());

    parser.eat(T![object]);
    if simple_identifier(parser).is_none() {
        parser.error("expected an identifier");
    }
    if parser.eat(T![:]) && delegation_specifiers(parser).is_none() {
        parser.error("expected delegation specifiers");
    }
    class_body(parser, None, None);

    Some(m.complete(parser, OBJECT_DECLARATION))
}

// can also parse userType
fn constructor_invocation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = user_type(parser) {
        let m = cm.precede(parser);
        if value_arguments(parser).is_some() {
            Some(m.complete(parser, CONSTRUCTOR_INVOCATION))
        } else {
            m.abandon(parser);
            None
        }
    } else {
        None
    }
}

fn property_delegate(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![by]) {
        let m = parser.start();
        parser.eat(T![by]);
        if expression(parser).is_none() {
            parser.error("expected an expression");
        }
        Some(m.complete(parser, PROPERTY_DELEGATE))
    } else {
        None
    }
}

pub(crate) const PROPERTY_DECLARATION_START: TokenSet = TokenSet::new(&[VAL, VAR]);

pub(crate) fn multi_variable_declaration(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T!['(']) {
        let m = parser.start();
        parser.eat(T!['(']);
        if variable_declaration(parser).is_some() {
            while !parser.eat(T![,]) && variable_declaration(parser).is_some() {}
        }
        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        Some(m.complete(parser, MULTI_VARIABLE_DECLARATION))
    } else {
        None
    }
}
pub(crate) fn variable_declaration(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    annotation(parser);
    if simple_identifier(parser).is_some() {
        if parser.eat(T![:]) && ty(parser).is_none() {
            parser.error("expected a type");
        }
        Some(m.complete(parser, VARIABLE_DECLARATION))
    } else {
        m.abandon(parser);
        None
    }
}

/// Starts with either 'val' or 'var' keyword
pub(crate) fn property_declaration(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if !parser.at_ts(PROPERTY_DECLARATION_START) {
        return None;
    }
    let m = modifiers_marker
        .map(|cm| cm.precede(parser))
        .unwrap_or_else(|| parser.start());

    parser.bump_any();
    type_parameters(parser);
    receiver_type(parser, RecvType::Dotted);
    multi_variable_declaration(parser).or_else(|| variable_declaration(parser));
    type_constraints(parser);
    if parser.eat(T![=])
        && property_delegate(parser)
            .or_else(|| expression(parser))
            .is_none()
    {
        parser.error("expected an expression");
    }
    parser.eat(T![;]);

    let mod_cm = modifiers(parser);
    if parser.at(GET) {
        getter(parser, mod_cm);
        semi(parser);
        let mod_cm = modifiers(parser);
        if parser.at(SET) {
            setter(parser, mod_cm);
        }
    } else if parser.at(SET) {
        setter(parser, mod_cm);
        semi(parser);
        let mod_cm = modifiers(parser);
        if parser.at(GET) {
            getter(parser, mod_cm);
        }
    }
    Some(m.complete(parser, PROPERTY_DECLARATION))
}

fn getter(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if !parser.at(GET) {
        return None;
    }
    let m = modifiers_marker
        .map(|cm| cm.precede(parser))
        .unwrap_or_else(|| parser.start());

    if parser.eat(T!['(']) {
        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        if parser.eat(T![:]) && ty(parser).is_none() {
            parser.error("expected a type");
        }
        if function_body(parser).is_none() {
            parser.error("expected a function body");
        }
    }
    Some(m.complete(parser, GETTER))
}

fn setter(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if !parser.at(SET) {
        return None;
    }
    let m = modifiers_marker
        .map(|cm| cm.precede(parser))
        .unwrap_or_else(|| parser.start());
    if parser.eat(T!['(']) {
        if function_value_parameter_with_optional_type(parser).is_some() {
            parser.eat(T![,]);
        }
        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        if parser.eat(T![:]) && ty(parser).is_none() {
            parser.error("expected a type");
        }
        if function_body(parser).is_none() {
            parser.error("expected a function body");
        }
    }
    Some(m.complete(parser, SETTER))
}

fn function_value_parameters(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T!['(']) {
        let m = parser.start();
        parser.eat(T!['(']);

        if function_value_parameter(parser).is_some() {
            while !parser.eat(T![,]) && function_value_parameter(parser).is_some() {}
        }

        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        Some(m.complete(parser, FUNCTION_VALUE_PARAMETERS))
    } else {
        None
    }
}
fn function_value_parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    parameter_modifiers(parser);
    if parameter(parser).is_none() {
        parser.error("expected a parameter");
    }

    if parser.eat(T![=]) && expression(parser).is_none() {
        parser.error("expected an expression");
    }
    Some(m.complete(parser, FUNCTION_VALUE_PARAMETER))
}

fn function_value_parameter_with_optional_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    parameter_modifiers(parser);
    if parameter_with_opt_type(parser).is_none() {
        parser.error("expected a parameter");
    }

    if parser.eat(T![=]) && expression(parser).is_none() {
        parser.error("expected an expression");
    }
    Some(m.complete(parser, FUNCTION_VALUE_PARAMETER_WITH_OPTIONAL_TYPE))
}

fn secondary_constructor(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if !parser.at(T![constructor]) {
        return None;
    }
    let m = modifiers_marker
        .map(|cm| cm.precede(parser))
        .unwrap_or_else(|| parser.start());
    parser.eat(T![constructor]);
    function_value_parameters(parser);
    if parser.eat(T![:]) {
        constructor_delegation_call(parser); // maybe record an error here if none
    }
    block(parser);
    Some(m.complete(parser, SECONDARY_CONSTRUCTOR))
}

pub(crate) fn parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_simple_identifier(parser) && parser.nth_at(1, T![:]) {
        let m = parser.start();
        simple_identifier(parser);
        parser.eat(T![:]);
        if ty(parser).is_none() {
            parser.error("expected a type");
        }
        Some(m.complete(parser, PARAMETER))
    } else {
        None
    }
}

pub(crate) fn parameters_with_opt_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T!['(']) {
        let m = parser.start();
        parser.eat(T!['(']);

        if function_value_parameter_with_optional_type(parser).is_some() {
            while !parser.eat(T![,])
                && function_value_parameter_with_optional_type(parser).is_some()
            {}
        }

        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        Some(m.complete(parser, PARAMETERS_WITH_OPTIONAL_TYPE))
    } else {
        None
    }
}

fn parameter_with_opt_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_simple_identifier(parser) && parser.nth_at(1, T![:]) {
        let m = parser.start();
        simple_identifier(parser);
        parser.eat(T![:]);
        ty(parser);
        Some(m.complete(parser, PARAMETER_WITH_OPTIONAL_TYPE))
    } else {
        None
    }
}

fn constructor_delegation_call(parser: &mut Parser<'_>) -> Option<()> {
    if parser.eat(T![this]) || parser.eat(T![super]) {
        if value_arguments(parser).is_none() {
            parser.error("expected an expression");
        }
        Some(())
    } else {
        None
    }
}
