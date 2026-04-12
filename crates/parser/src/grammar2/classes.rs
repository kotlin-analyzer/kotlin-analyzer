use syntax::{SyntaxKind::*, T};

use crate::{
    grammar2::{
        annotations::annotation,
        class_members::class_member_declarations,
        enum_classes::{BodyResult, enum_class_body},
        expressions::{expression, value_arguments},
        identifiers::simple_identifier,
        modifiers::{modifiers, type_parameter_modifiers},
        types::ty,
    },
    ra::{CompletedMarker, Marker, Parser},
};

pub(crate) fn class_declaration(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    modifiers(parser);

    if !(parser.at(T![class]) || parser.at(T![interface]) || parser.at(T![fun])) {
        return None;
    }

    if parser.eat(T![class]) {
    } else if parser.eat(T![interface]) {
    } else if parser.eat(T![fun]) {
        if !parser.eat(T![interface]) {
            parser.error("expected 'interface'");
        }
    }

    if simple_identifier(parser).is_none() {
        parser.error("expected an identifier");
    }

    type_parameters(parser);
    primary_constructor(parser);

    if parser.eat(T![:]) {
        if delegation_specifiers(parser).is_none() {
            parser.error("expected delegation specifiers");
        }
    }

    type_constraints(parser);

    if parser.at(T!['{']) {
        let m = parser.start();

        parser.bump(T!['{']);
        let first_modifiers = modifiers(parser);

        if let BodyResult::None(m) = enum_class_body(parser, m, first_modifiers.clone()) {
            class_body(parser, Some(m), first_modifiers);
        }
    }

    Some(m.complete(parser, CLASS_DECLARATION))
}

fn primary_constructor(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();

    modifiers(parser);
    if !parser.eat(T![constructor]) {
        m.abandon(parser);
        return None;
    }

    if class_parameters(parser).is_none() {
        parser.error("expected `(`");
    }

    Some(m.complete(parser, PRIMARY_CONSTRUCTOR))
}

pub(crate) fn class_body(
    parser: &mut Parser<'_>,
    opening_brace: Option<Marker>,
    modifier_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    let Some(m) = opening_brace.or_else(|| {
        if !parser.at(T!['{']) {
            None
        } else {
            let m = parser.start();
            parser.bump(T!['{']);
            Some(m)
        }
    }) else {
        return None;
    };

    class_member_declarations(parser, modifier_marker);

    if !parser.eat(T!['}']) {
        parser.error("expected '}'");
    }

    Some(m.complete(parser, CLASS_BODY))
}

fn class_parameters(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }

    let m = parser.start();
    parser.bump(T!['(']);

    if class_parameter(parser).is_some() {
        while parser.eat(T![,]) && class_parameter(parser).is_some() {}
    }

    if !parser.eat(T![')']) {
        parser.error("expected ')'");
    }

    Some(m.complete(parser, CLASS_PARAMETERS))
}

fn class_parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();

    modifiers(parser);
    if parser.at(T![val]) || parser.at(T![var]) {
        parser.bump_any();
    }

    if simple_identifier(parser).is_none() {
        parser.error("expected an identifier");
    }

    if !parser.eat(T![:]) {
        parser.error("expected ':'");
    }

    if ty(parser).is_none() {
        parser.error("expected a type");
    }

    if parser.eat(T![=]) && expression(parser).is_none() {
        parser.error("expected an expression");
    }

    Some(m.complete(parser, CLASS_PARAMETER))
}

pub(crate) fn delegation_specifiers(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(first) = annotated_delegation_specifier(parser) {
        let m = first.precede(parser);

        while parser.eat(T![,]) {
            if annotated_delegation_specifier(parser).is_none() {
                parser.error("expected a delegation specifier");
                break;
            }
        }

        Some(m.complete(parser, DELEGATION_SPECIFIERS))
    } else {
        None
    }
}

fn delegation_specifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();

    // NB: ty, in this function, matches more things than function | user types,
    // if this happens to be the case, we should report an error later.
    // Not in parsing phase
    if parser.eat(T![suspend]) {
        if ty(parser).is_none() {
            parser.error("expected a function type");
        }
        return Some(m.complete(parser, DELEGATION_SPECIFIER));
    }

    if let Some(ty_marker) = ty(parser) {
        if parser.at(T![by]) {
            explicit_delegation(parser, ty_marker);
        } else {
            constructor_invocation(parser, ty_marker);
        }
        Some(m.complete(parser, DELEGATION_SPECIFIER))
    } else {
        None
    }
}

fn annotated_delegation_specifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();

    while annotation(parser).is_some() {}
    if delegation_specifier(parser).is_none() {
        m.abandon(parser);
        return None;
    }

    Some(m.complete(parser, ANNOTATED_DELEGATION_SPECIFIER))
}

fn explicit_delegation(
    parser: &mut Parser<'_>,
    ty_marker: CompletedMarker,
) -> Option<CompletedMarker> {
    if parser.at(T![by]) {
        let m = ty_marker.precede(parser);
        parser.eat(T![by]);

        if expression(parser).is_none() {
            parser.error("expected an expression");
        }
        Some(m.complete(parser, EXPLICIT_DELEGATION))
    } else {
        None
    }
}

pub(crate) fn type_parameters(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![<]) {
        let m = parser.start();
        parser.eat(T![<]);
        if type_parameter(parser).is_some() {
            while parser.eat(T![,]) && type_parameter(parser).is_some() {}
        }
        if !parser.eat(T![>]) {
            parser.error("expected '>'");
        }
        Some(m.complete(parser, TYPE_PARAMETERS))
    } else {
        None
    }
}

pub(crate) fn constructor_invocation(
    parser: &mut Parser<'_>,
    user_type_marker: CompletedMarker,
) -> Option<CompletedMarker> {
    let m = user_type_marker.precede(parser);
    if value_arguments(parser).is_some() {
        Some(m.complete(parser, CONSTRUCTOR_INVOCATION))
    } else {
        m.abandon(parser);
        None
    }
}

fn type_parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    type_parameter_modifiers(parser);
    if simple_identifier(parser).is_some() {
        if parser.eat(T![:]) {
            if ty(parser).is_none() {
                parser.error("expected a type");
            }
        }
        Some(m.complete(parser, TYPE_PARAMETER))
    } else {
        m.abandon(parser);
        None
    }
}

pub(crate) fn type_constraints(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![where]) {
        let m = parser.start();
        parser.eat(T![where]);
        if type_constraint(parser).is_none() {
            parser.error("expected a type constraint");
        } else {
            while parser.eat(T![,]) {
                if type_constraint(parser).is_none() {
                    parser.error("expected a type constraint");
                    break;
                }
            }
        }
        Some(m.complete(parser, TYPE_CONSTRAINTS))
    } else {
        None
    }
}

fn type_constraint(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    annotation(parser);
    if simple_identifier(parser).is_some() {
        if parser.eat(T![:]) {
            if ty(parser).is_none() {
                parser.error("expected a type");
            }
        } else {
            parser.error("expected ':'");
        }
        Some(m.complete(parser, TYPE_CONSTRAINT))
    } else {
        m.abandon(parser);
        None
    }
}
