use crate::{SyntaxKind::*, T, TokenSet};

use super::annotations::annotation;
use super::class_members::class_member_declarations;
use super::enum_classes::{BodyResult, enum_class_body};
use super::expressions::{expression, value_arguments};
use super::identifiers::simple_identifier;
use super::modifiers::type_parameter_modifiers;
use super::types::{TypeResult, ty, unclosed_ty};
use crate::{CompletedMarker, Marker, Parser};

pub(crate) fn starts_class_declaration(parser: &mut Parser<'_>) -> bool {
    parser.at(T![class])
        || parser.at(T![interface])
        || (parser.at(T![fun]) && parser.nth_at(1, T![interface]))
}

// test class_declaration
// class Foo1
// class Foo2()
// class Foo22(name: String, age: Int)
// class Foo23<T>(name: T, val age: Int)
// class Foo3<T> : Bar by baz
// class Foo4<T> where T: Any
// class Foo5<T> where T: Any, T: Serializable
// class Foo7<T> where T: Any, T: Serializable {}
// class Foo8 private constructor(val name: String, var age: Int)
// fun interface Foo9<T> where T: Any, T: Serializable
// enum class Foo10<T> where T: Any, T: Serializable {}
// abstract class Foo11<T>(val name: String, val age: Int) where T: Any, T: Serializable
// data class Foo12<T>(val name: String, val age: Int)
// class A
// {}
pub(crate) fn class_declaration(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();

    if !starts_class_declaration(parser) {
        m.abandon(parser);
        return None;
    }

    if parser.at(T![class]) || parser.at(T![interface]) {
        parser.bump_any();
    } else if parser.at(T![fun]) {
        parser.bump(T![fun]);
        parser.bump(T![interface]);
    }

    if simple_identifier(parser).is_none() {
        parser.error("expected an identifier");
    }

    type_parameters(parser);
    let cm = m.complete(parser, CLASS_DECLARATION);

    if let Some(Err(_)) = primary_constructor(parser) {
        // test class_then_decl
        // abstract class AB private fun f() = 1
        return Some(cm);
    }

    if parser.eat(T![:]) && delegation_specifiers(parser).is_none() {
        parser.error("expected delegation specifiers");
    }

    type_constraints(parser);

    if parser.at(T!['{']) {
        let m = parser.start();

        parser.bump(T!['{']);

        if let BodyResult::None { opening_brace } = enum_class_body(parser, m) {
            class_body(parser, Some(opening_brace));
        }
    }

    Some(cm.extend_right(parser))
}

fn primary_constructor(parser: &mut Parser<'_>) -> Option<Result<CompletedMarker, ()>> {
    let m = parser.start_with_fresh_modifiers();
    match (m.has_modifiers(), parser.eat(T![constructor])) {
        (true, false) => {
            // This a new declaration
            m.abandon(parser);
            return Some(Err(()));
        }
        (false, false) if !parser.at(T!['(']) => {
            m.abandon(parser);
            return None;
        }
        _ => {}
    }

    class_parameters(parser);

    Some(Ok(m.complete(parser, PRIMARY_CONSTRUCTOR)))
}

pub(crate) fn class_body(
    parser: &mut Parser<'_>,
    opening_brace: Option<Marker>,
) -> Option<CompletedMarker> {
    let m = opening_brace.or_else(|| {
        if !parser.at(T!['{']) {
            None
        } else {
            let m = parser.start();
            parser.bump(T!['{']);
            Some(m)
        }
    })?;

    class_member_declarations(parser);

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
    // TODO: we can error if no parameter is parsed, asking to remove the parentheses, but this should really be a warning, not an error, so we can just ignore this case for now.
    if !parser.eat(T![')']) {
        parser.error("expected ')'");
    }

    Some(m.complete(parser, CLASS_PARAMETERS))
}

const CLASS_PARAMETER_RECOVERY: TokenSet = TokenSet::new(&[T![,], T![')'], T!['{']]);

fn class_parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();
    let mut seen = 0;

    if parser.at(T![val]) || parser.at(T![var]) {
        parser.bump_any();
        seen += 1;
    }

    if simple_identifier(parser).is_none() {
        if seen == 0 {
            m.abandon(parser);
            return None;
        }
        parser.err_recover("expected an identifier", CLASS_PARAMETER_RECOVERY);
    } else {
        if !parser.eat(T![:]) {
            parser.error("expected ':'");
        }

        if ty(parser).is_none() {
            parser.error("expected a type");
        }

        if parser.eat(T![=]) && expression(parser).is_none() {
            parser.error("expected an expression :#7");
        }
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

    let Some(types) = unclosed_ty(parser) else {
        m.abandon(parser);
        return None;
    };

    match types {
        TypeResult::User(cm) | TypeResult::Fn(cm) if parser.at(T![by]) => {
            explicit_delegation(parser, cm);
        }
        TypeResult::User(cm) if parser.at(T!['(']) => {
            let _ = constructor_invocation(parser, cm, false);
        }
        TypeResult::User(_) | TypeResult::Fn(_) => {}
        _ => {
            // maybe move this error linting phase
            parser.error("expected delegation specifier");
        }
    }
    Some(m.complete(parser, DELEGATION_SPECIFIER))
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

        parser.disallow_call_suffix();
        if expression(parser).is_none() {
            parser.error("expected an expression :#6");
        }
        parser.reset_disallow_call_suffix();
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
    in_fn_type_position: bool,
) -> Result<CompletedMarker, CompletedMarker> {
    if !parser.at(T!['(']) || (in_fn_type_position && parser.at_lparen_after_ws()) {
        return Err(user_type_marker);
    }
    let m = user_type_marker.precede(parser);
    value_arguments(parser);
    Ok(m.complete(parser, CONSTRUCTOR_INVOCATION))
}

fn type_parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    type_parameter_modifiers(parser);
    if simple_identifier(parser).is_some() {
        if parser.eat(T![:]) && ty(parser).is_none() {
            parser.error("expected a type");
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
