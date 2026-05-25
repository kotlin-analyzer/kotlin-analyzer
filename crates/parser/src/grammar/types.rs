use syntax::{SyntaxKind::*, T};

use super::annotations::annotation;
use super::identifiers::is_simple_ident_at;
use super::identifiers::{is_simple_identifier, simple_identifier};
use crate::{CompletedMarker, Parser};

pub(crate) fn ty(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    type_modifiers(parser);

    if let Some(lhs) = complex(parser).or_else(|| simple(parser)) {
        type_suffix(parser, lhs);
        Some(m.complete(parser, TYPE))
    } else {
        // TODO: handle when modifiers were parsed but no type was found.
        m.abandon(parser);
        None
    }
}

fn type_suffix(parser: &mut Parser<'_>, lhs: TypeResult) -> Option<TypeResult> {
    let lhs = match lhs {
        TypeResult::Simple(lhs) => {
            let m = lhs.precede(parser);
            while parser.at(T![.]) && is_simple_ident_at(parser, 1) {
                parser.eat(T![.]);
                simple_user_type(parser);
            }
            let res = m.complete(parser, USER_TYPE);
            return type_suffix(parser, TypeResult::User(res));
        }
        TypeResult::User(lhs) => {
            let cm = lhs.precede(parser).complete(parser, TYPE_REFERENCE);
            return type_suffix(parser, TypeResult::TyRef(cm));
        }
        it => it,
    };

    match parser.current() {
        QUEST_NO_WS | QUEST_WS => {
            // nullable type
            // TODO: check variants of lhs
            let m = lhs.marker().precede(parser);
            quests(parser);
            let res = m.complete(parser, NULLABLE_TYPE);
            type_suffix(parser, TypeResult::Nullable(res))
        }
        T![&] => {
            // definitely non-nullable type
            // TODO: check variants of lhs
            let m = lhs.marker().precede(parser);
            parser.bump(T![&]);
            type_modifiers(parser);
            if user_type(parser).or_else(|| parenthesized_user_type(parser)).is_none() {
                // FIXME: recovery
                parser.error("expected type after `&`");
            }
            let res = m.complete(parser, DEFINITELY_NON_NULLABLE_TYPE);
            type_suffix(parser, TypeResult::DefNonNull(res))
        }
        T![.] => {
            // Type with receiver (e.g. `A.(B) -> C`) or member extension type (e.g. `A.B`)
            if matches!(
                lhs,
                TypeResult::Paren(_)
                    | TypeResult::ParenUser(_)
                    | TypeResult::Nullable(_)
                    | TypeResult::TyRef(_)
            ) && parser.nth_at(1, T!['('])
            {
                let res = lhs.marker().precede(parser).complete(parser, RECEIVER_TYPE);
                parser.bump(T![.]);
                return type_suffix(parser, TypeResult::Receiver(res));
            }
            // FIXME: recovery
            parser.error("unexpected `.`");
            Some(lhs)
        }
        T!['('] => {
            // lhs is receiver type for function type (e.g. `A.` in `A.(B) -> C`)
            // while rhs is function type parameters (e.g. `(B) -> C` in `A.(B) -> C`)
            if function_type_parameters(parser).is_some() {
                let m = lhs.marker().precede(parser);
                if parser.eat(ARROW) {
                    ty(parser);
                } else {
                    parser.error("expected `->`");
                }
                let res = m.complete(parser, FUNCTION_TYPE);
                type_suffix(parser, TypeResult::Fn(res))
            } else {
                // FIXME: recovery
                parser.error("expected function type parameters");
                None
            }
        }
        _ => None,
    }
}

fn simple(parser: &mut Parser<'_>) -> Option<TypeResult> {
    if parser.at(DYNAMIC) {
        let m = parser.start();
        parser.bump(DYNAMIC);
        Some(TypeResult::TyRef(m.complete(parser, TYPE_REFERENCE)))
    } else {
        simple_user_type(parser).map(TypeResult::Simple)
    }
}

fn complex(parser: &mut Parser<'_>) -> Option<TypeResult> {
    if !parser.at(T!['(']) {
        return None;
    }

    let mut type_only = false;
    let mut entries = 0;

    let m = parser.start();
    parser.bump(T!['(']);

    if parameter(parser).is_none() {
        if ty(parser).is_some() {
            type_only = true;
            entries += 1;
        }
    } else {
        entries += 1;
    }

    while parser.at(T![,]) && !matches!(parser.nth(1), T![')'] | EOF) {
        parser.bump(T![,]);
        if parameter(parser).is_none() {
            ty(parser);
        } else {
            type_only = false;
        }
        entries += 1;
    }

    if parser.at(T![,]) {
        parser.bump(T![,]); // allow trailing comma
    }

    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }

    if type_only && !parser.at(T![->]) && entries <= 1 {
        Some(TypeResult::Paren(m.complete(parser, PARENTHESIZED_TYPE)))
    } else {
        let m = m.complete(parser, FUNCTION_TYPE_PARAMETERS).precede(parser);
        if parser.eat(ARROW) {
            ty(parser);
        } else {
            parser.error("expected `->`");
        }
        Some(TypeResult::Fn(m.complete(parser, FUNCTION_TYPE)))
    }
}

pub(crate) enum TypeResult {
    Simple(CompletedMarker),
    User(CompletedMarker),
    TyRef(CompletedMarker),
    Fn(CompletedMarker),
    Paren(CompletedMarker),
    ParenUser(CompletedMarker),
    Nullable(CompletedMarker),
    DefNonNull(CompletedMarker),
    Receiver(CompletedMarker),
}

impl TypeResult {
    pub(crate) fn marker(self) -> CompletedMarker {
        match self {
            TypeResult::Simple(m)
            | TypeResult::User(m)
            | TypeResult::TyRef(m)
            | TypeResult::Fn(m)
            | TypeResult::Paren(m)
            | TypeResult::ParenUser(m)
            | TypeResult::Nullable(m)
            | TypeResult::DefNonNull(m)
            | TypeResult::Receiver(m) => m,
        }
    }
}

pub(crate) fn type_reference(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        DYNAMIC => {
            let m = parser.start();
            parser.bump(DYNAMIC);
            Some(m.complete(parser, TYPE_REFERENCE))
        }
        _ if is_simple_identifier(parser) => {
            let m = parser.start();
            user_type(parser);
            Some(m.complete(parser, TYPE_REFERENCE))
        }
        _ => None,
    }
}

pub(crate) fn user_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = simple_user_type(parser) {
        let m = cm.precede(parser);
        while parser.at(T![.]) && is_simple_ident_at(parser, 1) {
            parser.eat(T![.]);
            simple_user_type(parser);
        }
        Some(m.complete(parser, USER_TYPE))
    } else {
        None
    }
}

fn simple_user_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_simple_identifier(parser) {
        let m = parser.start();
        simple_identifier(parser);
        if parser.at(T![<]) {
            type_arguments(parser);
        }
        Some(m.complete(parser, SIMPLE_USER_TYPE))
    } else {
        None
    }
}

pub(crate) fn type_projection(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![*]) {
        let m = parser.start();
        parser.bump(T![*]);
        Some(m.complete(parser, TYPE_PROJECTION))
    } else {
        if let Some(tp) = type_projection_modifiers(parser) {
            let m = tp.precede(parser);
            ty(parser);
            Some(m.complete(parser, TYPE_PROJECTION))
        } else {
            let m = parser.start();
            ty(parser);
            Some(m.complete(parser, TYPE_PROJECTION))
        }
    }
}

pub(crate) fn type_arguments(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T![<]) {
        return None;
    }
    let m = parser.start();
    parser.bump(T![<]);

    let mut parsed = 0;

    while !matches!(parser.current(), T![>] | EOF) {
        if type_projection(parser).is_some() {
            parsed += 1;
        }
        if !parser.eat(T![,]) {
            break;
        }
    }

    if parsed == 0 {
        parser.error("expected type projection");
    }
    if !parser.eat(T![>]) {
        // TODO: recover
        parser.error("expected `>`");
    }

    Some(m.complete(parser, TYPE_ARGUMENTS))
}

fn quests(parser: &mut Parser<'_>) {
    assert!(matches!(parser.current(), QUEST_NO_WS | QUEST_WS));
    while matches!(parser.current(), QUEST_NO_WS | QUEST_WS) {
        let m = parser.start();
        parser.bump_any();
        m.complete(parser, QUEST);
    }
}

fn nullable_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();

    if let Some(cm) = parenthesized_type(parser).or_else(|| type_reference(parser)) {
        let mut seen = 0;
        while matches!(parser.current(), QUEST_NO_WS | QUEST_WS) {
            seen += 1;
            let m = parser.start();
            parser.bump_any();
            m.complete(parser, QUEST);
        }

        if seen == 0 {
            m.abandon(parser);
            return Some(cm); // No nullable suffix, so not actually a nullable type. Return the inner type.
        }

        Some(m.complete(parser, NULLABLE_TYPE))
    } else {
        m.abandon(parser);
        None
    }
}

fn parenthesized_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }

    let m = parser.start();

    parser.eat(T!['(']);

    ty(parser);

    if !parser.eat(T![')']) {
        parser.error("expected closing parenthesis `)`");
    }
    Some(m.complete(parser, PARENTHESIZED_TYPE))
}

enum FnTypeResult {
    None,
    Inner(CompletedMarker),
    Fn,
}

fn function_type(parser: &mut Parser<'_>) -> FnTypeResult {
    let m = parser.start();

    let inner = receiver_type(parser, RecvType::Dotted);
    if function_type_parameters(parser).is_some() {
        if parser.eat(ARROW) {
            ty(parser);
        } else {
            parser.error("expected `->`");
        }
        m.complete(parser, FUNCTION_TYPE);
        return FnTypeResult::Fn;
    }
    m.abandon(parser);
    inner.map(FnTypeResult::Inner).unwrap_or(FnTypeResult::None)
}

fn function_type_parameters(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['(']);

    if parameter(parser).is_none() {
        ty(parser);
    }

    while parser.at(T![,]) && !matches!(parser.nth(1), T![')'] | EOF) {
        parser.bump(T![,]);
        if parameter(parser).is_none() {
            ty(parser);
        }
    }

    if parser.at(T![,]) {
        parser.bump(T![,]); // allow trailing comma
    }

    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }
    Some(m.complete(parser, FUNCTION_TYPE_PARAMETERS))
}

fn parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_simple_identifier(parser) && parser.nth(1) == COLON {
        let m = parser.start();
        simple_identifier(parser);
        parser.bump(COLON);
        ty(parser);
        Some(m.complete(parser, PARAMETER))
    } else {
        None
    }
}

pub(crate) enum RecvType {
    Dotted,
    NotDotted,
}

pub(crate) fn receiver_type(
    parser: &mut Parser<'_>,
    recv_type: RecvType,
) -> Option<CompletedMarker> {
    let m = parser.start();
    type_modifiers(parser);

    if let Some(cm) = nullable_type(parser) {
        if let RecvType::NotDotted = recv_type {
            return Some(m.complete(parser, RECEIVER_TYPE));
        }

        if parser.at(T![.]) {
            let res = m.complete(parser, RECEIVER_TYPE);
            // Note that the dot is not considered part of the receiver type,
            // so we don't include it, they go to the parent.
            parser.bump(T![.]);
            return Some(res);
        } else {
            m.abandon(parser);
            return Some(cm); // No dot, so not actually a receiver type. Return the inner type.
        }
    }
    m.abandon(parser);
    None
}

fn parenthesized_user_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['(']);

    if parser.at(T!['(']) {
        parenthesized_user_type(parser);
    } else {
        user_type(parser);
    }

    if !parser.eat(T![')']) {
        parser.error("expected closing parenthesis `)`");
    }

    Some(m.complete(parser, PARENTHESIZED_USER_TYPE))
}

fn type_modifiers(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = type_modifier(parser) {
        let m = cm.precede(parser);
        while type_modifier(parser).is_some() {}
        Some(m.complete(parser, TYPE_MODIFIERS))
    } else {
        None
    }
}

fn type_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(SUSPEND) {
        let m = parser.start();
        parser.bump(SUSPEND);
        Some(m.complete(parser, TYPE_MODIFIER))
    } else {
        annotation(parser).map(|cm| cm.precede(parser).complete(parser, TYPE_MODIFIER))
    }
}

fn type_projection_modifiers(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = type_projection_modifier(parser) {
        let m = cm.precede(parser);
        while type_projection_modifier(parser).is_some() {}
        Some(m.complete(parser, TYPE_PROJECTION_MODIFIERS))
    } else {
        None
    }
}

fn type_projection_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let mut is_variance = false;
    if let Some(cm) =
        variance_modifier(parser).inspect(|_| is_variance = true).or_else(|| annotation(parser))
    {
        let m = cm.precede(parser);
        Some(m.complete(parser, TYPE_PROJECTION_MODIFIER))
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
