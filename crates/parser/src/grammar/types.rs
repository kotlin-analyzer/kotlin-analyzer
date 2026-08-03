use crate::{Marker, SyntaxKind::*, T, TokenSet};

use super::annotations::annotation;
use super::identifiers::is_simple_ident_at;
use super::identifiers::{is_simple_identifier, simple_identifier};
use crate::{CompletedMarker, Parser};

pub(super) fn unclosed_ty(parser: &mut Parser<'_>) -> Option<TypeResult> {
    inner_ty(parser, true).map(|(ts, m)| {
        m.abandon(parser);
        ts
    })
}

fn inner_ty(parser: &mut Parser<'_>, forward_user_type: bool) -> Option<(TypeResult, Marker)> {
    let m = parser.start();
    type_modifiers(parser);

    if let Some(lhs) = complex(parser).or_else(|| simple(parser)) {
        Some((type_suffix(parser, lhs, forward_user_type), m))
    } else {
        // TODO: handle when modifiers were parsed but no type was found.
        m.abandon(parser);
        None
    }
}
pub(crate) fn ty(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some((_, m)) = inner_ty(parser, false) { Some(m.complete(parser, TYPE)) } else { None }
}

const TYPE_CONTINUATION: TokenSet = TokenSet::new(&[T![?], T![&], T![.]]);

fn type_suffix(parser: &mut Parser<'_>, lhs: TypeResult, forward_user_type: bool) -> TypeResult {
    let lhs = match lhs {
        TypeResult::Simple(lhs) => {
            let m = lhs.precede(parser);
            while parser.at(T![.]) && is_simple_ident_at(parser, 1) {
                parser.eat(T![.]);
                simple_user_type(parser);
            }
            let res = m.complete(parser, USER_TYPE);
            return type_suffix(parser, TypeResult::User(res), forward_user_type);
        }
        TypeResult::User(lhs) if !forward_user_type || parser.at_ts(TYPE_CONTINUATION) => {
            let cm = lhs.precede(parser).complete(parser, TYPE_REFERENCE);
            return type_suffix(parser, TypeResult::TyRef(cm), forward_user_type);
        }
        it => it,
    };

    match parser.current() {
        T![?] => {
            // nullable type
            // TODO: check variants of lhs
            let m = lhs.marker().precede(parser);
            quests(parser);
            let res = m.complete(parser, NULLABLE_TYPE);
            type_suffix(parser, TypeResult::Nullable(res), forward_user_type)
        }
        T![&] => {
            // definitely non-nullable type
            // TODO: check variants of lhs
            let m = lhs.marker().precede(parser);
            parser.bump(T![&]);
            type_modifiers(parser);
            if user_type(parser, UserType::All)
                .or_else(|| parenthesized_user_type(parser))
                .is_none()
            {
                // FIXME: recovery
                parser.error("expected type after `&`");
            }
            let res = m.complete(parser, DEFINITELY_NON_NULLABLE_TYPE);
            type_suffix(parser, TypeResult::DefNonNull(res), forward_user_type)
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
                return type_suffix(parser, TypeResult::Receiver(res), forward_user_type);
            }
            // FIXME: recovery
            parser.error("unexpected `.`");
            lhs
        }
        T!['('] => {
            // lhs is receiver type for function type (e.g. `A.` in `A.(B) -> C`)
            // while rhs is function type parameters (e.g. `(B) -> C` in `A.(B) -> C`)
            if !matches!(lhs, TypeResult::Receiver(_)) {
                return lhs;
            }
            if function_type_parameters(parser).is_some() {
                let m = lhs.marker().precede(parser);
                if parser.eat(ARROW) {
                    ty(parser);
                } else {
                    parser.error("expected `->`");
                }
                let res = m.complete(parser, FUNCTION_TYPE);
                type_suffix(parser, TypeResult::Fn(res), forward_user_type)
            } else {
                // FIXME: recovery
                parser.error("expected function type parameters");
                lhs
            }
        }
        _ => lhs,
    }
}

fn simple(parser: &mut Parser<'_>) -> Option<TypeResult> {
    if parser.at(T![dynamic]) {
        let m = parser.start();
        parser.bump(T![dynamic]);
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

pub(super) fn type_reference(parser: &mut Parser<'_>, mode: UserType) -> Option<CompletedMarker> {
    match parser.current() {
        T![dynamic] => {
            let m = parser.start();
            parser.bump(T![dynamic]);
            Some(m.complete(parser, TYPE_REFERENCE))
        }
        _ if is_simple_identifier(parser) => {
            let m = parser.start();
            user_type(parser, mode);
            Some(m.complete(parser, TYPE_REFERENCE))
        }
        _ => None,
    }
}

pub(super) fn user_type(parser: &mut Parser<'_>, mode: UserType) -> Option<CompletedMarker> {
    if let Some(cm) = simple_user_type(parser) {
        let m = cm.precede(parser);
        // HGKIC: todo
        while parser.at(T![.]) && is_simple_ident_at(parser, 1) {
            if !parser.nth_at(2, T![.]) && matches!(mode, UserType::BeforeName) {
                break;
            }
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

pub(crate) fn strict_type_arguments(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    base_type_arguments(parser, true)
}

pub(crate) fn type_arguments(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    base_type_arguments(parser, false)
}

pub(crate) fn base_type_arguments(
    parser: &mut Parser<'_>,
    strict: bool,
) -> Option<CompletedMarker> {
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
        if strict {
            m.abandon(parser);
            return None;
        }
        parser.error("expected type projection");
    }

    if !parser.eat(T![>]) {
        if strict {
            m.abandon(parser);
            return None;
        }
        // TODO: recover
        parser.error("expected `>`");
    }

    Some(m.complete(parser, TYPE_ARGUMENTS))
}

fn quests(parser: &mut Parser<'_>) {
    assert!(matches!(parser.current(), T![?]));
    while matches!(parser.current(), T![?]) {
        let m = parser.start();
        parser.bump_any();
        m.complete(parser, QUEST);
    }
}

fn nullable_type(parser: &mut Parser<'_>, user_type: UserType) -> Option<CompletedMarker> {
    let m = parser.start();

    if let Some(cm) = parenthesized_type(parser).or_else(|| type_reference(parser, user_type)) {
        let mut seen = 0;
        while matches!(parser.current(), T![?]) {
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

    let inner = receiver_type(parser, RecvType::Dotted(UserType::All));
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

pub(super) enum RecvType {
    Dotted(UserType),
    NotDotted(UserType),
}

impl RecvType {
    fn user_type(&self) -> UserType {
        match self {
            RecvType::Dotted(ut) | RecvType::NotDotted(ut) => *ut,
        }
    }
}

#[derive(Clone, Copy)]
pub(super) enum UserType {
    /// parse `A.B.C` in `A.B.C()`
    All,
    /// parse `A.B` in `A.B.C()` or `A.` in `A.name: T`, useful before function name
    BeforeName,
}

pub(super) fn receiver_type(
    parser: &mut Parser<'_>,
    recv_type: RecvType,
) -> Option<CompletedMarker> {
    let m = parser.start();
    type_modifiers(parser);

    if let Some(cm) = nullable_type(parser, recv_type.user_type()) {
        if let RecvType::NotDotted(_) = recv_type {
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
        user_type(parser, UserType::All);
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
    if parser.at(T![suspend]) {
        let m = parser.start();
        parser.bump(T![suspend]);
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
    variance_modifier(parser).or_else(|| annotation(parser))
}

fn variance_modifier(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![in] | T![out] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, VARIANCE_MODIFIER))
        }
        _ => None,
    }
}
