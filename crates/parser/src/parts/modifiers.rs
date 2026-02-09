use ast::syntax::SyntaxKind::*;
use tokens::Token;

use super::annotations::annotation;
use crate::Parser;
use crate::parts::utils::starts_annotation;

pub(crate) fn modifiers(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();

    if !starts_annotation(parser) && !starts_modifier(parser) {
        return;
    }

    parser.sink.start_node(MODIFIERS);

    while starts_annotation(parser) || starts_modifier(parser) {
        if starts_annotation(parser) {
            annotation(parser);
        } else {
            modifier(parser);
        }

        parser.skip_trivia_and_newlines();
    }

    parser.sink.finish_node();
}

pub(crate) fn parameter_modifiers(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();

    if !starts_annotation(parser) && !starts_parameter_modifier(parser) {
        return;
    }

    parser.sink.start_node(PARAMETER_MODIFIERS);

    while starts_annotation(parser) || starts_parameter_modifier(parser) {
        if starts_annotation(parser) {
            annotation(parser);
        } else {
            parameter_modifier(parser);
        }

        parser.skip_trivia_and_newlines();
    }

    parser.sink.finish_node();
}

pub(crate) fn type_parameter_modifiers(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();

    if !starts_type_parameter_modifier(parser) {
        return;
    }

    parser.sink.start_node(TYPE_PARAMETER_MODIFIERS);

    while starts_type_parameter_modifier(parser) {
        type_parameter_modifier(parser);
        parser.skip_trivia_and_newlines();
    }

    parser.sink.finish_node();
}

pub(crate) fn modifier(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia();
    parser.sink.start_node(MODIFIER);

    match parser.current_token() {
        Some(tok) if is_class_modifier(tok) => class_modifier(parser),
        Some(tok) if is_member_modifier(tok) => member_modifier(parser),
        Some(tok) if is_visibility_modifier(tok) => visibility_modifier(parser),
        Some(tok) if is_function_modifier(tok) => function_modifier(parser),
        Some(tok) if is_property_modifier(tok) => property_modifier(parser),
        Some(tok) if is_inheritance_modifier(tok) => inheritance_modifier(parser),
        Some(tok) if is_parameter_modifier(tok) => parameter_modifier(parser),
        Some(tok) if is_platform_modifier(tok) => platform_modifier(parser),
        Some(Token::ERR) => {
            parser.error("expected modifier");
            parser.bump();
        }
        Some(_) => {
            parser.error("expected modifier");
            parser.bump();
        }
        None => parser.error("expected modifier"),
    }

    parser.skip_trivia_and_newlines();
    parser.sink.finish_node();
}

fn type_parameter_modifier(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();
    parser.sink.start_node(TYPE_PARAMETER_MODIFIER);

    if starts_annotation(parser) {
        annotation(parser);
    } else {
        match parser.current_token() {
            Some(Token::REIFIED) => reification_modifier(parser),
            Some(Token::IN | Token::OUT) => variance_modifier(parser),
            Some(Token::ERR) => {
                parser.error("expected type parameter modifier");
                parser.bump();
            }
            Some(_) => {
                parser.error("expected type parameter modifier");
                parser.bump();
            }
            None => parser.error("expected type parameter modifier"),
        }
    }

    parser.skip_trivia_and_newlines();
    parser.sink.finish_node();
}

fn class_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(CLASS_MODIFIER);
    match parser.current_token() {
        Some(
            Token::ENUM
            | Token::SEALED
            | Token::ANNOTATION
            | Token::DATA
            | Token::INNER
            | Token::VALUE,
        ) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected class modifier");
            parser.bump();
        }
        _ => parser.error("expected class modifier"),
    }
    parser.sink.finish_node();
}

fn member_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(MEMBER_MODIFIER);
    match parser.current_token() {
        Some(Token::OVERRIDE | Token::LATEINIT) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected member modifier");
            parser.bump();
        }
        _ => parser.error("expected member modifier"),
    }
    parser.sink.finish_node();
}

fn visibility_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(VISIBILITY_MODIFIER);
    match parser.current_token() {
        Some(Token::PUBLIC | Token::PRIVATE | Token::INTERNAL | Token::PROTECTED) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected visibility modifier");
            parser.bump();
        }
        _ => parser.error("expected visibility modifier"),
    }
    parser.sink.finish_node();
}

fn function_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(FUNCTION_MODIFIER);
    match parser.current_token() {
        Some(
            Token::TAILREC
            | Token::OPERATOR
            | Token::INFIX
            | Token::INLINE
            | Token::EXTERNAL
            | Token::SUSPEND,
        ) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected function modifier");
            parser.bump();
        }
        _ => parser.error("expected function modifier"),
    }
    parser.sink.finish_node();
}

fn property_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(PROPERTY_MODIFIER);
    match parser.current_token() {
        Some(Token::CONST) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected property modifier");
            parser.bump();
        }
        _ => parser.error("expected property modifier"),
    }
    parser.sink.finish_node();
}

fn inheritance_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(INHERITANCE_MODIFIER);
    match parser.current_token() {
        Some(Token::ABSTRACT | Token::FINAL | Token::OPEN) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected inheritance modifier");
            parser.bump();
        }
        _ => parser.error("expected inheritance modifier"),
    }
    parser.sink.finish_node();
}

fn parameter_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(PARAMETER_MODIFIER);
    match parser.current_token() {
        Some(Token::VAR_ARG | Token::NO_INLINE | Token::CROSS_INLINE) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected parameter modifier");
            parser.bump();
        }
        _ => parser.error("expected parameter modifier"),
    }
    parser.sink.finish_node();
}

fn reification_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(REIFICATION_MODIFIER);
    match parser.current_token() {
        Some(Token::REIFIED) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected reification modifier");
            parser.bump();
        }
        _ => parser.error("expected reification modifier"),
    }
    parser.sink.finish_node();
}

fn variance_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(VARIANCE_MODIFIER);
    match parser.current_token() {
        Some(Token::IN | Token::OUT) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected variance modifier");
            parser.bump();
        }
        _ => parser.error("expected variance modifier"),
    }
    parser.sink.finish_node();
}

fn platform_modifier(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(PLATFORM_MODIFIER);
    match parser.current_token() {
        Some(Token::EXPECT | Token::ACTUAL) => parser.bump(),
        Some(Token::ERR) => {
            parser.error("expected platform modifier");
            parser.bump();
        }
        _ => parser.error("expected platform modifier"),
    }
    parser.sink.finish_node();
}

pub(crate) fn starts_modifiers(parser: &mut Parser<'_, '_>) -> bool {
    starts_annotation(parser) || starts_modifier(parser)
}

pub(crate) fn parse_optional_modifiers(parser: &mut Parser<'_, '_>) -> bool {
    if starts_modifiers(parser) {
        modifiers(parser);
        true
    } else {
        false
    }
}

fn starts_modifier(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser.current_token(),
        Some(
            Token::ENUM
                | Token::SEALED
                | Token::ANNOTATION
                | Token::DATA
                | Token::INNER
                | Token::VALUE
                | Token::OVERRIDE
                | Token::LATEINIT
                | Token::PUBLIC
                | Token::PRIVATE
                | Token::INTERNAL
                | Token::PROTECTED
                | Token::TAILREC
                | Token::OPERATOR
                | Token::INFIX
                | Token::INLINE
                | Token::EXTERNAL
                | Token::SUSPEND
                | Token::CONST
                | Token::ABSTRACT
                | Token::FINAL
                | Token::OPEN
                | Token::VAR_ARG
                | Token::NO_INLINE
                | Token::CROSS_INLINE
                | Token::EXPECT
                | Token::ACTUAL
        )
    )
}

fn starts_parameter_modifier(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser.current_token(),
        Some(Token::VAR_ARG | Token::NO_INLINE | Token::CROSS_INLINE)
    )
}

fn starts_type_parameter_modifier(parser: &mut Parser<'_, '_>) -> bool {
    starts_annotation(parser)
        || matches!(
            parser.current_token(),
            Some(Token::REIFIED | Token::IN | Token::OUT)
        )
}

fn is_class_modifier(token: &Token) -> bool {
    matches!(
        token,
        Token::ENUM | Token::SEALED | Token::ANNOTATION | Token::DATA | Token::INNER | Token::VALUE
    )
}

fn is_member_modifier(token: &Token) -> bool {
    matches!(token, Token::OVERRIDE | Token::LATEINIT)
}

fn is_visibility_modifier(token: &Token) -> bool {
    matches!(
        token,
        Token::PUBLIC | Token::PRIVATE | Token::INTERNAL | Token::PROTECTED
    )
}

fn is_function_modifier(token: &Token) -> bool {
    matches!(
        token,
        Token::TAILREC
            | Token::OPERATOR
            | Token::INFIX
            | Token::INLINE
            | Token::EXTERNAL
            | Token::SUSPEND
    )
}

fn is_property_modifier(token: &Token) -> bool {
    matches!(token, Token::CONST)
}

fn is_inheritance_modifier(token: &Token) -> bool {
    matches!(token, Token::ABSTRACT | Token::FINAL | Token::OPEN)
}

fn is_parameter_modifier(token: &Token) -> bool {
    matches!(
        token,
        Token::VAR_ARG | Token::NO_INLINE | Token::CROSS_INLINE
    )
}

fn is_platform_modifier(token: &Token) -> bool {
    matches!(token, Token::EXPECT | Token::ACTUAL)
}
