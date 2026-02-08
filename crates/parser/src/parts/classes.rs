use ast::syntax::SyntaxKind::*;
use tokens::Token;

use super::annotations::annotation;
use super::class_members::class_body;
use super::enum_classes::enum_class_body;
use super::expressions::{expression, value_arguments};
use super::identifiers::simple_identifier;
use super::modifiers::{
    modifiers, parse_optional_modifiers, starts_modifiers, type_parameter_modifiers,
};
use super::types::{ty, type_reference};
use super::utils::{starts_annotation, starts_simple_identifier};
use crate::{Parser, parse_loop, parse_while};

pub(crate) fn class_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(CLASS_DECLARATION);

    parse_optional_modifiers(parser);
    parser.skip_trivia_and_newlines();

    match parser.current_token() {
        Some(Token::CLASS) => parser.bump(),
        Some(Token::FUN) => {
            parser.bump();
            parser.skip_trivia_and_newlines();
            if parser.current_token() == Some(&Token::INTERFACE) {
                parser.bump();
            } else {
                parser.sink.error("expected 'interface'".into());
            }
        }
        Some(Token::INTERFACE) => parser.bump(),
        _ => parser.sink.error("expected 'class' or 'interface'".into()),
    }

    parser.skip_trivia_and_newlines();
    simple_identifier(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_ANGLE) {
        type_parameters(parser);
        parser.skip_trivia_and_newlines();
    }

    if parser.current_token() == Some(&Token::L_PAREN)
        || starts_modifiers(parser)
        || parser.current_token() == Some(&Token::CONSTRUCTOR)
    {
        primary_constructor(parser);
        parser.skip_trivia_and_newlines();
    }

    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        delegation_specifiers(parser);
        parser.skip_trivia_and_newlines();
    }

    if parser.current_token() == Some(&Token::WHERE) {
        type_constraints(parser);
        parser.skip_trivia_and_newlines();
    }

    if looks_like_enum_body(parser) {
        enum_class_body(parser);
    } else if parser.current_token() == Some(&Token::L_CURL) {
        class_body(parser);
    }

    parser.finish_node(CLASS_DECLARATION);
}

pub(crate) fn primary_constructor(parser: &mut Parser<'_, '_>) {
    parser.start_node(PRIMARY_CONSTRUCTOR);

    if starts_modifiers(parser) {
        modifiers(parser);
        parser.skip_trivia_and_newlines();
    }

    if parser.current_token() == Some(&Token::CONSTRUCTOR) {
        parser.bump();
        parser.skip_trivia_and_newlines();
    }

    class_parameters(parser);
    parser.finish_node(PRIMARY_CONSTRUCTOR);
}

pub(crate) fn class_parameters(parser: &mut Parser<'_, '_>) {
    parser.start_node(CLASS_PARAMETERS);

    if parser.current_token() != Some(&Token::L_PAREN) {
        parser.sink.error("expected '('".into());
        parser.finish_node(CLASS_PARAMETERS);
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();

    parse_loop! { parser =>
        if matches!(parser.current_token(), Some(Token::R_PAREN) | None) {
            break;
        }
        class_parameter(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::COMMA) {
            parser.bump();
            parser.skip_trivia_and_newlines();
            continue;
        }
        break;
    }

    if parser.current_token() == Some(&Token::R_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected ')'".into());
    }

    parser.finish_node(CLASS_PARAMETERS);
}

fn class_parameter(parser: &mut Parser<'_, '_>) {
    parser.start_node(CLASS_PARAMETER);
    parse_optional_modifiers(parser);
    parser.skip_trivia_and_newlines();

    if matches!(parser.current_token(), Some(Token::VAL | Token::VAR)) {
        parser.bump();
        parser.skip_trivia_and_newlines();
    }

    simple_identifier(parser);
    parser.skip_trivia_and_newlines();

    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
    } else {
        parser.sink.error("expected ':'".into());
    }

    parser.skip_trivia_and_newlines();
    ty(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::ASSIGNMENT_TOKEN) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        expression(parser);
    }

    parser.finish_node(CLASS_PARAMETER);
}

fn delegation_specifiers(parser: &mut Parser<'_, '_>) {
    parser.start_node(DELEGATION_SPECIFIERS);
    annotated_delegation_specifier(parser);

    parse_while!(parser.current_token() == Some(&Token::COMMA), parser => {
        parser.bump();
        parser.skip_trivia_and_newlines();
        annotated_delegation_specifier(parser);
    });

    parser.finish_node(DELEGATION_SPECIFIERS);
}

fn annotated_delegation_specifier(parser: &mut Parser<'_, '_>) {
    parser.start_node(ANNOTATED_DELEGATION_SPECIFIER);
    parse_while!(starts_annotation(parser), parser => {
        annotation(parser);
        parser.skip_trivia_and_newlines();
    });
    delegation_specifier(parser);
    parser.finish_node(ANNOTATED_DELEGATION_SPECIFIER);
}

fn delegation_specifier(parser: &mut Parser<'_, '_>) {
    parser.start_node(DELEGATION_SPECIFIER);

    if parser.current_token() == Some(&Token::SUSPEND) {
        parser.bump();
        parser.skip_trivia_and_newlines();
    }

    if looks_like_explicit_delegation(parser) {
        explicit_delegation(parser);
    } else if looks_like_constructor_invocation(parser) {
        constructor_invocation(parser);
    } else {
        type_reference(parser);
    }

    parser.finish_node(DELEGATION_SPECIFIER);
}

fn constructor_invocation(parser: &mut Parser<'_, '_>) {
    parser.start_node(CONSTRUCTOR_INVOCATION);
    type_reference(parser);
    parser.skip_trivia_and_newlines();
    value_arguments(parser);
    parser.finish_node(CONSTRUCTOR_INVOCATION);
}

fn explicit_delegation(parser: &mut Parser<'_, '_>) {
    parser.start_node(EXPLICIT_DELEGATION);
    type_reference(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::BY) {
        parser.bump();
    } else {
        parser.sink.error("expected 'by'".into());
    }
    parser.skip_trivia_and_newlines();
    expression(parser);
    parser.finish_node(EXPLICIT_DELEGATION);
}

pub(crate) fn type_parameters(parser: &mut Parser<'_, '_>) {
    parser.start_node(TYPE_PARAMETERS);
    if parser.current_token() != Some(&Token::L_ANGLE) {
        parser.sink.error("expected '<'".into());
        parser.finish_node(TYPE_PARAMETERS);
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::R_ANGLE) {
        parser.bump();
        parser.finish_node(TYPE_PARAMETERS);
        return;
    }

    parse_loop! { parser =>
        type_parameter(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::COMMA) {
            parser.bump();
            parser.skip_trivia_and_newlines();
            if parser.current_token() == Some(&Token::R_ANGLE) {
                break;
            }
            continue;
        }
        break;
    }

    if parser.current_token() == Some(&Token::R_ANGLE) {
        parser.bump();
    } else {
        parser.sink.error("expected '>'".into());
    }

    parser.finish_node(TYPE_PARAMETERS);
}

fn type_parameter(parser: &mut Parser<'_, '_>) {
    parser.start_node(TYPE_PARAMETER);
    type_parameter_modifiers(parser);
    parser.skip_trivia_and_newlines();
    simple_identifier(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        ty(parser);
    }
    parser.finish_node(TYPE_PARAMETER);
}

fn type_constraints(parser: &mut Parser<'_, '_>) {
    parser.start_node(TYPE_CONSTRAINTS);
    if parser.current_token() == Some(&Token::WHERE) {
        parser.bump();
    } else {
        parser.sink.error("expected 'where'".into());
    }

    parse_loop! { parser =>
        parser.skip_trivia_and_newlines();
        type_constraint(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::COMMA) {
            parser.bump();
            continue;
        }
        break;
    }

    parser.finish_node(TYPE_CONSTRAINTS);
}

fn type_constraint(parser: &mut Parser<'_, '_>) {
    parser.start_node(TYPE_CONSTRAINT);
    parse_while!(starts_annotation(parser), parser => {
        annotation(parser);
        parser.skip_trivia_and_newlines();
    });
    simple_identifier(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
    } else {
        parser.sink.error("expected ':'".into());
    }
    parser.skip_trivia_and_newlines();
    ty(parser);
    parser.finish_node(TYPE_CONSTRAINT);
}

fn looks_like_constructor_invocation(parser: &mut Parser<'_, '_>) -> bool {
    if !starts_simple_identifier(parser) {
        return false;
    }
    let mut idx = 0usize;
    loop {
        match parser.lookahead_token(idx) {
            Some(Token::WS | Token::NL | Token::LINE_COMMENT | Token::DELIMITED_COMMENT) => {
                idx += 1
            }
            Some(Token::L_PAREN) => return true,
            Some(Token::COLON | Token::COMMA | Token::R_CURL | Token::EOF) | None => return false,
            _ => idx += 1,
        }
    }
}

fn looks_like_explicit_delegation(parser: &mut Parser<'_, '_>) -> bool {
    if !starts_simple_identifier(parser) {
        return false;
    }
    let mut idx = 0usize;
    loop {
        match parser.lookahead_token(idx) {
            Some(Token::WS | Token::NL | Token::LINE_COMMENT | Token::DELIMITED_COMMENT) => {
                idx += 1
            }
            Some(Token::BY) => return true,
            Some(Token::COMMA | Token::R_CURL | Token::EOF) | None => return false,
            _ => idx += 1,
        }
    }
}

fn looks_like_enum_body(parser: &mut Parser<'_, '_>) -> bool {
    if parser.current_token() != Some(&Token::L_CURL) {
        return false;
    }

    let mut idx = 1usize;
    loop {
        match parser.lookahead_token(idx) {
            Some(Token::WS | Token::NL | Token::LINE_COMMENT | Token::DELIMITED_COMMENT) => {
                idx += 1
            }
            Some(
                Token::IDENTIFIER_TOKEN | Token::AT_NO_WS | Token::AT_PRE_WS | Token::AT_BOTH_WS,
            ) => return true,
            _ => return false,
        }
    }
}
