use ast::syntax::SyntaxKind::*;
use tokens::Token;

use super::annotations::annotation;
use super::expressions::{expression, value_arguments};
use super::identifiers::simple_identifier;
use super::modifiers::{parameter_modifiers, parse_optional_modifiers, starts_modifiers};
use super::types::ty;
use super::utils::{starts_annotation, starts_simple_identifier};
use crate::{Parser, parse_loop, parse_while};

pub(crate) fn class_member_declarations(parser: &mut Parser<'_, '_>) {
    parser.start_node(CLASS_MEMBER_DECLARATIONS);
    parser.skip_trivia_and_newlines();

    parse_loop! { parser =>
        if !starts_class_member_declaration(parser) {
            break;
        }

        class_member_declaration(parser);
        parser.skip_trivia_and_newlines();

        if starts_semis(parser) {
            semis(parser);
            parser.skip_trivia_and_newlines();
        }
    }

    parser.finish_node(CLASS_MEMBER_DECLARATIONS);
}

pub(crate) fn class_member_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(CLASS_MEMBER_DECLARATION);
    let _had_modifiers = parse_optional_modifiers(parser);
    parser.skip_trivia_and_newlines();

    match parser.current_token() {
        Some(Token::COMPANION) => companion_object(parser),
        Some(Token::INIT) => anonymous_initializer(parser),
        Some(Token::CONSTRUCTOR) => secondary_constructor(parser),
        Some(Token::FUN) => function_declaration(parser),
        Some(Token::VAL | Token::VAR) => property_declaration(parser),
        Some(Token::OBJECT) => object_declaration(parser),
        Some(Token::ERR) => {
            parser
                .sink
                .error("expected class member declaration".into());
            parser.bump();
        }
        _ => {
            parser
                .sink
                .error("expected class member declaration".into());
            if parser.current_token().is_some() {
                parser.bump();
            }
        }
    }

    parser.finish_node(CLASS_MEMBER_DECLARATION);
}

fn anonymous_initializer(parser: &mut Parser<'_, '_>) {
    parser.start_node(ANONYMOUS_INITIALIZER);
    if parser.current_token() == Some(&Token::INIT) {
        parser.bump();
    } else {
        parser.sink.error("expected 'init'".into());
    }
    parser.skip_trivia_and_newlines();
    block(parser);
    parser.finish_node(ANONYMOUS_INITIALIZER);
}

fn companion_object(parser: &mut Parser<'_, '_>) {
    parser.start_node(COMPANION_OBJECT);

    if parser.current_token() == Some(&Token::COMPANION) {
        parser.bump();
    } else {
        parser.sink.error("expected 'companion'".into());
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::DATA) {
        parser.bump();
        parser.skip_trivia_and_newlines();
    }

    if parser.current_token() == Some(&Token::OBJECT) {
        parser.bump();
    } else {
        parser.sink.error("expected 'object'".into());
    }

    parser.skip_trivia_and_newlines();
    if starts_simple_identifier(parser) {
        simple_identifier(parser);
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
        parse_loop! { parser =>
            parser.skip_trivia_and_newlines();
            match parser.current_token() {
                Some(Token::L_CURL | Token::NL | Token::SEMICOLON) | None => break,
                _ => parser.bump(),
            }
        }
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_CURL) {
        class_body(parser);
    }

    parser.finish_node(COMPANION_OBJECT);
}

fn function_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(FUNCTION_DECLARATION);

    if parser.current_token() == Some(&Token::FUN) {
        parser.bump();
    } else {
        parser.sink.error("expected 'fun'".into());
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_ANGLE) {
        type_parameters(parser);
        parser.skip_trivia_and_newlines();
    }

    if looks_like_receiver_type(parser) {
        ty(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::DOT) {
            parser.bump();
            parser.skip_trivia_and_newlines();
        }
    }

    simple_identifier(parser);
    parser.skip_trivia_and_newlines();
    function_value_parameters(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        ty(parser);
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_CURL)
        || parser.current_token() == Some(&Token::ASSIGNMENT_TOKEN)
    {
        function_body(parser);
    }

    parser.finish_node(FUNCTION_DECLARATION);
}

fn function_body(parser: &mut Parser<'_, '_>) {
    parser.start_node(FUNCTION_BODY);
    match parser.current_token() {
        Some(Token::L_CURL) => block(parser),
        Some(Token::ASSIGNMENT_TOKEN) => {
            parser.bump();
            parser.skip_trivia_and_newlines();
            expression(parser);
        }
        _ => parser.sink.error("expected function body".into()),
    }
    parser.finish_node(FUNCTION_BODY);
}

fn function_value_parameters(parser: &mut Parser<'_, '_>) {
    parser.start_node(FUNCTION_VALUE_PARAMETERS);
    if parser.current_token() != Some(&Token::L_PAREN) {
        parser.sink.error("expected '(' for parameters".into());
        parser.finish_node(FUNCTION_VALUE_PARAMETERS);
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();

    parse_loop! { parser =>
        if matches!(parser.current_token(), Some(Token::R_PAREN) | None) {
            break;
        }
        function_value_parameter(parser);
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

    parser.finish_node(FUNCTION_VALUE_PARAMETERS);
}

fn function_value_parameter(parser: &mut Parser<'_, '_>) {
    parser.start_node(FUNCTION_VALUE_PARAMETER);
    parameter_modifiers(parser);
    parser.skip_trivia_and_newlines();
    parameter(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::ASSIGNMENT_TOKEN) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        expression(parser);
    }

    parser.finish_node(FUNCTION_VALUE_PARAMETER);
}

fn parameter(parser: &mut Parser<'_, '_>) {
    parser.start_node(PARAMETER);
    simple_identifier(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
    } else {
        parser.sink.error("expected ':'".into());
    }
    parser.skip_trivia_and_newlines();
    ty(parser);
    parser.finish_node(PARAMETER);
}

fn property_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(PROPERTY_DECLARATION);

    match parser.current_token() {
        Some(Token::VAL | Token::VAR) => parser.bump(),
        _ => parser.sink.error("expected 'val' or 'var'".into()),
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_ANGLE) {
        type_parameters(parser);
        parser.skip_trivia_and_newlines();
    }

    if looks_like_receiver_type(parser) {
        ty(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::DOT) {
            parser.bump();
            parser.skip_trivia_and_newlines();
        }
    }

    if parser.current_token() == Some(&Token::L_PAREN) {
        multi_variable_declaration(parser);
    } else {
        variable_declaration(parser);
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::ASSIGNMENT_TOKEN) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        expression(parser);
    } else if parser.current_token() == Some(&Token::BY) {
        property_delegate(parser);
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::SEMICOLON) {
        parser.bump();
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::GET) || starts_modifiers(parser) {
        getter(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::SET) || starts_modifiers(parser) {
            setter(parser);
        }
    } else if parser.current_token() == Some(&Token::SET) || starts_modifiers(parser) {
        setter(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::GET) || starts_modifiers(parser) {
            getter(parser);
        }
    }

    parser.finish_node(PROPERTY_DECLARATION);
}

fn property_delegate(parser: &mut Parser<'_, '_>) {
    parser.start_node(PROPERTY_DELEGATE);
    if parser.current_token() == Some(&Token::BY) {
        parser.bump();
    } else {
        parser.sink.error("expected 'by'".into());
    }
    parser.skip_trivia_and_newlines();
    expression(parser);
    parser.finish_node(PROPERTY_DELEGATE);
}

fn getter(parser: &mut Parser<'_, '_>) {
    parser.start_node(GETTER);
    parse_optional_modifiers(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::GET) {
        parser.bump();
    } else {
        parser.sink.error("expected 'get'".into());
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_PAREN) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::R_PAREN) {
            parser.bump();
        } else {
            parser.sink.error("expected ')'".into());
        }

        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::COLON) {
            parser.bump();
            parser.skip_trivia_and_newlines();
            ty(parser);
        }

        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::L_CURL)
            || parser.current_token() == Some(&Token::ASSIGNMENT_TOKEN)
        {
            function_body(parser);
        }
    }
    parser.finish_node(GETTER);
}

fn setter(parser: &mut Parser<'_, '_>) {
    parser.start_node(SETTER);
    parse_optional_modifiers(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::SET) {
        parser.bump();
    } else {
        parser.sink.error("expected 'set'".into());
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_PAREN) {
        parameters_with_optional_type(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::COLON) {
            parser.bump();
            parser.skip_trivia_and_newlines();
            ty(parser);
        }
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::L_CURL)
            || parser.current_token() == Some(&Token::ASSIGNMENT_TOKEN)
        {
            function_body(parser);
        }
    }

    parser.finish_node(SETTER);
}

fn parameters_with_optional_type(parser: &mut Parser<'_, '_>) {
    parser.start_node(PARAMETERS_WITH_OPTIONAL_TYPE);
    if parser.current_token() != Some(&Token::L_PAREN) {
        parser.sink.error("expected '('".into());
        parser.finish_node(PARAMETERS_WITH_OPTIONAL_TYPE);
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();
    parse_loop! { parser =>
        if matches!(parser.current_token(), Some(Token::R_PAREN) | None) {
            break;
        }
        function_value_parameter_with_optional_type(parser);
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

    parser.finish_node(PARAMETERS_WITH_OPTIONAL_TYPE);
}

fn function_value_parameter_with_optional_type(parser: &mut Parser<'_, '_>) {
    parser.start_node(FUNCTION_VALUE_PARAMETER_WITH_OPTIONAL_TYPE);
    parameter_modifiers(parser);
    parser.skip_trivia_and_newlines();
    parameter_with_optional_type(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::ASSIGNMENT_TOKEN) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        expression(parser);
    }
    parser.finish_node(FUNCTION_VALUE_PARAMETER_WITH_OPTIONAL_TYPE);
}

fn parameter_with_optional_type(parser: &mut Parser<'_, '_>) {
    parser.start_node(PARAMETER_WITH_OPTIONAL_TYPE);
    simple_identifier(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        ty(parser);
    }
    parser.finish_node(PARAMETER_WITH_OPTIONAL_TYPE);
}

fn object_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(OBJECT_DECLARATION);
    if parser.current_token() == Some(&Token::OBJECT) {
        parser.bump();
    } else {
        parser.sink.error("expected 'object'".into());
    }
    parser.skip_trivia_and_newlines();
    if starts_simple_identifier(parser) {
        simple_identifier(parser);
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_CURL) {
        class_body(parser);
    }

    parser.finish_node(OBJECT_DECLARATION);
}

fn secondary_constructor(parser: &mut Parser<'_, '_>) {
    parser.start_node(SECONDARY_CONSTRUCTOR);
    if parser.current_token() == Some(&Token::CONSTRUCTOR) {
        parser.bump();
    } else {
        parser.sink.error("expected 'constructor'".into());
    }
    parser.skip_trivia_and_newlines();
    function_value_parameters(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        constructor_delegation_call(parser);
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_CURL) {
        block(parser);
    }

    parser.finish_node(SECONDARY_CONSTRUCTOR);
}

fn constructor_delegation_call(parser: &mut Parser<'_, '_>) {
    parser.start_node(CONSTRUCTOR_DELEGATION_CALL);
    match parser.current_token() {
        Some(Token::THIS | Token::SUPER) => parser.bump(),
        _ => parser.sink.error("expected 'this' or 'super'".into()),
    }
    parser.skip_trivia_and_newlines();
    value_arguments(parser);
    parser.finish_node(CONSTRUCTOR_DELEGATION_CALL);
}

pub(crate) fn class_body(parser: &mut Parser<'_, '_>) {
    parser.start_node(CLASS_BODY);
    if parser.current_token() != Some(&Token::L_CURL) {
        parser.sink.error("expected '{'".into());
        parser.finish_node(CLASS_BODY);
        return;
    }
    parser.bump();
    parser.skip_trivia_and_newlines();
    class_member_declarations(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::R_CURL) {
        parser.bump();
    } else {
        parser.sink.error("expected '}'".into());
    }
    parser.finish_node(CLASS_BODY);
}

pub(crate) fn block(parser: &mut Parser<'_, '_>) {
    parser.start_node(BLOCK);
    if parser.current_token() != Some(&Token::L_CURL) {
        parser.sink.error("expected '{'".into());
        parser.finish_node(BLOCK);
        return;
    }
    let mut depth = 0i32;
    while let Some(tok) = parser.current_token() {
        match tok {
            Token::L_CURL => {
                depth += 1;
                parser.bump();
            }
            Token::R_CURL => {
                depth -= 1;
                parser.bump();
                if depth == 0 {
                    break;
                }
            }
            _ => parser.bump(),
        }
    }
    parser.finish_node(BLOCK);
}

pub(crate) fn variable_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(VARIABLE_DECLARATION);
    parse_while!(starts_annotation(parser), parser => {
        annotation(parser);
        parser.skip_trivia_and_newlines();
    });
    simple_identifier(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        ty(parser);
    }
    parser.finish_node(VARIABLE_DECLARATION);
}

pub(crate) fn multi_variable_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(MULTI_VARIABLE_DECLARATION);
    if parser.current_token() != Some(&Token::L_PAREN) {
        parser.sink.error("expected '('".into());
        parser.finish_node(MULTI_VARIABLE_DECLARATION);
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();

    parse_loop! { parser =>
        if matches!(parser.current_token(), Some(Token::R_PAREN) | None) {
            break;
        }
        variable_declaration(parser);
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

    parser.finish_node(MULTI_VARIABLE_DECLARATION);
}

pub(crate) fn semis(parser: &mut Parser<'_, '_>) {
    parser.start_node(SEMIS);
    parse_while!(starts_semis(parser), parser => {
        semi(parser);
    });
    parser.finish_node(SEMIS);
}

pub(crate) fn semi(parser: &mut Parser<'_, '_>) {
    parser.start_node(SEMI);
    match parser.current_token() {
        Some(Token::SEMICOLON | Token::NL) => parser.bump(),
        _ => parser.sink.error("expected ';' or newline".into()),
    }
    parser.finish_node(SEMI);
}

pub(crate) fn starts_semis(parser: &mut Parser<'_, '_>) -> bool {
    matches!(parser.current_token(), Some(Token::SEMICOLON | Token::NL))
}

fn starts_class_member_declaration(parser: &mut Parser<'_, '_>) -> bool {
    starts_modifiers(parser)
        || matches!(
            parser.current_token(),
            Some(
                Token::COMPANION
                    | Token::INIT
                    | Token::CONSTRUCTOR
                    | Token::FUN
                    | Token::VAL
                    | Token::VAR
                    | Token::OBJECT
            )
        )
}

fn type_parameters(parser: &mut Parser<'_, '_>) {
    parser.start_node(TYPE_PARAMETERS);
    if parser.current_token() != Some(&Token::L_ANGLE) {
        parser.sink.error("expected '<'".into());
        parser.finish_node(TYPE_PARAMETERS);
        return;
    }

    parser.bump();
    let mut depth = 1i32;
    while let Some(tok) = parser.current_token() {
        match tok {
            Token::L_ANGLE => {
                depth += 1;
                parser.bump();
            }
            Token::R_ANGLE => {
                depth -= 1;
                parser.bump();
                if depth == 0 {
                    break;
                }
            }
            _ => parser.bump(),
        }
    }

    parser.finish_node(TYPE_PARAMETERS);
}

fn looks_like_receiver_type(parser: &mut Parser<'_, '_>) -> bool {
    let mut idx = 0usize;
    let mut depth = 0i32;
    loop {
        match parser.lookahead_token(idx) {
            Some(Token::WS | Token::NL | Token::LINE_COMMENT | Token::DELIMITED_COMMENT) => {
                idx += 1;
            }
            Some(Token::L_PAREN) => {
                depth += 1;
                idx += 1;
            }
            Some(Token::R_PAREN) => {
                if depth > 0 {
                    depth -= 1;
                }
                idx += 1;
            }
            Some(Token::DOT) if depth == 0 => return true,
            Some(Token::EOF | Token::ERR) | None => return false,
            _ => idx += 1,
        }
    }
}
