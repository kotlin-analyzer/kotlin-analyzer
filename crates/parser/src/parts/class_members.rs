use ast::syntax::SyntaxKind::*;
use tokens::Token;

use super::annotations::annotation;
use super::classes::type_parameters;
use super::expressions::{expression, value_arguments};
use super::identifiers::simple_identifier;
use super::modifiers::{parameter_modifiers, parse_optional_modifiers, starts_modifiers};
use super::types::ty;
use super::utils::{LIST_ITEM_RECOVERY, starts_annotation, starts_simple_identifier};
use crate::{Parser, parse_loop, parse_while};

const MEMBER_RECOVERY: &[Token] = &[Token::SEMICOLON, Token::NL, Token::R_CURL];
const PAREN_RECOVERY: &[Token] = &[Token::R_PAREN, Token::SEMICOLON, Token::NL, Token::R_CURL];
const BRACE_RECOVERY: &[Token] = &[Token::R_CURL, Token::SEMICOLON, Token::NL];

pub(crate) fn class_member_declarations(parser: &mut Parser<'_, '_>) {
    parser.start_node(CLASS_MEMBER_DECLARATIONS);
    parser.skip_trivia_and_newlines();

    parse_loop! { parser =>
        if !starts_class_member_declaration(parser) {
            break;
        }

        class_member_declaration(parser);
        parser.skip_trivia_and_newlines();

        if starts_semi(parser) {
            semis(parser);
            parser.skip_trivia_and_newlines();
        }
    }

    parser.finish_node(CLASS_MEMBER_DECLARATIONS);
}

pub(crate) fn class_member_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(CLASS_MEMBER_DECLARATION);
    parse_loop! { parser =>
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
                parser.error("expected class member declaration");
                parser.bump();
                parser.recover_until(MEMBER_RECOVERY);
                break;
            }
            _ => {
                parser.error("expected class member declaration");
                parser.recover_until(MEMBER_RECOVERY);
                break;
            }
        }
        break;
    }

    parser.finish_node(CLASS_MEMBER_DECLARATION);
}

fn anonymous_initializer(parser: &mut Parser<'_, '_>) {
    parser.start_node(ANONYMOUS_INITIALIZER);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::INIT, "expected 'init'", MEMBER_RECOVERY) {
            break;
        }
        parser.skip_trivia_and_newlines();
        block(parser);
        break;
    }
    parser.finish_node(ANONYMOUS_INITIALIZER);
}

fn companion_object(parser: &mut Parser<'_, '_>) {
    parser.start_node(COMPANION_OBJECT);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::COMPANION, "expected 'companion'", MEMBER_RECOVERY) {
            break;
        }

        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::DATA) {
            parser.bump();
            parser.skip_trivia_and_newlines();
        }

        if !parser.expect_recover(Token::OBJECT, "expected 'object'", MEMBER_RECOVERY) {
            break;
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
        break;
    }

    parser.finish_node(COMPANION_OBJECT);
}

pub(crate) fn function_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(FUNCTION_DECLARATION);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::FUN, "expected 'fun'", MEMBER_RECOVERY) {
            break;
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
        break;
    }

    parser.finish_node(FUNCTION_DECLARATION);
}

fn function_body(parser: &mut Parser<'_, '_>) {
    parser.start_node(FUNCTION_BODY);
    parse_loop! { parser =>
        match parser.current_token() {
            Some(Token::L_CURL) => block(parser),
            Some(Token::ASSIGNMENT_TOKEN) => {
                parser.bump();
                parser.skip_trivia_and_newlines();
                expression(parser);
            }
            _ => {
                parser.error("expected function body");
                parser.recover_until(BRACE_RECOVERY);
                break;
            }
        }
        break;
    }
    parser.finish_node(FUNCTION_BODY);
}

fn function_value_parameters(parser: &mut Parser<'_, '_>) {
    parser.start_node(FUNCTION_VALUE_PARAMETERS);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::L_PAREN, "expected '(' after function name", PAREN_RECOVERY) {
            if parser.current_token() == Some(&Token::R_PAREN) {
                // recover from empty parameter list without trying to parse parameters
                parser.bump();
            }
            break;
        }

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

        if !parser.expect_recover(Token::R_PAREN, "expected ')'", PAREN_RECOVERY) {
            break;
        }
        break;
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
    parse_loop! { parser =>
        if !simple_identifier(parser) {
            parser.error("expected parameter name or ')'");
            parser.recover_until(LIST_ITEM_RECOVERY);
            break;
        }
        parser.skip_trivia_and_newlines();
        if !parser.expect_recover(Token::COLON, "expected ':'", PAREN_RECOVERY) {
            break;
        }
        parser.skip_trivia_and_newlines();
        ty(parser);
        break;
    }
    parser.finish_node(PARAMETER);
}

pub(crate) fn property_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(PROPERTY_DECLARATION);
    parse_loop! { parser =>
        match parser.current_token() {
            Some(Token::VAL | Token::VAR) => parser.bump(),
            _ => {
                parser.error("expected 'val' or 'var'");
                parser.recover_until(MEMBER_RECOVERY);
                break;
            }
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
        break;
    }

    parser.finish_node(PROPERTY_DECLARATION);
}

fn property_delegate(parser: &mut Parser<'_, '_>) {
    parser.start_node(PROPERTY_DELEGATE);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::BY, "expected 'by'", MEMBER_RECOVERY) {
            break;
        }
        parser.skip_trivia_and_newlines();
        expression(parser);
        break;
    }
    parser.finish_node(PROPERTY_DELEGATE);
}

fn getter(parser: &mut Parser<'_, '_>) {
    parser.start_node(GETTER);
    parse_loop! { parser =>
        parse_optional_modifiers(parser);
        parser.skip_trivia_and_newlines();
        if !parser.expect_recover(Token::GET, "expected 'get'", MEMBER_RECOVERY) {
            break;
        }

        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::L_PAREN) {
            parser.bump();
            parser.skip_trivia_and_newlines();
            if !parser.expect_recover(Token::R_PAREN, "expected ')'", PAREN_RECOVERY) {
                break;
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
        break;
    }
    parser.finish_node(GETTER);
}

fn setter(parser: &mut Parser<'_, '_>) {
    parser.start_node(SETTER);
    parse_loop! { parser =>
        parse_optional_modifiers(parser);
        parser.skip_trivia_and_newlines();
        if !parser.expect_recover(Token::SET, "expected 'set'", MEMBER_RECOVERY) {
            break;
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
        break;
    }

    parser.finish_node(SETTER);
}

fn parameters_with_optional_type(parser: &mut Parser<'_, '_>) {
    parser.start_node(PARAMETERS_WITH_OPTIONAL_TYPE);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::L_PAREN, "expected '('", PAREN_RECOVERY) {
            break;
        }

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

        if !parser.expect_recover(Token::R_PAREN, "expected ')'", PAREN_RECOVERY) {
            break;
        }
        break;
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

pub(crate) fn object_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(OBJECT_DECLARATION);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::OBJECT, "expected 'object'", MEMBER_RECOVERY) {
            break;
        }
        parser.skip_trivia_and_newlines();
        if starts_simple_identifier(parser) {
            simple_identifier(parser);
        }

        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::L_CURL) {
            class_body(parser);
        }
        break;
    }

    parser.finish_node(OBJECT_DECLARATION);
}

fn secondary_constructor(parser: &mut Parser<'_, '_>) {
    parser.start_node(SECONDARY_CONSTRUCTOR);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::CONSTRUCTOR, "expected 'constructor'", MEMBER_RECOVERY) {
            break;
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
        break;
    }

    parser.finish_node(SECONDARY_CONSTRUCTOR);
}

fn constructor_delegation_call(parser: &mut Parser<'_, '_>) {
    parser.start_node(CONSTRUCTOR_DELEGATION_CALL);
    parse_loop! { parser =>
        match parser.current_token() {
            Some(Token::THIS | Token::SUPER) => parser.bump(),
            _ => {
                parser.error("expected 'this' or 'super'");
                parser.recover_until(PAREN_RECOVERY);
                break;
            }
        }
        parser.skip_trivia_and_newlines();
        value_arguments(parser);
        break;
    }
    parser.finish_node(CONSTRUCTOR_DELEGATION_CALL);
}

pub(crate) fn class_body(parser: &mut Parser<'_, '_>) {
    parser.start_node(CLASS_BODY);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::L_CURL, "expected '{'", BRACE_RECOVERY) {
            break;
        }
        parser.skip_trivia_and_newlines();
        class_member_declarations(parser);
        parser.skip_trivia_and_newlines();
        if !parser.expect_recover(Token::R_CURL, "expected '}'", BRACE_RECOVERY) {
            break;
        }
        break;
    }
    parser.finish_node(CLASS_BODY);
}

pub(crate) fn block(parser: &mut Parser<'_, '_>) {
    parser.start_node(BLOCK);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::L_CURL, "expected '{'", BRACE_RECOVERY) {
            break;
        }
        let mut depth = 1i32;
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
        break;
    }
    parser.finish_node(BLOCK);
}

pub(crate) fn variable_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(VARIABLE_DECLARATION);
    parse_loop! { parser =>
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
        break;
    }
    parser.finish_node(VARIABLE_DECLARATION);
}

pub(crate) fn multi_variable_declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(MULTI_VARIABLE_DECLARATION);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::L_PAREN, "expected '('", PAREN_RECOVERY) {
            break;
        }

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

        if !parser.expect_recover(Token::R_PAREN, "expected ')'", PAREN_RECOVERY) {
            break;
        }
        break;
    }

    parser.finish_node(MULTI_VARIABLE_DECLARATION);
}

pub(crate) fn semis(parser: &mut Parser<'_, '_>) -> bool {
    let mut had_semi = false;

    parser.skip_trivia();
    if starts_semi(parser) {
        parser.start_node(SEMIS);
        parse_while!(semi(parser), parser => {
            had_semi = true;
        });
        parser.finish_node(SEMIS);
    }
    had_semi
}

pub(crate) fn semi(parser: &mut Parser<'_, '_>) -> bool {
    parser.skip_trivia();
    let mut had_semi = false;
    if starts_semi(parser) {
        parser.start_node(SEMI);
        parser.bump();
        had_semi = true;
        parser.skip_trivia_and_newlines();
        parser.finish_node(SEMI);
    }
    had_semi
}

fn starts_semi(parser: &mut Parser<'_, '_>) -> bool {
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
