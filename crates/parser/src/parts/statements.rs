use ast::syntax::SyntaxKind::*;
use tokens::Token;

use super::annotations::annotation;
use super::class_members::{
    block, multi_variable_declaration, semis, starts_semis, variable_declaration,
};
use super::expressions::{
    assignable_expression, directly_assignable_expression, expression, label, starts_label,
};
use super::modifiers::starts_modifiers;
use super::utils::{starts_annotation, starts_simple_identifier};
use crate::{Parser, parse_loop, parse_while};

pub(crate) fn statements(parser: &mut Parser<'_, '_>) {
    parser.start_node(STATEMENTS);
    parser.skip_trivia_and_newlines();

    if starts_statement(parser) {
        statement(parser);
        parser.skip_trivia();

        parse_loop! { parser =>
            if !starts_semis(parser) {
                break;
            }
            semis(parser);
            parser.skip_trivia();
            if starts_statement(parser) {
                statement(parser);
                parser.skip_trivia();
                continue;
            }
            break;
        }
    }

    parser.skip_trivia();
    if starts_semis(parser) {
        semis(parser);
    }

    parser.finish_node(STATEMENTS);
}

pub(crate) fn statement(parser: &mut Parser<'_, '_>) {
    parser.start_node(STATEMENT);

    parse_loop! { parser =>
        if starts_label(parser) {
            label(parser);
            parser.skip_trivia_and_newlines();
            continue;
        }
        if starts_annotation(parser) {
            annotation(parser);
            parser.skip_trivia_and_newlines();
            continue;
        }
        break;
    }

    if starts_loop_statement(parser) {
        loop_statement(parser);
    } else if looks_like_assignment(parser) {
        assignment(parser);
    } else if starts_declaration(parser) {
        // Best-effort declaration support: parse as assignment/expression when unsupported.
        // For now, fall back to expression parsing.
        expression(parser);
    } else {
        expression(parser);
    }

    parser.finish_node(STATEMENT);
}

fn control_structure_body(parser: &mut Parser<'_, '_>) {
    parser.start_node(CONTROL_STRUCTURE_BODY);
    if parser.current_token() == Some(&Token::L_CURL) {
        block(parser);
    } else {
        statement(parser);
    }
    parser.finish_node(CONTROL_STRUCTURE_BODY);
}

fn loop_statement(parser: &mut Parser<'_, '_>) {
    parser.start_node(LOOP_STATEMENT);
    match parser.current_token() {
        Some(Token::FOR) => for_statement(parser),
        Some(Token::WHILE) => while_statement(parser),
        Some(Token::DO) => do_while_statement(parser),
        _ => parser.sink.error("expected loop statement".into()),
    }
    parser.finish_node(LOOP_STATEMENT);
}

fn for_statement(parser: &mut Parser<'_, '_>) {
    parser.start_node(FOR_STATEMENT);
    if parser.current_token() == Some(&Token::FOR) {
        parser.bump();
    } else {
        parser.sink.error("expected 'for'".into());
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected '('".into());
    }

    parser.skip_trivia_and_newlines();
    parse_while!(starts_annotation(parser), parser => {
        annotation(parser);
        parser.skip_trivia_and_newlines();
    });

    if parser.current_token() == Some(&Token::L_PAREN) {
        multi_variable_declaration(parser);
    } else {
        variable_declaration(parser);
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::IN) {
        parser.bump();
    } else {
        parser.sink.error("expected 'in'".into());
    }

    parser.skip_trivia_and_newlines();
    expression(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::R_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected ')'".into());
    }

    parser.skip_trivia_and_newlines();
    if starts_statement(parser) || parser.current_token() == Some(&Token::L_CURL) {
        control_structure_body(parser);
    }

    parser.finish_node(FOR_STATEMENT);
}

fn while_statement(parser: &mut Parser<'_, '_>) {
    parser.start_node(WHILE_STATEMENT);
    if parser.current_token() == Some(&Token::WHILE) {
        parser.bump();
    } else {
        parser.sink.error("expected 'while'".into());
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected '('".into());
    }

    parser.skip_trivia_and_newlines();
    expression(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::R_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected ')'".into());
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::SEMICOLON) {
        parser.bump();
    } else if starts_statement(parser) || parser.current_token() == Some(&Token::L_CURL) {
        control_structure_body(parser);
    }

    parser.finish_node(WHILE_STATEMENT);
}

fn do_while_statement(parser: &mut Parser<'_, '_>) {
    parser.start_node(DO_WHILE_STATEMENT);
    if parser.current_token() == Some(&Token::DO) {
        parser.bump();
    } else {
        parser.sink.error("expected 'do'".into());
    }

    parser.skip_trivia_and_newlines();
    if starts_statement(parser) || parser.current_token() == Some(&Token::L_CURL) {
        control_structure_body(parser);
        parser.skip_trivia_and_newlines();
    }

    if parser.current_token() == Some(&Token::WHILE) {
        parser.bump();
    } else {
        parser.sink.error("expected 'while'".into());
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::L_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected '('".into());
    }

    parser.skip_trivia_and_newlines();
    expression(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::R_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected ')'".into());
    }

    parser.finish_node(DO_WHILE_STATEMENT);
}

fn assignment(parser: &mut Parser<'_, '_>) {
    parser.start_node(ASSIGNMENT);

    if is_direct_assignment(parser) {
        directly_assignable_expression(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::ASSIGNMENT_TOKEN) {
            parser.bump();
        } else {
            parser.sink.error("expected '='".into());
        }
    } else {
        assignable_expression(parser);
        parser.skip_trivia_and_newlines();
        assignment_and_operator(parser);
    }

    parser.skip_trivia_and_newlines();
    expression(parser);

    parser.finish_node(ASSIGNMENT);
}

fn assignment_and_operator(parser: &mut Parser<'_, '_>) {
    parser.start_node(ASSIGNMENT_AND_OPERATOR);
    match parser.current_token() {
        Some(
            Token::ADD_ASSIGNMENT
            | Token::SUB_ASSIGNMENT
            | Token::MULT_ASSIGNMENT
            | Token::DIV_ASSIGNMENT
            | Token::MOD_ASSIGNMENT,
        ) => parser.bump(),
        _ => parser.sink.error("expected assignment operator".into()),
    }
    parser.finish_node(ASSIGNMENT_AND_OPERATOR);
}

fn assignment_operator(parser: &mut Parser<'_, '_>) -> Option<Token> {
    let mut idx = 0usize;
    let mut paren_depth = 0i32;
    let mut bracket_depth = 0i32;

    loop {
        match parser.lookahead_token(idx) {
            Some(Token::WS | Token::NL | Token::LINE_COMMENT | Token::DELIMITED_COMMENT) => {
                idx += 1;
            }
            Some(Token::L_PAREN) => {
                paren_depth += 1;
                idx += 1;
            }
            Some(Token::R_PAREN) => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                }
                idx += 1;
            }
            Some(Token::L_SQUARE) => {
                bracket_depth += 1;
                idx += 1;
            }
            Some(Token::R_SQUARE) => {
                if bracket_depth > 0 {
                    bracket_depth -= 1;
                }
                idx += 1;
            }
            Some(Token::ASSIGNMENT_TOKEN) if paren_depth == 0 && bracket_depth == 0 => {
                return Some(Token::ASSIGNMENT_TOKEN);
            }
            Some(
                Token::ADD_ASSIGNMENT
                | Token::SUB_ASSIGNMENT
                | Token::MULT_ASSIGNMENT
                | Token::DIV_ASSIGNMENT
                | Token::MOD_ASSIGNMENT,
            ) if paren_depth == 0 && bracket_depth == 0 => return parser.lookahead_token(idx),
            Some(Token::SEMICOLON | Token::R_CURL | Token::EOF) => return None,
            None => return None,
            _ => idx += 1,
        }
    }
}

fn is_direct_assignment(parser: &mut Parser<'_, '_>) -> bool {
    matches!(assignment_operator(parser), Some(Token::ASSIGNMENT_TOKEN))
}

fn starts_loop_statement(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser.current_token(),
        Some(Token::FOR | Token::WHILE | Token::DO)
    )
}

fn starts_statement(parser: &mut Parser<'_, '_>) -> bool {
    starts_label(parser)
        || starts_annotation(parser)
        || starts_loop_statement(parser)
        || looks_like_assignment(parser)
        || starts_expression_start(parser)
        || starts_declaration(parser)
}

fn starts_declaration(parser: &mut Parser<'_, '_>) -> bool {
    starts_modifiers(parser)
        || matches!(
            parser.current_token(),
            Some(
                Token::CLASS
                    | Token::OBJECT
                    | Token::FUN
                    | Token::VAL
                    | Token::VAR
                    | Token::TYPE_ALIAS
            )
        )
}

fn starts_expression_start(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser.current_token(),
        Some(
            Token::L_PAREN
                | Token::L_CURL
                | Token::INTEGER_LITERAL
                | Token::REAL_LITERAL
                | Token::HEX_LITERAL
                | Token::BIN_LITERAL
                | Token::LONG_LITERAL
                | Token::BOOLEAN_LITERAL
                | Token::NULL_LITERAL
                | Token::CHARACTER_LITERAL
                | Token::QUOTE_OPEN
                | Token::TRIPLE_QUOTE_OPEN
                | Token::THIS
                | Token::THIS_AT
                | Token::SUPER
                | Token::SUPER_AT
                | Token::INCR
                | Token::DECR
                | Token::SUB
                | Token::ADD
                | Token::EXCL_NO_WS
                | Token::EXCL_WS
                | Token::DATA
                | Token::OBJECT
        )
    ) || starts_simple_identifier(parser)
}

fn looks_like_assignment(parser: &mut Parser<'_, '_>) -> bool {
    assignment_operator(parser).is_some()
}
