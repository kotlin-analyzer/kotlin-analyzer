use ast::syntax::SyntaxKind::*;
use tokens::Token;

use super::annotations::annotation;
use super::identifiers::simple_identifier;
use super::types::{ty, type_arguments};
use super::utils::{skip_trivia_tokens, starts_annotation, starts_simple_identifier};
use crate::{Parser, parse_loop, parse_while};

pub(crate) fn expression(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();
    parser.start_node(EXPRESSION);
    disjunction(parser);
    parser.finish_node(EXPRESSION);
}

fn disjunction(parser: &mut Parser<'_, '_>) {
    parser.start_node(DISJUNCTION);
    conjunction(parser);
    parser.skip_trivia_and_newlines();

    parse_while!(parser.current_token() == Some(&Token::DISJ), parser => {
        parser.bump();
        parser.skip_trivia_and_newlines();
        conjunction(parser);
    });

    parser.finish_node(DISJUNCTION);
}

fn conjunction(parser: &mut Parser<'_, '_>) {
    parser.start_node(CONJUNCTION);
    equality(parser);
    parser.skip_trivia_and_newlines();

    parse_while!(parser.current_token() == Some(&Token::CONJ), parser => {
        parser.bump();
        parser.skip_trivia_and_newlines();
        equality(parser);
    });

    parser.finish_node(CONJUNCTION);
}

fn equality(parser: &mut Parser<'_, '_>) {
    parser.start_node(EQUALITY);
    comparison(parser);
    parser.skip_trivia_and_newlines();

    parse_while!(
        matches!(
            parser.current_token(),
            Some(Token::EXCL_EQ | Token::EXCL_EQ_EQ | Token::EQ_EQ | Token::EQ_EQ_EQ)
        ),
        parser => {
            parser.start_node(EQUALITY_OPERATOR);
            parser.bump();
            parser.finish_node(EQUALITY_OPERATOR);

            parser.skip_trivia_and_newlines();
            comparison(parser);
        }
    );

    parser.finish_node(EQUALITY);
}

fn comparison(parser: &mut Parser<'_, '_>) {
    parser.start_node(COMPARISON);
    generic_call_like_comparison(parser);
    parser.skip_trivia_and_newlines();

    parse_while!(
        matches!(
            parser.current_token(),
            Some(Token::L_ANGLE | Token::R_ANGLE | Token::LE | Token::GE)
        ),
        parser => {
            parser.start_node(COMPARISON_OPERATOR);
            parser.bump();
            parser.finish_node(COMPARISON_OPERATOR);

            parser.skip_trivia_and_newlines();
            generic_call_like_comparison(parser);
        }
    );

    parser.finish_node(COMPARISON);
}

fn generic_call_like_comparison(parser: &mut Parser<'_, '_>) {
    parser.start_node(GENERIC_CALL_LIKE_COMPARISON);
    infix_operation(parser);
    parser.skip_trivia_and_newlines();

    parse_while!(starts_call_suffix(parser), parser => {
        call_suffix(parser);
    });

    parser.finish_node(GENERIC_CALL_LIKE_COMPARISON);
}

fn infix_operation(parser: &mut Parser<'_, '_>) {
    parser.start_node(INFIX_OPERATION);
    elvis_expression(parser);

    parse_loop! { parser =>
        parser.skip_trivia_and_newlines();
        match parser.current_token() {
            Some(Token::IN | Token::NOT_IN) => {
                parser.start_node(IN_OPERATOR);
                parser.bump();
               parser.finish_node(IN_OPERATOR);
                parser.skip_trivia_and_newlines();
                elvis_expression(parser);
            }
            Some(Token::IS | Token::NOT_IS) => {
                parser.start_node(IS_OPERATOR);
                parser.bump();
               parser.finish_node(IS_OPERATOR);
                parser.skip_trivia_and_newlines();
                ty(parser);
            }
            _ => break,
        }
    }

    parser.finish_node(INFIX_OPERATION);
}

fn elvis_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(ELVIS_EXPRESSION);
    infix_function_call(parser);

    parse_while!(parser.current_token() == Some(&Token::QUEST_NO_WS), parser => {
        let mut idx = 1usize;
        parse_while!(matches!(parser.lookahead_token(idx), Some(Token::WS | Token::NL)), parser => {
            idx += 1;
        });
        if parser.lookahead_token(idx) != Some(Token::COLON) {
            break;
        }

        parser.start_node(ELVIS);
        parser.bump();
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::COLON) {
            parser.bump();
        } else {
            parser.sink.error("expected ':' in elvis operator".into());
        }
        parser.finish_node(ELVIS);

        parser.skip_trivia_and_newlines();
        infix_function_call(parser);
    });

    parser.finish_node(ELVIS_EXPRESSION);
}

fn infix_function_call(parser: &mut Parser<'_, '_>) {
    parser.start_node(INFIX_FUNCTION_CALL);
    range_expression(parser);
    let mut i = 0;

    parse_loop! { parser =>
        parser.skip_trivia();
        if !starts_simple_identifier(parser) {
            break;
        }
        if i > 2 {
            // To prevent infinite loops in case of parser errors, we limit the number of infix calls.
            println!("Too many infix calls, breaking to prevent infinite loop");
            println!("Current token: {:?}", parser.current_token());
            break;
        }
        i += 1;
        simple_identifier(parser);
        parser.skip_trivia_and_newlines();
        range_expression(parser);
    }

    parser.finish_node(INFIX_FUNCTION_CALL);
}

fn range_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(RANGE_EXPRESSION);
    additive_expression(parser);
    parser.skip_trivia();

    parse_while!(
        matches!(
            parser.current_token(),
            Some(Token::RANGE | Token::RANGE_LESS)
        ),
        parser => {
            parser.bump();
            parser.skip_trivia_and_newlines();
            additive_expression(parser);
            parser.skip_trivia();
        }
    );

    parser.finish_node(RANGE_EXPRESSION);
}

fn additive_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(ADDITIVE_EXPRESSION);
    multiplicative_expression(parser);
    parser.skip_trivia();

    parse_while!(matches!(parser.current_token(), Some(Token::ADD | Token::SUB)), parser => {
        parser.start_node(ADDITIVE_OPERATOR);
        parser.bump();
        parser.finish_node(ADDITIVE_OPERATOR);

        parser.skip_trivia_and_newlines();
        multiplicative_expression(parser);
        parser.skip_trivia();
    });

    parser.finish_node(ADDITIVE_EXPRESSION);
}

fn multiplicative_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(MULTIPLICATIVE_EXPRESSION);
    as_expression(parser);
    parser.skip_trivia();

    parse_while!(
        matches!(
            parser.current_token(),
            Some(Token::MULT | Token::DIV | Token::MOD)
        ),
        parser => {
            parser.start_node(MULTIPLICATIVE_OPERATOR);
            parser.bump();
            parser.finish_node(MULTIPLICATIVE_OPERATOR);

            parser.skip_trivia_and_newlines();
            as_expression(parser);
            parser.skip_trivia();
        }
    );

    parser.finish_node(MULTIPLICATIVE_EXPRESSION);
}

fn as_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(AS_EXPRESSION);
    prefix_unary_expression(parser);
    parser.skip_trivia_and_newlines();

    parse_while!(matches!(parser.current_token(), Some(Token::AS | Token::AS_SAFE)), parser => {
        parser.start_node(AS_OPERATOR);
        parser.bump();
        parser.finish_node(AS_OPERATOR);
        parser.skip_trivia_and_newlines();
        ty(parser);
    });

    parser.finish_node(AS_EXPRESSION);
}

fn prefix_unary_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(PREFIX_UNARY_EXPRESSION);

    parse_while!(starts_unary_prefix(parser), parser => {
        unary_prefix(parser);
    });

    postfix_unary_expression(parser);
    parser.finish_node(PREFIX_UNARY_EXPRESSION);
}

fn unary_prefix(parser: &mut Parser<'_, '_>) {
    parser.start_node(UNARY_PREFIX);

    if starts_annotation(parser) {
        annotation(parser);
    } else if starts_label(parser) {
        label(parser);
    } else {
        prefix_unary_operator(parser);
    }

    parser.finish_node(UNARY_PREFIX);
}

fn postfix_unary_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(POSTFIX_UNARY_EXPRESSION);
    primary_expression(parser);
    parser.skip_trivia_and_newlines();

    parse_while!(starts_postfix_unary_suffix(parser), parser => {
        postfix_unary_suffix(parser);
        parser.skip_trivia_and_newlines();
    });

    parser.finish_node(POSTFIX_UNARY_EXPRESSION);
}

fn postfix_unary_suffix(parser: &mut Parser<'_, '_>) {
    parser.start_node(POSTFIX_UNARY_SUFFIX);
    println!(
        "Checking for postfix unary suffix at token: {:?}",
        parser.current_token()
    );

    if starts_postfix_unary_operator(parser) {
        postfix_unary_operator(parser);
    } else if starts_call_suffix(parser) {
        call_suffix(parser);
    } else if parser.current_token() == Some(&Token::L_ANGLE) {
        type_arguments(parser);
    } else if parser.current_token() == Some(&Token::L_SQUARE) {
        indexing_suffix(parser);
    } else {
        navigation_suffix(parser);
    }

    parser.finish_node(POSTFIX_UNARY_SUFFIX);
}

pub(crate) fn directly_assignable_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(DIRECTLY_ASSIGNABLE_EXPRESSION);
    parser.skip_trivia_and_newlines();

    if parser.current_token() == Some(&Token::L_PAREN) {
        parenthesized_directly_assignable_expression(parser);
    } else if starts_simple_identifier(parser) {
        simple_identifier(parser);
    } else {
        postfix_unary_expression(parser);
        assignable_suffix(parser);
    }

    parser.finish_node(DIRECTLY_ASSIGNABLE_EXPRESSION);
}

fn parenthesized_directly_assignable_expression(parser: &mut Parser<'_, '_>) {
    parser
        .sink
        .start_node(PARENTHESIZED_DIRECTLY_ASSIGNABLE_EXPRESSION);

    if parser.current_token() == Some(&Token::L_PAREN) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        directly_assignable_expression(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::R_PAREN) {
            parser.bump();
        } else {
            parser.sink.error("expected ')'".into());
        }
    } else {
        parser.sink.error("expected '('".into());
    }

    parser.finish_node(PARENTHESIZED_DIRECTLY_ASSIGNABLE_EXPRESSION);
}

pub(crate) fn assignable_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(ASSIGNABLE_EXPRESSION);
    if parser.current_token() == Some(&Token::L_PAREN) {
        parenthesized_assignable_expression(parser);
    } else {
        prefix_unary_expression(parser);
    }
    parser.finish_node(ASSIGNABLE_EXPRESSION);
}

fn parenthesized_assignable_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(PARENTHESIZED_ASSIGNABLE_EXPRESSION);

    if parser.current_token() == Some(&Token::L_PAREN) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        assignable_expression(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::R_PAREN) {
            parser.bump();
        } else {
            parser.sink.error("expected ')'".into());
        }
    } else {
        parser.sink.error("expected '('".into());
    }

    parser.finish_node(PARENTHESIZED_ASSIGNABLE_EXPRESSION);
}

fn assignable_suffix(parser: &mut Parser<'_, '_>) {
    parser.start_node(ASSIGNABLE_SUFFIX);
    match parser.current_token() {
        Some(Token::L_ANGLE) => type_arguments(parser),
        Some(Token::L_SQUARE) => indexing_suffix(parser),
        _ => navigation_suffix(parser),
    }
    parser.finish_node(ASSIGNABLE_SUFFIX);
}

fn indexing_suffix(parser: &mut Parser<'_, '_>) {
    parser.start_node(INDEXING_SUFFIX);
    if parser.current_token() != Some(&Token::L_SQUARE) {
        parser.sink.error("expected '['".into());
        parser.finish_node(INDEXING_SUFFIX);
        return;
    }

    parser.bump();
    parse_loop! { parser =>
        parser.skip_trivia_and_newlines();
        if matches!(parser.current_token(), Some(Token::R_SQUARE) | None) {
            break;
        }
        expression(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::COMMA) {
            parser.bump();
            continue;
        }
        break;
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::R_SQUARE) {
        parser.bump();
    } else {
        parser.sink.error("expected ']'".into());
    }

    parser.finish_node(INDEXING_SUFFIX);
}

fn navigation_suffix(parser: &mut Parser<'_, '_>) {
    parser.start_node(NAVIGATION_SUFFIX);
    member_access_operator(parser);
    parser.skip_trivia_and_newlines();

    match parser.current_token() {
        Some(Token::CLASS) => parser.bump(),
        Some(Token::L_PAREN) => parenthesized_expression(parser),
        Some(Token::L_SQUARE) => collection_literal(parser),
        _ => simple_identifier(parser),
    }

    parser.finish_node(NAVIGATION_SUFFIX);
}

fn call_suffix(parser: &mut Parser<'_, '_>) {
    parser.start_node(CALL_SUFFIX);

    if parser.current_token() == Some(&Token::L_ANGLE) {
        type_arguments(parser);
        parser.skip_trivia_and_newlines();
    }

    let mut parsed_value_args = false;
    if parser.current_token() == Some(&Token::L_PAREN) {
        value_arguments(parser);
        parsed_value_args = true;
        parser.skip_trivia_and_newlines();
    }

    if starts_annotated_lambda(parser) {
        annotated_lambda(parser);
    } else if !parsed_value_args && parser.current_token() == Some(&Token::L_PAREN) {
        value_arguments(parser);
    }

    parser.finish_node(CALL_SUFFIX);
}

fn annotated_lambda(parser: &mut Parser<'_, '_>) {
    parser.start_node(ANNOTATED_LAMBDA);

    parse_while!(starts_annotation(parser), parser => {
        annotation(parser);
    });

    if starts_label(parser) {
        label(parser);
    }

    parser.skip_trivia_and_newlines();
    lambda_literal(parser);

    parser.finish_node(ANNOTATED_LAMBDA);
}

fn lambda_literal(parser: &mut Parser<'_, '_>) {
    parser.start_node(LAMBDA_LITERAL);
    if parser.current_token() != Some(&Token::L_CURL) {
        parser.sink.error("expected '{' to start lambda".into());
        parser.finish_node(LAMBDA_LITERAL);
        return;
    }

    // This is a minimal lambda parser that consumes until matching '}' for test purposes.
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

    parser.finish_node(LAMBDA_LITERAL);
}

pub(crate) fn value_arguments(parser: &mut Parser<'_, '_>) {
    parser.start_node(VALUE_ARGUMENTS);

    if parser.current_token() != Some(&Token::L_PAREN) {
        parser.sink.error("expected '(' for value arguments".into());
        parser.finish_node(VALUE_ARGUMENTS);
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();

    parse_loop! { parser =>
        if matches!(parser.current_token(), Some(Token::R_PAREN) | None) {
            break;
        }

        value_argument(parser);
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
        parser.sink.error("expected ')' to close arguments".into());
    }

    parser.finish_node(VALUE_ARGUMENTS);
}

fn value_argument(parser: &mut Parser<'_, '_>) {
    parser.start_node(VALUE_ARGUMENT);

    if starts_annotation(parser) {
        annotation(parser);
    }

    parser.skip_trivia_and_newlines();

    if starts_simple_identifier(parser) {
        let mut idx = 1usize;
        parse_while!(matches!(parser.lookahead_token(idx), Some(Token::WS | Token::NL)), parser => {
            idx += 1;
        });
        if parser.lookahead_token(idx) == Some(Token::ASSIGNMENT_TOKEN) {
            simple_identifier(parser);
            parser.skip_trivia_and_newlines();
            if parser.current_token() == Some(&Token::ASSIGNMENT_TOKEN) {
                parser.bump();
            }
            parser.skip_trivia_and_newlines();
        }
    }

    if parser.current_token() == Some(&Token::MULT) {
        parser.bump();
    }

    expression(parser);
    parser.finish_node(VALUE_ARGUMENT);
}

fn primary_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(PRIMARY_EXPRESSION);
    parser.skip_trivia_and_newlines();

    let current = parser
        .current()
        .map(|sp| (sp.is_soft_keyword(), *sp.token()));
    let token_only = current.map(|(_, tok)| tok);
    let is_identifier_like = matches!(
        current,
        Some((true, _)) | Some((_, Token::IDENTIFIER_TOKEN))
    );

    match token_only {
        Some(Token::L_PAREN) => parenthesized_expression(parser),
        Some(Token::L_SQUARE) => collection_literal(parser),
        Some(
            Token::INTEGER_LITERAL
            | Token::REAL_LITERAL
            | Token::HEX_LITERAL
            | Token::BIN_LITERAL
            | Token::LONG_LITERAL
            | Token::BOOLEAN_LITERAL
            | Token::NULL_LITERAL
            | Token::CHARACTER_LITERAL,
        ) => literal_constant(parser),
        Some(Token::QUOTE_OPEN | Token::TRIPLE_QUOTE_OPEN) => string_literal(parser),
        Some(Token::THIS | Token::THIS_AT) => this_expression(parser),
        Some(Token::SUPER | Token::SUPER_AT) => super_expression(parser),
        _ if is_identifier_like => simple_identifier(parser),
        Some(Token::L_CURL) => lambda_literal(parser),
        Some(Token::DATA) => object_literal(parser),
        Some(Token::OBJECT) => object_literal(parser),
        _ => {
            parser.sink.error("expected primary expression".into());
            if parser.current_token().is_some() {
                parser.bump();
            }
        }
    }

    parser.finish_node(PRIMARY_EXPRESSION);
}

fn parenthesized_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(PARENTHESIZED_EXPRESSION);
    if parser.current_token() != Some(&Token::L_PAREN) {
        parser.sink.error("expected '('".into());
        parser.finish_node(PARENTHESIZED_EXPRESSION);
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();
    expression(parser);
    parser.skip_trivia_and_newlines();

    if parser.current_token() == Some(&Token::R_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected ')'".into());
    }

    parser.finish_node(PARENTHESIZED_EXPRESSION);
}

fn collection_literal(parser: &mut Parser<'_, '_>) {
    parser.start_node(COLLECTION_LITERAL);
    if parser.current_token() != Some(&Token::L_SQUARE) {
        parser.sink.error("expected '['".into());
        parser.finish_node(COLLECTION_LITERAL);
        return;
    }

    parser.bump();
    parse_loop! { parser =>
        parser.skip_trivia_and_newlines();
        if matches!(parser.current_token(), Some(Token::R_SQUARE) | None) {
            break;
        }
        expression(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::COMMA) {
            parser.bump();
            continue;
        }
        break;
    }

    if parser.current_token() == Some(&Token::R_SQUARE) {
        parser.bump();
    } else {
        parser.sink.error("expected ']'".into());
    }

    parser.finish_node(COLLECTION_LITERAL);
}

fn literal_constant(parser: &mut Parser<'_, '_>) {
    parser.start_node(LITERAL_CONSTANT);
    parser.bump();
    parser.finish_node(LITERAL_CONSTANT);
}

fn string_literal(parser: &mut Parser<'_, '_>) {
    parser.start_node(STRING_LITERAL);
    parser.bump();
    while let Some(tok) = parser.current_token() {
        match tok {
            Token::QUOTE_CLOSE | Token::TRIPLE_QUOTE_CLOSE => {
                parser.bump();
                break;
            }
            Token::EOF => break,
            _ => parser.bump(),
        }
    }
    parser.finish_node(STRING_LITERAL);
}

fn this_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(THIS_EXPRESSION);
    parser.bump();
    parser.finish_node(THIS_EXPRESSION);
}

fn super_expression(parser: &mut Parser<'_, '_>) {
    parser.start_node(SUPER_EXPRESSION);
    parser.bump();
    parser.finish_node(SUPER_EXPRESSION);
}

fn object_literal(parser: &mut Parser<'_, '_>) {
    parser.start_node(OBJECT_LITERAL);

    if parser.current_token() == Some(&Token::DATA) {
        parser.bump();
        parser.skip_trivia_and_newlines();
    }

    if parser.current_token() == Some(&Token::OBJECT) {
        parser.bump();
    } else {
        parser.sink.error("expected 'object'".into());
    }

    parser.finish_node(OBJECT_LITERAL);
}

pub(crate) fn label(parser: &mut Parser<'_, '_>) {
    parser.start_node(LABEL);
    simple_identifier(parser);
    parser.skip_trivia_and_newlines();
    match parser.current_token() {
        Some(Token::AT_NO_WS | Token::AT_POST_WS) => parser.bump(),
        _ => parser.sink.error("expected '@' in label".into()),
    }
    parser.finish_node(LABEL);
}

fn prefix_unary_operator(parser: &mut Parser<'_, '_>) {
    parser.start_node(PREFIX_UNARY_OPERATOR);
    match parser.current_token() {
        Some(Token::INCR | Token::DECR | Token::SUB | Token::ADD) => parser.bump(),
        Some(Token::EXCL_NO_WS | Token::EXCL_WS) => parser.bump(),
        _ => parser.sink.error("expected prefix unary operator".into()),
    }
    parser.finish_node(PREFIX_UNARY_OPERATOR);
}

fn postfix_unary_operator(parser: &mut Parser<'_, '_>) {
    parser.start_node(POSTFIX_UNARY_OPERATOR);
    match parser.current_token() {
        Some(Token::INCR | Token::DECR) => parser.bump(),
        Some(Token::EXCL_NO_WS) => {
            parser.bump();
            match parser.current_token() {
                Some(Token::EXCL_NO_WS | Token::EXCL_WS) => parser.bump(),
                _ => parser.sink.error("expected second '!'".into()),
            }
        }
        _ => parser.sink.error("expected postfix unary operator".into()),
    }
    parser.finish_node(POSTFIX_UNARY_OPERATOR);
}

fn member_access_operator(parser: &mut Parser<'_, '_>) {
    parser.start_node(MEMBER_ACCESS_OPERATOR);
    match parser.current_token() {
        Some(Token::DOT) => parser.bump(),
        Some(Token::COLON_COLON) => parser.bump(),
        Some(Token::QUEST_NO_WS) => {
            let idx = 1usize;
            if parser.lookahead_token(idx) == Some(Token::DOT) {
                parser.start_node(SAFE_NAV);
                parser.bump();
                parser.bump();
                parser.finish_node(SAFE_NAV);
            } else {
                parser.sink.error("expected '.' after '?'".into());
            }
        }
        _ => parser.sink.error("expected member access".into()),
    }
    parser.finish_node(MEMBER_ACCESS_OPERATOR);
}

fn starts_call_suffix(parser: &mut Parser<'_, '_>) -> bool {
    match parser.current_token() {
        Some(Token::L_PAREN | Token::L_CURL) => true,
        Some(Token::L_ANGLE) => {
            let mut idx = 0usize;
            let mut depth = 0i32;

            loop {
                match parser.lookahead_token(idx) {
                    Some(Token::L_ANGLE) => {
                        depth += 1;
                        idx += 1;
                    }
                    Some(Token::R_ANGLE) => {
                        depth -= 1;
                        idx += 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    Some(Token::EOF | Token::ERR) | None => return false,
                    _ => idx += 1,
                }
            }

            skip_trivia_tokens(parser, &mut idx);
            matches!(
                parser.lookahead_token(idx),
                Some(
                    Token::L_PAREN
                        | Token::L_CURL
                        | Token::AT_NO_WS
                        | Token::AT_PRE_WS
                        | Token::AT_POST_WS
                        | Token::AT_BOTH_WS
                )
            )
        }
        _ => false,
    }
}

fn starts_annotated_lambda(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser.current_token(),
        Some(Token::L_CURL | Token::AT_NO_WS | Token::AT_PRE_WS)
    )
}

pub(crate) fn starts_label(parser: &mut Parser<'_, '_>) -> bool {
    if !starts_simple_identifier(parser) {
        return false;
    }
    let mut idx = 1usize;
    while matches!(parser.lookahead_token(idx), Some(Token::WS | Token::NL)) {
        idx += 1;
    }
    matches!(
        parser.lookahead_token(idx),
        Some(Token::AT_NO_WS | Token::AT_POST_WS)
    )
}

fn starts_unary_prefix(parser: &mut Parser<'_, '_>) -> bool {
    starts_annotation(parser)
        || starts_label(parser)
        || matches!(
            parser.current_token(),
            Some(
                Token::INCR
                    | Token::DECR
                    | Token::SUB
                    | Token::ADD
                    | Token::EXCL_NO_WS
                    | Token::EXCL_WS
            )
        )
}

fn starts_postfix_unary_suffix(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser.current_token(),
        Some(
            Token::INCR
                | Token::DECR
                | Token::EXCL_NO_WS
                | Token::L_ANGLE
                | Token::L_PAREN
                | Token::L_SQUARE
                | Token::DOT
                | Token::COLON_COLON
                | Token::QUEST_NO_WS
        )
    ) && 
    // To avoid ambiguity with the elvis operator, we need to ensure that a '?' is not followed by a ':'.
    !matches!(
        parser.next_two_tokens(),
        Some((Token::QUEST_NO_WS, Token::COLON))
    )
}

fn starts_postfix_unary_operator(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser.current_token(),
        Some(Token::INCR | Token::DECR | Token::EXCL_NO_WS)
    )
}
