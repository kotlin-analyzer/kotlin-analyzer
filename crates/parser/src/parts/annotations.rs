use ast::syntax::SyntaxKind::*;
use tokens::Token;

use crate::Parser;
use crate::parts::types::type_reference;
use crate::parts::utils::{skip_trivia_tokens, starts_use_site_target};

pub(crate) fn annotation(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();

    if !matches!(
        parser.current_token(),
        Some(Token::AT_NO_WS | Token::AT_PRE_WS)
    ) && !starts_use_site_target(parser)
    {
        parser.sink.error("expected annotation".into());
        return;
    }

    parser.start_node(ANNOTATION);

    if is_multi_annotation(parser) {
        multi_annotation(parser);
    } else {
        single_annotation(parser);
    }

    parser.skip_trivia_and_newlines();
    parser.finish_node(ANNOTATION);
}

fn single_annotation(parser: &mut Parser<'_, '_>) {
    parser.start_node(SINGLE_ANNOTATION);

    if starts_use_site_target(parser) {
        annotation_use_site_target(parser);
    } else if matches!(
        parser.current_token(),
        Some(Token::AT_NO_WS | Token::AT_PRE_WS)
    ) {
        parser.bump();
    } else {
        parser.sink.error("expected '@'".into());
    }

    parser.skip_trivia_and_newlines();
    unescaped_annotation(parser);

    parser.finish_node(SINGLE_ANNOTATION);
}

fn multi_annotation(parser: &mut Parser<'_, '_>) {
    parser.start_node(MULTI_ANNOTATION);

    if starts_use_site_target(parser) {
        annotation_use_site_target(parser);
    } else if matches!(
        parser.current_token(),
        Some(Token::AT_NO_WS | Token::AT_PRE_WS)
    ) {
        parser.bump();
    } else {
        parser.sink.error("expected '@'".into());
    }

    parser.skip_trivia_and_newlines();

    if parser.current_token() == Some(&Token::L_SQUARE) {
        parser.bump();
    } else {
        parser
            .sink
            .error("expected '[' to start annotation list".into());
    }

    parser.skip_trivia_and_newlines();

    let mut parsed_any = false;
    while !matches!(parser.current_token(), Some(Token::R_SQUARE) | None) {
        unescaped_annotation(parser);
        parsed_any = true;
        parser.skip_trivia_and_newlines();
    }

    if !parsed_any {
        parser
            .sink
            .error("expected at least one annotation inside brackets".into());
    }

    if parser.current_token() == Some(&Token::R_SQUARE) {
        parser.bump();
    } else {
        parser
            .sink
            .error("expected ']' to close annotation list".into());
    }

    parser.finish_node(MULTI_ANNOTATION);
}

fn annotation_use_site_target(parser: &mut Parser<'_, '_>) {
    parser.start_node(ANNOTATION_USE_SITE_TARGET);

    match parser.current_token() {
        Some(Token::AT_NO_WS | Token::AT_PRE_WS) => parser.bump(),
        _ => parser.sink.error("expected '@' for use-site target".into()),
    }

    parser.skip_trivia_and_newlines();

    if matches!(
        parser.current_token(),
        Some(
            Token::FIELD
                | Token::PROPERTY
                | Token::GET
                | Token::SET
                | Token::RECEIVER
                | Token::PARAM
                | Token::SET_PARAM
                | Token::DELEGATE
        )
    ) {
        parser.bump();
    } else {
        parser.sink.error("expected use-site target".into());
    }

    parser.skip_trivia_and_newlines();

    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
    } else {
        parser
            .sink
            .error("expected ':' after use-site target".into());
    }

    parser.finish_node(ANNOTATION_USE_SITE_TARGET);
}

pub(crate) fn unescaped_annotation(parser: &mut Parser<'_, '_>) {
    parser.start_node(UNESCAPED_ANNOTATION);
    parser.skip_trivia_and_newlines();

    // Parse userType via type_reference for now.
    // TODO: hanle constructor invocation
    if starts_user_type(parser) {
        type_reference(parser);
    } else {
        parser.sink.error("expected annotation type".into());
        if parser.current_token().is_some() {
            parser.bump();
        }
    }

    parser.finish_node(UNESCAPED_ANNOTATION);
}

fn starts_user_type(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser
            .current()
            .map(|sp| (sp.is_soft_keyword(), *sp.token())),
        Some((true, _)) | Some((_, Token::IDENTIFIER_TOKEN))
    )
}

fn is_multi_annotation(parser: &mut Parser<'_, '_>) -> bool {
    if !matches!(
        parser.current_token(),
        Some(Token::AT_NO_WS | Token::AT_PRE_WS)
    ) && !starts_use_site_target(parser)
    {
        return false;
    }

    let mut idx = 1usize;

    // If there's a use-site target, advance past it.
    if starts_use_site_target(parser) {
        // Skip '@'
        skip_trivia_tokens(parser, &mut idx);

        // target token
        idx += 1;
        skip_trivia_tokens(parser, &mut idx);

        // colon
        if matches!(parser.lookahead_token(idx), Some(Token::COLON)) {
            idx += 1;
        } else {
            return false;
        }
    }

    skip_trivia_tokens(parser, &mut idx);
    matches!(parser.lookahead_token(idx), Some(Token::L_SQUARE))
}
