use ast::syntax::SyntaxKind::*;
use tokens::Token;

use crate::parts::types::user_type;
use crate::parts::utils::{skip_trivia_tokens, starts_use_site_target};
use crate::{Parser, parse_loop};

const ANNO_RECOVERY: &[Token] = &[
    Token::R_SQUARE,
    Token::SEMICOLON,
    Token::NL,
    Token::R_CURL,
    Token::EOF,
];

pub(crate) fn annotation(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();

    parser.start_node(ANNOTATION);
    parse_loop! { parser =>
        if !matches!(
            parser.current_token(),
            Some(Token::AT_NO_WS | Token::AT_PRE_WS)
        ) && !starts_use_site_target(parser)
        {
            parser.error("expected annotation");
            parser.recover_until(ANNO_RECOVERY);
            break;
        }

        if is_multi_annotation(parser) {
            multi_annotation(parser);
        } else {
            single_annotation(parser);
        }

        parser.skip_trivia_and_newlines();
        break;
    }
    parser.finish_node(ANNOTATION);
}

fn single_annotation(parser: &mut Parser<'_, '_>) {
    parser.start_node(SINGLE_ANNOTATION);
    parse_loop! { parser =>
        if starts_use_site_target(parser) {
            annotation_use_site_target(parser);
        } else if matches!(
            parser.current_token(),
            Some(Token::AT_NO_WS | Token::AT_PRE_WS)
        ) {
            parser.bump();
        } else {
            parser.error("expected '@'");
            parser.recover_until(ANNO_RECOVERY);
            break;
        }

        parser.skip_trivia_and_newlines();
         if !unescaped_annotation(parser) {
            parser.error("expected annotation");
        }
        break;
    }

    parser.finish_node(SINGLE_ANNOTATION);
}

fn multi_annotation(parser: &mut Parser<'_, '_>) {
    parser.start_node(MULTI_ANNOTATION);
    parse_loop! { parser =>
        if starts_use_site_target(parser) {
            annotation_use_site_target(parser);
        } else if matches!(
            parser.current_token(),
            Some(Token::AT_NO_WS | Token::AT_PRE_WS)
        ) {
            parser.bump();
        } else {
            parser.error("expected '@'");
            parser.recover_until(ANNO_RECOVERY);
            break;
        }

        parser.skip_trivia_and_newlines();

        if !parser.expect_recover(Token::L_SQUARE, "expected '[' to start annotation list", ANNO_RECOVERY) {
            break;
        }

        parser.skip_trivia_and_newlines();

        let mut parsed_any = false;
        while !matches!(parser.current_token(), Some(Token::R_SQUARE) | None) {
            if unescaped_annotation(parser) {
                parsed_any = true;
                parser.skip_trivia_and_newlines();
            }
        }

        if !parsed_any {
            parser.error("expected at least one annotation inside brackets");
        }

        if !parser.expect_recover(Token::R_SQUARE, "expected ']' to close annotation list", ANNO_RECOVERY) {
            break;
        }
        break;
    }

    parser.finish_node(MULTI_ANNOTATION);
}

fn annotation_use_site_target(parser: &mut Parser<'_, '_>) {
    parser.start_node(ANNOTATION_USE_SITE_TARGET);
    parse_loop! { parser =>
        match parser.current_token() {
            Some(Token::AT_NO_WS | Token::AT_PRE_WS) => parser.bump(),
            _ => {
                parser.error("expected '@' for use-site target");
                parser.recover_until(ANNO_RECOVERY);
                break;
            }
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
            parser.error("expected use-site target");
            parser.recover_until(ANNO_RECOVERY);
            break;
        }

        parser.skip_trivia_and_newlines();

        if !parser.expect_recover(Token::COLON, "expected ':' after use-site target", ANNO_RECOVERY) {
            break;
        }
        break;
    }

    parser.finish_node(ANNOTATION_USE_SITE_TARGET);
}

pub(crate) fn unescaped_annotation(parser: &mut Parser<'_, '_>) -> bool {
    parser.start_node(UNESCAPED_ANNOTATION);
    parser.skip_trivia_and_newlines();
    let mut successful = false;
    parse_loop! { parser =>
        // TODO: handle constructor invocation
        if starts_user_type(parser) {
            user_type(parser);
            successful = true;
        } else {
            parser.recover_until(ANNO_RECOVERY);
            successful = false;
        }
        break;
    }

    parser.finish_node(UNESCAPED_ANNOTATION);
    successful
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
