use ast::syntax::SyntaxKind::*;
use tokens::Token;

use crate::{Parser, parse_loop};

pub(crate) fn simple_identifier(parser: &mut Parser<'_, '_>) -> bool {
    parser.skip_trivia();
    match parser
        .current()
        .map(|sp| (sp.is_soft_keyword(), sp.token()))
    {
        Some((true, _) | (_, Token::IDENTIFIER_TOKEN)) => {
            parser.start_node(SIMPLE_IDENTIFIER);
            parser.bump();
            parser.finish_node(SIMPLE_IDENTIFIER);
            true
        }
        Some((_, Token::ERR)) => {
            parser.bump();
            false
        }
        _ => false,
    }
}

pub(crate) fn identifier(parser: &mut Parser<'_, '_>) {
    parser.start_node(IDENTIFIER);
    parse_loop! { parser =>
        if !simple_identifier(parser){
            break;
        };
        parse_loop! { parser =>
            parser.skip_trivia_and_newlines();
            match parser.current().map(|t| t.token()) {
                Some(Token::ERR) => {
                    parser.error("expected an identifier");
                    parser.bump();
                }
                Some(Token::DOT) => {
                    parser.bump(); // consume '.'
                    simple_identifier(parser);
                }
                _ => break,
            }
        }
        break;
    }
    parser.finish_node(IDENTIFIER);
}
