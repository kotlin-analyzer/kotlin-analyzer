use ast::syntax::SyntaxKind::*;
use tokens::Token;

use crate::Parser;

pub(crate) fn simple_identifier(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia();
    match parser
        .current()
        .map(|sp| (sp.is_soft_keyword(), sp.token()))
    {
        Some((true, _) | (_, Token::IDENTIFIER_TOKEN)) => {
            parser.sink.start_node(SIMPLE_IDENTIFIER);
            parser.bump();
            parser.sink.finish_node();
        }
        Some((_, Token::ERR)) => {
            parser.sink.error("expected an identifier".into());
            parser.bump();
        }
        _ => {}
    }
}

pub(crate) fn identifier(parser: &mut Parser<'_, '_>) {
    parser.start_node(IDENTIFIER);
    simple_identifier(parser);
    loop {
        parser.skip_trivia_and_newlines();
        match parser.current().map(|t| t.token()) {
            Some(Token::ERR) => {
                parser.sink.error("expected an identifier".into());
                parser.bump();
            }
            Some(Token::DOT) => {
                parser.bump(); // consume '.'
                simple_identifier(parser);
            }
            _ => break,
        }
    }
    parser.finish_node(IDENTIFIER);
}
