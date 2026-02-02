use ast::syntax::SyntaxKind::*;
use tokens::Token;

use crate::Parser;

pub(crate) fn simple_identifier(parser: &mut Parser<'_, '_>) {
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
