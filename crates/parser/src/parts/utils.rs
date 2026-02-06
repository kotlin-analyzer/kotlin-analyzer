use tokens::Token;

use crate::Parser;

pub(crate) fn skip_trivia_tokens(parser: &mut Parser<'_, '_>, idx: &mut usize) {
    loop {
        match parser.lookahead_token(*idx) {
            Some(Token::WS | Token::NL | Token::LINE_COMMENT | Token::DELIMITED_COMMENT) => {
                *idx += 1;
            }
            _ => return,
        }
    }
}

pub(crate) fn starts_use_site_target(parser: &mut Parser<'_, '_>) -> bool {
    if !matches!(
        parser.current_token(),
        Some(Token::AT_NO_WS | Token::AT_PRE_WS)
    ) {
        return false;
    }

    let mut idx = 1usize;
    skip_trivia_tokens(parser, &mut idx);

    match parser.lookahead_token(idx) {
        Some(
            Token::FIELD
            | Token::PROPERTY
            | Token::GET
            | Token::SET
            | Token::RECEIVER
            | Token::PARAM
            | Token::SET_PARAM
            | Token::DELEGATE,
        ) => {
            idx += 1;
            skip_trivia_tokens(parser, &mut idx);
            matches!(parser.lookahead_token(idx), Some(Token::COLON))
        }
        _ => false,
    }
}

#[macro_export]
macro_rules! debug_loop {
    ($parser:expr => $($body:tt)*) => {
        let mut counter = 0;
        loop {
            if counter >= 100 {
                println!("Current token: {:?}", $parser.current_token());
                panic!("Debug loop exceeded 100 iterations");
            }
            counter += 1;
            $($body)*
        }
    };
}
