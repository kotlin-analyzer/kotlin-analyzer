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

pub(crate) fn starts_annotation(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser.current_token(),
        Some(Token::AT_NO_WS | Token::AT_PRE_WS)
    ) || starts_use_site_target(parser)
}

#[macro_export]
macro_rules! parse_loop {
    ($parser:expr => $($body:tt)*) => {
        #[cfg(debug_assertions)]
        let start_index = $parser.current_token_index();
        #[cfg(debug_assertions)]
        let mut counter = 0;
        loop {
            #[cfg(debug_assertions)]
            if counter >= 1 && start_index == $parser.current_token_index() {
                println!("Infinite loop detected. Current token: {:?}", $parser.current_token());
                break;
            }
            #[cfg(debug_assertions)]
            { counter += 1; }

            $($body)*
        }
    };
}

#[macro_export]
macro_rules! parse_while {
    ($condition:expr, $parser:expr => $($body:tt)*) => {
        parse_loop!($parser => {
            if $condition {
                $($body)*
            } else {
                break;
            }
        });
    };
}
