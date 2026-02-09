use tokens::Token;

use crate::Parser;
pub const LIST_ITEM_RECOVERY: &[Token] =
    &[Token::COMMA, Token::R_PAREN, Token::SEMICOLON, Token::NL];

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

pub(crate) fn starts_simple_identifier(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser
            .current()
            .map(|sp| (sp.is_soft_keyword(), *sp.token())),
        Some((true, _)) | Some((_, Token::IDENTIFIER_TOKEN))
    )
}

#[macro_export]
macro_rules! parse_loop {
    ($parser:expr => $($body:tt)*) => {{
        let mut last_index = $parser.current_token_index();
        let mut counter = 0;
        loop {

            if counter >= 1 && last_index == $parser.current_token_index() {
                if cfg!(debug_assertions) {
                    panic!("Infinite loop detected. Current token: {:?}", $parser.current_token());
                } else {
                    break;
                }
            }
            #[allow(unused_assignments)]
            {
                counter += 1;
                last_index = $parser.current_token_index();
            }

            $($body)*
        }
    }};
}

#[macro_export]
macro_rules! parse_while {
    ($condition:expr, $parser:expr => $($body:tt)*) => {
        $crate::parse_loop!($parser => {
            if $condition {
                $($body)*
            } else {
                break;
            }
        });
    };
}

#[macro_export]
macro_rules! trivia {
    () => {
        Token::WS | Token::NL | Token::LINE_COMMENT | Token::DELIMITED_COMMENT
    };
}
