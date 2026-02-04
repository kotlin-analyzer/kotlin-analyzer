use ast::syntax::SyntaxKind::*;
use tokens::Token;

use super::identifiers::simple_identifier;
use crate::Parser;

macro_rules! trivia {
    () => {
        Token::WS | Token::NL | Token::LINE_COMMENT | Token::DELIMITED_COMMENT
    };
}

pub(crate) fn ty(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia();
    parser.sink.start_node(TYPE);

    type_modifiers(parser);

    let current = parser.current_token().copied();
    let paren_starts = matches!(current, Some(Token::L_PAREN));
    let looks_function = paren_starts && looks_like_function_type(parser);
    let looks_receiver_function = !paren_starts && looks_like_receiver_function_type(parser);
    let nullable_paren = paren_starts && has_nullable_suffix_after_paren(parser);
    let nullable_reference =
        starts_type_reference(parser) && has_nullable_suffix_after_reference(parser);

    match current {
        Some(Token::L_PAREN) if looks_function => function_type(parser),
        _ if looks_receiver_function => function_type(parser),
        Some(Token::L_PAREN) if nullable_paren => nullable_type(parser),
        Some(Token::L_PAREN) => parenthesized_type(parser),
        _ if nullable_reference => nullable_type(parser),
        _ if starts_type_reference(parser) => type_reference(parser),
        _ => parser.sink.error("expected a type".into()),
    }

    parser.sink.finish_node();
}

pub(crate) fn type_reference(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();

    let current = parser
        .current()
        .map(|sp| (sp.is_soft_keyword(), *sp.token()));
    let is_identifier_like = matches!(
        current,
        Some((true, _)) | Some((_, Token::IDENTIFIER_TOKEN))
    );

    match current {
        Some((_, Token::DYNAMIC)) => {
            parser.sink.start_node(TYPE_REFERENCE);
            parser.bump();
            parser.sink.finish_node();
        }
        _ if is_identifier_like => {
            parser.sink.start_node(TYPE_REFERENCE);
            user_type(parser);
            parser.sink.finish_node();
        }
        Some((_, Token::ERR)) => {
            parser.sink.error("expected a type reference".into());
            parser.bump();
        }
        _ => {}
    }
}

fn user_type(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(USER_TYPE);
    simple_user_type(parser);

    loop {
        parser.skip_trivia_and_newlines();
        match parser.current_token() {
            Some(Token::DOT) => {
                parser.bump();
                simple_user_type(parser);
            }
            Some(Token::ERR) => {
                parser.sink.error("expected a type name".into());
                parser.bump();
            }
            _ => break,
        }
    }

    parser.sink.finish_node();
}

fn simple_user_type(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();

    let current = parser
        .current()
        .map(|sp| (sp.is_soft_keyword(), *sp.token()));
    let is_identifier_like = matches!(
        current,
        Some((true, _)) | Some((_, Token::IDENTIFIER_TOKEN))
    );

    match current {
        _ if is_identifier_like => {
            parser.sink.start_node(SIMPLE_USER_TYPE);
            simple_identifier(parser);

            parser.skip_trivia_and_newlines();
            if parser.current_token() == Some(&Token::L_ANGLE) {
                type_arguments(parser);
            }

            parser.sink.finish_node();
        }
        Some((_, Token::ERR)) => {
            parser.sink.error("expected a type name".into());
            parser.bump();
        }
        _ => parser.sink.error("expected a type name".into()),
    }
}

fn type_projection(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();
    parser.sink.start_node(TYPE_PROJECTION);

    if parse_type_projection_modifiers(parser) {
        parser.skip_trivia_and_newlines();
    }

    match parser
        .current()
        .map(|sp| (sp.is_soft_keyword(), *sp.token()))
    {
        Some((_, Token::MULT)) => parser.bump(),
        _ if starts_type(parser) => ty(parser),
        Some((_, Token::ERR)) => {
            parser.sink.error("expected type projection".into());
            parser.bump();
        }
        _ => parser.sink.error("expected type projection".into()),
    }

    parser.sink.finish_node();
}

fn type_arguments(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(TYPE_ARGUMENTS);

    if parser.current_token() != Some(&Token::L_ANGLE) {
        parser
            .sink
            .error("expected `<` to start type arguments".into());
        parser.sink.finish_node();
        return;
    }

    parser.bump();

    loop {
        parser.skip_trivia_and_newlines();

        if matches!(parser.current_token(), Some(Token::R_ANGLE) | None) {
            break;
        }

        type_projection(parser);
        parser.skip_trivia_and_newlines();

        match parser.current_token() {
            Some(Token::COMMA) => {
                parser.bump();
            }
            _ => break,
        }
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::R_ANGLE) {
        parser.bump();
    } else {
        parser
            .sink
            .error("expected `>` to close type arguments".into());
    }

    parser.sink.finish_node();
}

fn nullable_type(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(NULLABLE_TYPE);

    if parser.current_token() == Some(&Token::L_PAREN) {
        parenthesized_type(parser);
    } else {
        type_reference(parser);
    }

    parser.skip_trivia_and_newlines();

    let mut found = false;
    while matches!(
        parser.current_token(),
        Some(Token::QUEST_NO_WS | Token::QUEST_WS)
    ) {
        found = true;
        parser.sink.start_node(QUEST);
        parser.bump();
        parser.sink.finish_node();
        parser.skip_trivia();
    }

    if !found {
        parser.sink.error("expected `?` for nullable type".into());
    }

    parser.sink.finish_node();
}

fn parenthesized_type(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(PARENTHESIZED_TYPE);

    if parser.current_token() != Some(&Token::L_PAREN) {
        parser.sink.error("expected `(`".into());
        parser.sink.finish_node();
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();

    ty(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::R_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected `)`".into());
    }

    parser.sink.finish_node();
}

fn function_type(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(FUNCTION_TYPE);

    // optional receiverType DOT
    if parser.current_token() != Some(&Token::L_PAREN) {
        receiver_type(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::DOT) {
            parser.bump();
        } else {
            parser.sink.error("expected `.` after receiver type".into());
        }
        parser.skip_trivia_and_newlines();
    }

    function_type_parameters(parser);

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::ARROW) {
        parser.bump();
    } else {
        parser.sink.error("expected `->` in function type".into());
    }

    parser.skip_trivia_and_newlines();
    ty(parser);

    parser.sink.finish_node();
}

fn function_type_parameters(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(FUNCTION_TYPE_PARAMETERS);

    if parser.current_token() != Some(&Token::L_PAREN) {
        parser
            .sink
            .error("expected `(` to start function type parameters".into());
        parser.sink.finish_node();
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();

    if parser.current_token() == Some(&Token::R_PAREN) {
        parser.bump();
        parser.sink.finish_node();
        return;
    }

    loop {
        parser.skip_trivia_and_newlines();

        if looks_like_parameter(parser) {
            parameter(parser);
        } else if starts_type(parser) {
            ty(parser);
        } else {
            parser.sink.error("expected parameter or type".into());
            if matches!(
                parser.current_token(),
                Some(Token::COMMA | Token::R_PAREN) | None
            ) {
                // avoid infinite loop
            } else {
                parser.bump();
            }
        }

        parser.skip_trivia_and_newlines();
        match parser.current_token() {
            Some(Token::COMMA) => {
                parser.bump();
                parser.skip_trivia_and_newlines();
                if parser.current_token() == Some(&Token::R_PAREN) {
                    break;
                }
            }
            _ => break,
        }
    }

    if parser.current_token() == Some(&Token::R_PAREN) {
        parser.bump();
    } else {
        parser
            .sink
            .error("expected `)` to end function type parameters".into());
    }

    parser.sink.finish_node();
}

fn parameter(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(PARAMETER);
    simple_identifier(parser);
    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::COLON) {
        parser.bump();
    } else {
        parser.sink.error("expected `:` in parameter".into());
    }
    parser.skip_trivia_and_newlines();
    ty(parser);
    parser.sink.finish_node();
}

fn receiver_type(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(RECEIVER_TYPE);
    type_modifiers(parser);

    let starts_reference = starts_type_reference(parser);
    let nullable_reference = starts_reference && has_nullable_suffix_after_reference(parser);

    match parser.current_token() {
        Some(Token::L_PAREN) => parenthesized_type(parser),
        _ if nullable_reference => nullable_type(parser),
        _ if starts_reference => type_reference(parser),
        _ => parser.sink.error("expected receiver type".into()),
    }

    parser.sink.finish_node();
}

fn parenthesized_user_type(parser: &mut Parser<'_, '_>) {
    parser.sink.start_node(PARENTHESIZED_USER_TYPE);

    if parser.current_token() != Some(&Token::L_PAREN) {
        parser.sink.error("expected `(`".into());
        parser.sink.finish_node();
        return;
    }

    parser.bump();
    parser.skip_trivia_and_newlines();

    if parser.current_token() == Some(&Token::L_PAREN) {
        parenthesized_user_type(parser);
    } else {
        user_type(parser);
    }

    parser.skip_trivia_and_newlines();
    if parser.current_token() == Some(&Token::R_PAREN) {
        parser.bump();
    } else {
        parser.sink.error("expected `)`".into());
    }

    parser.sink.finish_node();
}

fn starts_type_reference(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser
            .current()
            .map(|sp| (sp.is_soft_keyword(), *sp.token())),
        Some((_, Token::DYNAMIC)) | Some((true, _)) | Some((_, Token::IDENTIFIER_TOKEN))
    )
}

fn starts_type(parser: &mut Parser<'_, '_>) -> bool {
    matches!(
        parser.current_token(),
        Some(Token::L_PAREN | Token::SUSPEND)
    ) || starts_type_reference(parser)
}

fn type_modifiers(parser: &mut Parser<'_, '_>) {
    let mut collected = false;
    while parser.current_token() == Some(&Token::SUSPEND) {
        if !collected {
            parser.sink.start_node(TYPE_MODIFIERS);
            collected = true;
        }

        parser.sink.start_node(TYPE_MODIFIER);
        parser.bump();
        parser.sink.finish_node();
        parser.skip_trivia_and_newlines();
    }

    if collected {
        parser.sink.finish_node();
    }
}

fn parse_type_projection_modifiers(parser: &mut Parser<'_, '_>) -> bool {
    let mut collected = false;

    while matches!(parser.current_token(), Some(Token::IN | Token::OUT)) {
        if !collected {
            parser.sink.start_node(TYPE_PROJECTION_MODIFIERS);
            collected = true;
        }

        parser.sink.start_node(TYPE_PROJECTION_MODIFIER);
        parser.sink.start_node(VARIANCE_MODIFIER);
        parser.bump();
        parser.sink.finish_node();
        parser.sink.finish_node();

        parser.skip_trivia_and_newlines();
    }

    if collected {
        parser.sink.finish_node();
    }

    collected
}

fn has_nullable_suffix_after_paren(parser: &mut Parser<'_, '_>) -> bool {
    // Assumes current token is `(`. Walk to matching `)` and see whether a `?` follows.
    let mut depth = 0i32;
    let mut idx = 0usize;

    loop {
        let tok = match parser.lookahead_token(idx) {
            Some(t) => t,
            None => return false,
        };

        match tok {
            trivia!() => idx += 1,
            Token::L_PAREN => {
                depth += 1;
                idx += 1;
            }
            Token::R_PAREN => {
                depth -= 1;
                idx += 1;
                if depth == 0 {
                    while let Some(t) = parser.lookahead_token(idx) {
                        match t {
                            trivia!() => {
                                idx += 1;
                            }
                            Token::QUEST_NO_WS | Token::QUEST_WS => return true,
                            _ => return false,
                        }
                    }
                    return false;
                }
            }
            _ => idx += 1,
        }
    }
}

fn has_nullable_suffix_after_reference(parser: &mut Parser<'_, '_>) -> bool {
    let mut idx = 0usize;
    let mut angle_depth = 0i32;

    loop {
        let tok = match parser.lookahead_token(idx) {
            Some(t) => t,
            None => return false,
        };

        match tok {
            trivia!() => idx += 1,
            Token::L_ANGLE => {
                angle_depth += 1;
                idx += 1;
            }
            Token::R_ANGLE => {
                if angle_depth > 0 {
                    angle_depth -= 1;
                }
                idx += 1;
            }
            Token::COMMA if angle_depth > 0 => idx += 1,
            Token::DOT => idx += 1,
            Token::QUEST_NO_WS | Token::QUEST_WS if angle_depth == 0 => return true,
            Token::QUEST_NO_WS | Token::QUEST_WS => idx += 1,
            tok if angle_depth == 0
                && matches!(
                    tok,
                    Token::COMMA
                        | Token::R_PAREN
                        | Token::ARROW
                        | Token::SEMICOLON
                        | Token::COLON
                        | Token::EOF
                ) =>
            {
                return false;
            }
            _ => idx += 1,
        }
    }
}

fn looks_like_parameter(parser: &mut Parser<'_, '_>) -> bool {
    let current = parser
        .current()
        .map(|sp| (sp.is_soft_keyword(), *sp.token()));
    let ident_like = matches!(
        current,
        Some((true, _)) | Some((_, Token::IDENTIFIER_TOKEN))
    );
    if !ident_like {
        return false;
    }

    match next_non_trivia(parser, 1) {
        Some(Token::COLON) => true,
        _ => false,
    }
}

fn next_non_trivia(parser: &mut Parser<'_, '_>, mut offset: usize) -> Option<Token> {
    loop {
        match parser.lookahead_token(offset)? {
            trivia!() => {
                offset += 1;
            }
            tok => return Some(tok),
        }
    }
}

fn looks_like_function_type(parser: &mut Parser<'_, '_>) -> bool {
    arrow_after_paren(parser, 0)
}

fn arrow_after_paren(parser: &mut Parser<'_, '_>, mut idx: usize) -> bool {
    // Assumes the token at idx is `(`.
    let mut depth = 0i32;
    loop {
        let tok = match parser.lookahead_token(idx) {
            Some(t) => t,
            None => return false,
        };

        match tok {
            trivia!() => idx += 1,
            Token::L_PAREN => {
                depth += 1;
                idx += 1;
            }
            Token::R_PAREN => {
                depth -= 1;
                idx += 1;
                if depth == 0 {
                    while let Some(t) = parser.lookahead_token(idx) {
                        match t {
                            trivia!() => {
                                idx += 1;
                            }
                            Token::ARROW => return true,
                            _ => return false,
                        }
                    }
                    return false;
                }
            }
            _ => idx += 1,
        }
    }
}

fn looks_like_receiver_function_type(parser: &mut Parser<'_, '_>) -> bool {
    let mut idx = 0usize;
    let mut angle_depth = 0i32;

    loop {
        let tok = match parser.lookahead_token(idx) {
            Some(t) => t,
            None => return false,
        };

        match tok {
            trivia!() => idx += 1,
            Token::L_ANGLE => {
                angle_depth += 1;
                idx += 1;
            }
            Token::R_ANGLE => {
                if angle_depth > 0 {
                    angle_depth -= 1;
                }
                idx += 1;
            }
            Token::COMMA if angle_depth > 0 => idx += 1,
            Token::DOT if angle_depth == 0 => {
                idx += 1;
                while let Some(t) = parser.lookahead_token(idx) {
                    match t {
                        trivia!() => idx += 1,
                        Token::L_PAREN => return arrow_after_paren(parser, idx),
                        _ => return false,
                    }
                }
                return false;
            }
            Token::L_PAREN => return false,
            Token::EOF => return false,
            _ => idx += 1,
        }
    }
}
