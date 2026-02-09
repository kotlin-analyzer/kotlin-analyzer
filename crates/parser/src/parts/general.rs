use ast::syntax::SyntaxKind::*;
use tokens::Token;

use super::annotations::unescaped_annotation;
use super::class_members::{
    function_declaration, object_declaration, property_declaration, semi, semis,
};
use super::classes::{class_declaration, type_parameters};
use super::identifiers::{identifier, simple_identifier};
use super::modifiers::{parse_optional_modifiers, starts_modifiers};
use super::statements::statement;
use super::types::ty;
use crate::{Parser, parse_loop, parse_while};

const TOP_LEVEL_RECOVERY: &[Token] = &[Token::SEMICOLON, Token::NL, Token::R_CURL, Token::EOF];
const BRACKET_RECOVERY: &[Token] = &[Token::R_SQUARE, Token::SEMICOLON, Token::NL, Token::EOF];

pub(crate) fn kotlin_file(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();

    match parser.current_token() {
        None | Some(Token::EOF) => return,
        _ => {}
    }

    parser.start_node(KOTLIN_FILE);

    if parser.current_token() == Some(&Token::SHEBANG_LINE_TOKEN) {
        shebang_line(parser);
        parser.skip_trivia_and_newlines();
    }

    parse_while!(starts_file_annotation(parser), parser => {
        file_annotation(parser);
        parser.skip_trivia_and_newlines();
    });

    package_header(parser);
    parser.skip_trivia_and_newlines();

    import_list(parser);
    parser.skip_trivia_and_newlines();

    parse_loop! { parser =>
        if !starts_top_level_object(parser) {
            break;
        }
        top_level_object(parser);
        parser.skip_trivia_and_newlines();
    }

    if parser.current_token() == Some(&Token::EOF) {
        parser.bump();
    }

    parser.finish_node(KOTLIN_FILE);
}

pub(crate) fn script(parser: &mut Parser<'_, '_>) {
    parser.skip_trivia_and_newlines();
    match parser.current_token() {
        None | Some(Token::EOF) => return,
        _ => {}
    }

    parser.start_node(SCRIPT);

    if parser.current_token() == Some(&Token::SHEBANG_LINE_TOKEN) {
        shebang_line(parser);
        parser.skip_trivia_and_newlines();
    }

    parse_while!(starts_file_annotation(parser), parser => {
        file_annotation(parser);
        parser.skip_trivia_and_newlines();
    });

    package_header(parser);
    parser.skip_trivia_and_newlines();

    import_list(parser);
    parser.skip_trivia_and_newlines();

    parse_loop! { parser =>
        if parser.current_token() == Some(&Token::EOF) {
            break;
        }
        statement(parser);
        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::SEMICOLON) || parser.current_token() == Some(&Token::NL) {
            semi(parser);
        }
        parser.skip_trivia_and_newlines();
    }

    if parser.current_token() == Some(&Token::EOF) {
        parser.bump();
    }

    parser.finish_node(SCRIPT);
}

fn shebang_line(parser: &mut Parser<'_, '_>) {
    parser.start_node(SHEBANG_LINE);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::SHEBANG_LINE_TOKEN, "expected shebang", TOP_LEVEL_RECOVERY) {
            break;
        }
        parse_while!(parser.current_token() == Some(&Token::NL), parser => {
            parser.bump();
        });
        break;
    }
    parser.finish_node(SHEBANG_LINE);
}

fn file_annotation(parser: &mut Parser<'_, '_>) {
    parser.start_node(FILE_ANNOTATION);
    loop {
        if matches!(
            parser.current_token(),
            Some(Token::AT_NO_WS | Token::AT_PRE_WS)
        ) {
            parser.bump();
        } else {
            parser.error("expected '@'");
            parser.recover_until(TOP_LEVEL_RECOVERY);
            break;
        }

        parser.skip_trivia_and_newlines();
        if !parser.expect_recover(Token::FILE, "expected 'file'", TOP_LEVEL_RECOVERY) {
            break;
        }

        parser.skip_trivia_and_newlines();
        if !parser.expect_recover(Token::COLON, "expected ':'", TOP_LEVEL_RECOVERY) {
            break;
        }

        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::L_SQUARE) {
            parser.bump();
            parser.skip_trivia_and_newlines();
            parse_loop! { parser =>
                if matches!(parser.current_token(), Some(Token::R_SQUARE) | None) {
                    break;
                }
                if unescaped_annotation(parser){
                    parser.skip_trivia_and_newlines();
                } else {
                    break;
                }
            }
            if !parser.expect_recover(Token::R_SQUARE, "expected ']'", BRACKET_RECOVERY) {
                break;
            }
        } else {
            unescaped_annotation(parser);
        }

        parser.skip_trivia_and_newlines();
        break;
    }
    parser.finish_node(FILE_ANNOTATION);
}

fn package_header(parser: &mut Parser<'_, '_>) {
    parser.start_node(PACKAGE_HEADER);
    if parser.current_token() == Some(&Token::PACKAGE) {
        parser.bump();
        parser.skip_trivia_and_newlines();
        identifier(parser);
        parser.skip_trivia();
        semis(parser);
    }
    parser.finish_node(PACKAGE_HEADER);
}

fn import_list(parser: &mut Parser<'_, '_>) {
    parser.start_node(IMPORT_LIST);
    parse_while!(parser.current_token() == Some(&Token::IMPORT), parser => {
        import_header(parser);
        parser.skip_trivia_and_newlines();
    });
    parser.finish_node(IMPORT_LIST);
}

fn import_header(parser: &mut Parser<'_, '_>) {
    parser.start_node(IMPORT_HEADER);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::IMPORT, "expected 'import'", TOP_LEVEL_RECOVERY) {
            break;
        }

        parser.skip_trivia_and_newlines();
        identifier(parser);

        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::DOT) {
            parser.bump();
            parser.skip_trivia_and_newlines();
            if parser.current_token() == Some(&Token::MULT) {
                parser.bump();
            } else if parser.current_token() == Some(&Token::AS) {
                import_alias(parser);
            }
        } else if parser.current_token() == Some(&Token::AS) {
            import_alias(parser);
        }

        parser.skip_trivia_and_newlines();
        semis(parser);
        break;
    }

    parser.finish_node(IMPORT_HEADER);
}

fn import_alias(parser: &mut Parser<'_, '_>) {
    parser.start_node(IMPORT_ALIAS);
    parse_loop! { parser =>
        if !parser.expect_recover(Token::AS, "expected 'as'", TOP_LEVEL_RECOVERY) {
            break;
        }
        parser.skip_trivia_and_newlines();
        simple_identifier(parser);
        break;
    }
    parser.finish_node(IMPORT_ALIAS);
}

fn top_level_object(parser: &mut Parser<'_, '_>) {
    parser.start_node(TOP_LEVEL_OBJECT);
    declaration(parser);
    parser.skip_trivia_and_newlines();
    semis(parser);
    parser.finish_node(TOP_LEVEL_OBJECT);
}

fn type_alias(parser: &mut Parser<'_, '_>) {
    parser.start_node(TYPE_ALIAS);
    parse_loop! { parser =>
        parse_optional_modifiers(parser);
        parser.skip_trivia_and_newlines();

        if !parser.expect_recover(Token::TYPE_ALIAS, "expected 'typealias'", TOP_LEVEL_RECOVERY) {
            break;
        }

        parser.skip_trivia_and_newlines();
        simple_identifier(parser);

        parser.skip_trivia_and_newlines();
        if parser.current_token() == Some(&Token::L_ANGLE) {
            type_parameters(parser);
            parser.skip_trivia_and_newlines();
        }

        if !parser.expect_recover(Token::ASSIGNMENT_TOKEN, "expected '='", TOP_LEVEL_RECOVERY) {
            break;
        }

        parser.skip_trivia_and_newlines();
        ty(parser);
        break;
    }
    parser.finish_node(TYPE_ALIAS);
}

fn declaration(parser: &mut Parser<'_, '_>) {
    parser.start_node(DECLARATION);
    parse_loop! { parser =>
        let has_modifiers = starts_modifiers(parser);
        match parser.current_token() {
            Some(Token::CLASS | Token::INTERFACE) => class_declaration(parser),
            Some(Token::OBJECT) => object_declaration(parser),
            Some(Token::FUN) => function_declaration(parser),
            Some(Token::VAL | Token::VAR) => property_declaration(parser),
            Some(Token::TYPE_ALIAS) => type_alias(parser),
            _ if has_modifiers => {
                // Best-effort: try class declaration when modifiers are present.
                class_declaration(parser);
            }
            _ => {
                parser.error("expected declaration");
                parser.recover_until(TOP_LEVEL_RECOVERY);
                break;
            }
        }
        break;
    }
    parser.finish_node(DECLARATION);
}

fn starts_file_annotation(parser: &mut Parser<'_, '_>) -> bool {
    match parser.current_token() {
        Some(Token::AT_NO_WS | Token::AT_PRE_WS) => {
            let mut idx = 1usize;
            loop {
                match parser.lookahead_token(idx) {
                    Some(
                        Token::WS | Token::NL | Token::LINE_COMMENT | Token::DELIMITED_COMMENT,
                    ) => idx += 1,
                    Some(Token::FILE) => return true,
                    _ => return false,
                }
            }
        }
        _ => false,
    }
}

fn starts_top_level_object(parser: &mut Parser<'_, '_>) -> bool {
    starts_declaration(parser)
}

fn starts_declaration(parser: &mut Parser<'_, '_>) -> bool {
    starts_modifiers(parser)
        || matches!(
            parser.current_token(),
            Some(
                Token::CLASS
                    | Token::INTERFACE
                    | Token::OBJECT
                    | Token::FUN
                    | Token::VAL
                    | Token::VAR
                    | Token::TYPE_ALIAS
            )
        )
}
