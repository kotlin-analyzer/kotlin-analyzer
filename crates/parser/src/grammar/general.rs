use syntax::Token;
use syntax::{SyntaxKind::*, T};

use super::annotations::unescaped_annotation;
use super::class_members::PROPERTY_DECLARATION_START;
use super::class_members::{
    function_declaration, object_declaration, property_declaration, starts_fn_declaration,
};
use super::classes::starts_class_declaration;
use super::classes::{class_declaration, type_parameters};
use super::identifiers::{identifier, simple_identifier};
use super::modifiers::modifiers;
use super::statements::{semi, semis, statement};
use super::types::ty;
use crate::ra::{CompletedMarker, Parser};

const TOP_LEVEL_RECOVERY: &[Token] = &[Token::SEMICOLON, Token::NL, Token::R_CURL, Token::EOF];
const BRACKET_RECOVERY: &[Token] = &[Token::R_SQUARE, Token::SEMICOLON, Token::NL, Token::EOF];

pub(crate) fn kotlin_file(parser: &mut Parser<'_>) {
    let m = parser.start();
    shebang_line(parser);
    while file_annotation(parser).is_some() {}
    package_header(parser);
    import_list(parser);
    while top_level_object(parser).is_some() {}
    m.complete(parser, KOTLIN_FILE);
}

pub(crate) fn script(parser: &mut Parser<'_>) {
    let m = parser.start();
    shebang_line(parser);
    while file_annotation(parser).is_some() {}
    package_header(parser);
    import_list(parser);
    while statement(parser)
        .inspect(|_| {
            semi(parser);
        })
        .is_some()
    {}

    m.complete(parser, SCRIPT);
}

fn shebang_line(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(SHEBANG_LINE_TOKEN) {
        let m = parser.start();
        parser.eat(SHEBANG_LINE_TOKEN);
        Some(m.complete(parser, SHEBANG_LINE))
    } else {
        None
    }
}

fn file_annotation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match (parser.current(), parser.nth(1)) {
        (T![@] | AT_PRE_WS, FILE) => {
            let m = parser.start();
            parser.bump_any();
            parser.bump(FILE);

            if !parser.eat(T![:]) {
                parser.error("expected `:`");
            }

            if parser.eat(T!['[']) {
                while unescaped_annotation(parser).is_some() {}
                if !parser.eat(T![']']) {
                    parser.error("expected ']'");
                }
            } else if unescaped_annotation(parser).is_none() {
                parser.error("expected annotation");
            }
            semi(parser);
            Some(m.complete(parser, FILE_ANNOTATION))
        }
        _ => None,
    }
}

fn package_header(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![package]) {
        let m = parser.start();
        parser.eat(T![package]);
        identifier(parser);
        semi(parser);
        Some(m.complete(parser, PACKAGE_HEADER))
    } else {
        None
    }
}

fn import_list(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    let mut has_imports = false;
    while import_header(parser).is_some() {
        has_imports = true;
    }
    if has_imports {
        Some(m.complete(parser, IMPORT_LIST))
    } else {
        m.abandon(parser);
        None
    }
}

fn import_header(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![import]) {
        let m = parser.start();
        parser.eat(T![import]);
        identifier(parser);

        if parser.eat(T![.]) {
            if !parser.eat(T![*]) {
                parser.error("expected either '*' or identifier");
            }
        } else {
            import_alias(parser);
        }

        semis(parser);
        Some(m.complete(parser, IMPORT_HEADER))
    } else {
        None
    }
}

fn import_alias(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![as]) {
        let m = parser.start();
        parser.eat(T![as]);
        simple_identifier(parser);
        Some(m.complete(parser, IMPORT_ALIAS))
    } else {
        None
    }
}

fn top_level_object(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    let modifiers_marker = modifiers(parser);

    if declaration(parser, modifiers_marker).is_none() {
        m.abandon(parser);
        return None;
    }

    semis(parser);
    Some(m.complete(parser, TOP_LEVEL_OBJECT))
}

fn type_alias(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if !parser.at(T![typealias]) {
        return None;
    }
    let m = modifiers_marker.map(|cm| cm.precede(parser)).unwrap_or_else(|| parser.start());

    parser.bump(T![typealias]);

    if simple_identifier(parser).is_none() {
        parser.error("expected type alias name");
    }
    type_parameters(parser);
    if !parser.eat(T![=]) {
        parser.error("expected '='");
        if ty(parser).is_none() {
            parser.error("expected type");
        }
    }
    Some(m.complete(parser, TYPE_ALIAS))
}

pub(crate) fn declaration(
    parser: &mut Parser<'_>,
    modifiers_marker: Option<CompletedMarker>,
) -> Option<CompletedMarker> {
    if starts_class_declaration(parser) {
        class_declaration(parser, modifiers_marker)
            .map(|cm| cm.precede(parser).complete(parser, DECLARATION))
    } else if starts_fn_declaration(parser) {
        function_declaration(parser, modifiers_marker)
            .map(|cm| cm.precede(parser).complete(parser, DECLARATION))
    } else if parser.at(T![object]) {
        object_declaration(parser, modifiers_marker)
            .map(|cm| cm.precede(parser).complete(parser, DECLARATION))
    } else if parser.at_ts(PROPERTY_DECLARATION_START) {
        property_declaration(parser, modifiers_marker)
            .map(|cm| cm.precede(parser).complete(parser, DECLARATION))
    } else if parser.at(T![typealias]) {
        type_alias(parser, modifiers_marker)
            .map(|cm| cm.precede(parser).complete(parser, DECLARATION))
    } else {
        None
    }
}
