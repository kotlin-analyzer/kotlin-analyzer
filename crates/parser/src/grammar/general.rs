// use syntax::Token;
use crate::{Marker, SyntaxKind::*, T};

use super::annotations::unescaped_annotation;
use super::class_members::PROPERTY_DECLARATION_START;
use super::class_members::{
    function_declaration, object_declaration, property_declaration, starts_fn_declaration,
};
use super::classes::starts_class_declaration;
use super::classes::{class_declaration, type_parameters};
use super::identifiers::{identifier, simple_identifier};
use super::modifiers::modifiers;
use super::statements::{semi, semis, statements};
use super::types::ty;
use crate::{CompletedMarker, Parser};

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

    statements(parser, None);

    m.complete(parser, SCRIPT);
}

fn shebang_line(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(SHEBANG) {
        let m = parser.start();
        parser.eat(SHEBANG);
        Some(m.complete(parser, SHEBANG_LINE))
    } else {
        None
    }
}

fn file_annotation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match (parser.current(), parser.nth(1)) {
        (T![@], T![file]) => {
            let m = parser.start();
            parser.bump_any();
            parser.bump(T![file]);

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
    modifiers(parser);

    match declaration(parser, m) {
        Ok(cm) => {
            let (cm, optm) = cm.into_parts();
            if let Some(optm) = optm {
                // TODO: this should be forwarded to the next declaration
                optm.abandon(parser);
            }
            let m = cm.precede(parser);
            semis(parser);
            Some(m.complete(parser, TOP_LEVEL_OBJECT))
        }
        Err(cm) => {
            cm.abandon(parser);
            None
        }
    }
}

fn type_alias(parser: &mut Parser<'_>, start: Marker) -> Result<CompletedMarker, Marker> {
    if !parser.at(T![typealias]) {
        return Err(start);
    }
    let m = start;

    parser.bump(T![typealias]);

    if simple_identifier(parser).is_none() {
        parser.error("expected type alias name");
    }
    type_parameters(parser);
    if !parser.eat(T![=]) {
        parser.error("expected '='");
    }
    if ty(parser).is_none() {
        parser.error("expected type");
    }
    Ok(m.complete(parser, TYPE_ALIAS))
}

pub(super) fn declaration(
    parser: &mut Parser<'_>,
    start: Marker,
) -> Result<CompletedMarker, Marker> {
    if starts_class_declaration(parser) {
        class_declaration(parser, start)
    } else if starts_fn_declaration(parser) {
        function_declaration(parser, start)
    } else if parser.at(T![object]) {
        object_declaration(parser, start)
    } else if parser.at_ts(PROPERTY_DECLARATION_START) {
        property_declaration(parser, start)
    } else if parser.at(T![typealias]) {
        type_alias(parser, start)
    } else {
        Err(start)
    }
}
