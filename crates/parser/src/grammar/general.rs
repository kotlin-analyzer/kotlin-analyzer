use crate::{SyntaxKind::*, T};

use super::annotations::unescaped_annotation;
use super::class_members::{function_declaration, object_declaration, property_declaration};

use super::classes::{class_declaration, type_parameters};
use super::identifiers::{identifier, simple_identifier};
use super::statements::{semi, semis, statements};
use super::types::ty;
use crate::{CompletedMarker, Parser};

pub(crate) fn kotlin_file(parser: &mut Parser<'_>) {
    let m = parser.start();
    shebang_line(parser);
    while file_annotation(parser).is_some() {}
    package_header(parser);
    import_list(parser);
    top_level_objects(parser);
    m.complete(parser, KOTLIN_FILE);
}

pub(crate) fn script(parser: &mut Parser<'_>) {
    let m = parser.start();
    shebang_line(parser);
    while file_annotation(parser).is_some() {}
    package_header(parser);
    import_list(parser);

    statements(parser);

    m.complete(parser, SCRIPT);
}

// test shebang_line
// #!/usr/bin/env kotlinc
fn shebang_line(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(SHEBANG) {
        let m = parser.start();
        parser.eat(SHEBANG);
        Some(m.complete(parser, SHEBANG_LINE))
    } else {
        None
    }
}

// test file_annotation
// @file:JvmName("Foo")
// @file:[JvmName("Foo") JvmMultifileClass]
fn file_annotation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match (parser.current(), parser.nth(1)) {
        (T![@], T![file]) => {
            let m = parser.start();
            parser.bump_any();
            parser.bump(T![file]);

            if !parser.eat(T![:]) {
                parser.error("expected `:`");
            }
            // test_err file_annotation
            // @file:[JvmName("Foo"), JvmMultifileClass]
            // @file:[JvmName("Foo"), JvmMultifileClass
            // @file:
            if parser.eat(T!['[']) {
                while unescaped_annotation(parser).is_some() {
                    if parser.eat(T![,]) {
                        parser.error("file annotations should not be separated by ','");
                    }
                }
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

// test package_header
// package foo.bar

// test package_header2
// package foo.bar.baz
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

// test import_list
// import foo.bar
// import foo.bar.*
// import foo.bar.Baz as BazAlias
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
fn top_level_objects(parser: &mut Parser<'_>) {
    while top_level_object(parser).is_some() {}
}

fn top_level_object(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    declaration(parser, false).inspect(|_| {
        semis(parser);
    })
    // can not complete the marker here because we might have a dangling marker. same as in statement
}

fn type_alias(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();

    if !parser.at(T![typealias]) {
        m.abandon(parser);
        return None;
    }

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
    Some(m.complete(parser, TYPE_ALIAS))
}

pub(super) fn declaration(
    parser: &mut Parser<'_>,
    allow_expressions: bool,
) -> Option<CompletedMarker> {
    class_declaration(parser)
        .or_else(|| function_declaration(parser, allow_expressions))
        .or_else(|| object_declaration(parser))
        .or_else(|| property_declaration(parser))
        .or_else(|| type_alias(parser))
}
