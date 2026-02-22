use super::enum_classes::{enum_class_body, enum_entries, enum_entry};
use crate::test_utils::make_parser;
use ast::syntax::SyntaxKind::*;

#[test]
fn parses_enum_entry_simple() {
    let parse = make_parser(enum_entry);
    let node = parse("FOO").syntax();

    assert_eq!(node.kind(), ENUM_ENTRY);
    assert!(node.descendants().any(|n| n.kind() == SIMPLE_IDENTIFIER));
}

#[test]
fn parses_enum_entries_with_trailing_comma() {
    let parse = make_parser(enum_entries);
    let node = parse("A, B,").syntax();

    let entries: Vec<_> = node.children().filter(|n| n.kind() == ENUM_ENTRY).collect();

    assert_eq!(entries.len(), 2);
}

#[test]
fn parses_enum_entry_with_arguments_and_body() {
    let parse = make_parser(enum_entry);
    let node = parse("A(1) { }").syntax();

    assert!(node.descendants().any(|n| n.kind() == VALUE_ARGUMENTS));
    assert!(node.descendants().any(|n| n.kind() == CLASS_BODY));
}

#[test]
fn parses_enum_class_body_with_members() {
    let parse = make_parser(enum_class_body);
    let node = parse("{ A, B; fun foo() {} }").syntax();

    assert!(node.descendants().any(|n| n.kind() == ENUM_ENTRIES));
    assert!(
        node.descendants()
            .any(|n| n.kind() == CLASS_MEMBER_DECLARATIONS)
    );
}

#[test]
fn parses_enum_entry_with_modifiers() {
    let parse = make_parser(enum_entry);
    let node = parse("data A").syntax();

    assert!(node.descendants().any(|n| n.kind() == MODIFIERS));
    assert!(node.descendants().any(|n| n.kind() == SIMPLE_IDENTIFIER));
}

#[test]
fn parses_enum_class_body_without_members() {
    let parse = make_parser(enum_class_body);
    let node = parse("{ A, B }").syntax();

    assert!(node.descendants().any(|n| n.kind() == ENUM_ENTRIES));
}

#[test]
fn recovers_from_missing_enum_class_closing_brace() {
    let parse = make_parser(enum_class_body);
    let parsed = parse("{ A");

    assert!(!parsed.errors.is_empty());
    let node = parsed.syntax();
    assert!(node.descendants().any(|n| n.kind() == ENUM_ENTRIES));
}
