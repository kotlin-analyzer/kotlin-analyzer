use super::class_members::{class_member_declaration, class_member_declarations};
use crate::test_utils::make_parser;
use ast::syntax::SyntaxKind::*;

#[test]
fn parses_anonymous_initializer() {
    let parse = make_parser(class_member_declaration);
    let node = parse("init { }").syntax();

    assert!(
        node.descendants()
            .any(|n| n.kind() == ANONYMOUS_INITIALIZER)
    );
    assert!(node.descendants().any(|n| n.kind() == BLOCK));
}

#[test]
fn parses_companion_object() {
    let parse = make_parser(class_member_declaration);
    let node = parse("companion object {}").syntax();

    assert!(node.descendants().any(|n| n.kind() == COMPANION_OBJECT));
    assert!(node.descendants().any(|n| n.kind() == CLASS_BODY));
}

#[test]
fn parses_secondary_constructor_with_delegation() {
    let parse = make_parser(class_member_declaration);
    let node = parse("constructor() : this()").syntax();

    assert!(
        node.descendants()
            .any(|n| n.kind() == SECONDARY_CONSTRUCTOR)
    );
    assert!(
        node.descendants()
            .any(|n| n.kind() == CONSTRUCTOR_DELEGATION_CALL)
    );
    assert!(node.descendants().any(|n| n.kind() == VALUE_ARGUMENTS));
}

#[test]
fn parses_function_declaration() {
    let parse = make_parser(class_member_declaration);
    let node = parse("fun foo(a: Int): String { }").syntax();

    assert!(node.descendants().any(|n| n.kind() == FUNCTION_DECLARATION));
    assert!(
        node.descendants()
            .any(|n| n.kind() == FUNCTION_VALUE_PARAMETERS)
    );
    assert!(node.descendants().any(|n| n.kind() == FUNCTION_BODY));
}

#[test]
fn parses_property_declaration_with_initializer() {
    let parse = make_parser(class_member_declaration);
    let node = parse("val x: Int = 1").syntax();

    assert!(node.descendants().any(|n| n.kind() == PROPERTY_DECLARATION));
    assert!(node.descendants().any(|n| n.kind() == VARIABLE_DECLARATION));
}

#[test]
fn parses_class_member_declarations_list() {
    let parse = make_parser(class_member_declarations);
    let node = parse("init { }\nfun foo() {}\nval x = 1").syntax();

    let kinds: Vec<_> = node
        .children()
        .filter(|n| n.kind() == CLASS_MEMBER_DECLARATION)
        .collect();
    assert_eq!(kinds.len(), 3);
}

#[test]
fn parses_property_with_getter_setter() {
    let parse = make_parser(class_member_declaration);
    let node = parse("var x: Int = 1").syntax();

    assert!(node.descendants().any(|n| n.kind() == PROPERTY_DECLARATION));
}

#[test]
fn parses_property_delegate() {
    let parse = make_parser(class_member_declaration);
    let node = parse("val x by foo()\n").syntax();

    assert!(node.descendants().any(|n| n.kind() == PROPERTY_DELEGATE));
}

#[test]
fn parses_secondary_constructor_with_block() {
    let parse = make_parser(class_member_declaration);
    let node = parse("constructor() { }").syntax();

    assert!(
        node.descendants()
            .any(|n| n.kind() == SECONDARY_CONSTRUCTOR)
    );
    assert!(node.descendants().any(|n| n.kind() == BLOCK));
}

#[test]
fn parses_companion_object_with_name() {
    let parse = make_parser(class_member_declaration);
    let node = parse("companion object Foo { }").syntax();

    assert!(node.descendants().any(|n| n.kind() == COMPANION_OBJECT));
    assert!(node.descendants().any(|n| n.kind() == SIMPLE_IDENTIFIER));
}
