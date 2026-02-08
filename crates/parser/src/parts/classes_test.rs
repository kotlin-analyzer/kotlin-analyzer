use super::classes::{class_declaration, primary_constructor};
use crate::test_utils::make_parser;
use ast::syntax::SyntaxKind::*;

#[test]
fn parses_simple_class_declaration() {
	let parse = make_parser(class_declaration);
	let node = parse("class Foo {}").syntax();

	assert!(node.descendants().any(|n| n.kind() == CLASS_DECLARATION));
	assert!(node.descendants().any(|n| n.kind() == CLASS_BODY));
}

#[test]
fn parses_fun_interface_with_type_params() {
	let parse = make_parser(class_declaration);
	let node = parse("fun interface Fn<T>").syntax();

	assert!(node.descendants().any(|n| n.kind() == TYPE_PARAMETERS));
}

#[test]
fn parses_primary_constructor_and_parameters() {
	let parse = make_parser(primary_constructor);
	let node = parse("(val x: Int, y: String)").syntax();

	assert!(node.descendants().any(|n| n.kind() == CLASS_PARAMETERS));
	assert!(node.descendants().any(|n| n.kind() == CLASS_PARAMETER));
}

#[test]
fn parses_delegation_specifiers() {
	let parse = make_parser(class_declaration);
	let node = parse("class Foo: Bar(), Baz by qux").syntax();

	assert!(node.descendants().any(|n| n.kind() == DELEGATION_SPECIFIERS));
	assert!(node.descendants().any(|n| n.kind() == CONSTRUCTOR_INVOCATION));
	assert!(node.descendants().any(|n| n.kind() == EXPLICIT_DELEGATION));
}

#[test]
fn parses_type_constraints() {
	let parse = make_parser(class_declaration);
	let node = parse("class Foo<T> where T: Bar").syntax();

	assert!(node.descendants().any(|n| n.kind() == TYPE_CONSTRAINTS));
	assert!(node.descendants().any(|n| n.kind() == TYPE_CONSTRAINT));
}

#[test]
fn parses_enum_class_body_variant() {
	let parse = make_parser(class_declaration);
	let node = parse("class Foo { A, B }").syntax();

	assert!(node.descendants().any(|n| n.kind() == ENUM_CLASS_BODY));
}
