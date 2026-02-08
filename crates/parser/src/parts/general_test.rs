use super::general::{kotlin_file, script};
use crate::test_utils::make_parser;
use ast::syntax::SyntaxKind::*;

#[test]
fn parses_shebang_and_package_imports() {
	let parse = make_parser(|parser| {
		kotlin_file(parser);
	});
	let node = parse("#!/usr/bin/env kotlin\n\npackage foo.bar\nimport baz.qux\nclass Foo {}").syntax();

	assert!(node.descendants().any(|n| n.kind() == SHEBANG_LINE));
	assert!(node.descendants().any(|n| n.kind() == PACKAGE_HEADER));
	assert!(node.descendants().any(|n| n.kind() == IMPORT_HEADER));
	assert!(node.descendants().any(|n| n.kind() == CLASS_DECLARATION));
}

#[test]
fn parses_file_annotation_list() {
	let parse = make_parser(|parser| {
		kotlin_file(parser);
	});
	let node = parse("@file:[Foo Bar]\npackage test").syntax();

	assert!(node.descendants().any(|n| n.kind() == FILE_ANNOTATION));
	assert!(node.descendants().any(|n| n.kind() == UNESCAPED_ANNOTATION));
}

#[test]
fn parses_type_alias_declaration() {
	let parse = make_parser(|parser| {
		kotlin_file(parser);
	});
	let node = parse("typealias Foo = Bar").syntax();

	assert!(node.descendants().any(|n| n.kind() == TYPE_ALIAS));
	assert!(node.descendants().any(|n| n.kind() == TYPE));
}

#[test]
fn parses_script_with_statements() {
	let parse = make_parser(|parser| {
		script(parser);
	});
	let node = parse("package test\nimport foo\nprintln(1); println(2)").syntax();

	assert!(node.descendants().any(|n| n.kind() == SCRIPT));
	assert!(node.descendants().any(|n| n.kind() == IMPORT_LIST));
	assert!(node.descendants().any(|n| n.kind() == STATEMENT));
}
