use super::statements::{statement, statements};
use crate::test_utils::make_parser;
use ast::syntax::SyntaxKind::*;

#[test]
fn parses_simple_statement_expression() {
    let parse = make_parser(statement);
    let node = parse("x").syntax();

    assert_eq!(node.kind(), STATEMENT);
    assert!(node.descendants().any(|n| n.kind() == EXPRESSION));
}

#[test]
fn parses_labeled_statement() {
    let parse = make_parser(statement);
    let node = parse("label@ x").syntax();

    assert!(node.descendants().any(|n| n.kind() == LABEL));
    assert!(node.descendants().any(|n| n.kind() == EXPRESSION));
}

#[test]
fn parses_assignment_statement() {
    let parse = make_parser(statement);
    let node = parse("a = 1").syntax();

    assert!(node.descendants().any(|n| n.kind() == ASSIGNMENT));
}

#[test]
fn parses_compound_assignment_statement() {
    let parse = make_parser(statement);
    let node = parse("a += 1").syntax();

    assert!(
        node.descendants()
            .any(|n| n.kind() == ASSIGNMENT_AND_OPERATOR)
    );
}

#[test]
fn parses_for_statement() {
    let parse = make_parser(statement);
    let node = parse("for (i in items) i").syntax();

    assert!(node.descendants().any(|n| n.kind() == FOR_STATEMENT));
    assert!(
        node.descendants()
            .any(|n| n.kind() == CONTROL_STRUCTURE_BODY)
    );
}

#[test]
fn parses_while_and_do_while() {
    let parse = make_parser(statements);
    let node = parse("while (x) { }\ndo { } while (x)").syntax();

    assert!(node.descendants().any(|n| n.kind() == WHILE_STATEMENT));
    assert!(node.descendants().any(|n| n.kind() == DO_WHILE_STATEMENT));
}

#[test]
fn parses_statements_with_semis() {
    let parse = make_parser(statements);
    let parsed = parse("a; b; c");

    let node = parsed.syntax();
    let count = node.descendants().filter(|n| n.kind() == STATEMENT).count();

    assert_eq!(count, 3);
}

#[test]
fn parses_for_with_annotations() {
    let parse = make_parser(statement);
    let node = parse("for (@A i: Int in items) i").syntax();

    assert!(node.descendants().any(|n| n.kind() == FOR_STATEMENT));
    assert!(node.descendants().any(|n| n.kind() == ANNOTATION));
}

#[test]
fn parses_for_with_multi_variable() {
    let parse = make_parser(statement);
    let node = parse("for ((a, b) in items) a").syntax();

    assert!(node.descendants().any(|n| n.kind() == FOR_STATEMENT));
    assert!(
        node.descendants()
            .any(|n| n.kind() == MULTI_VARIABLE_DECLARATION)
    );
}

#[test]
fn parses_while_with_semicolon_body() {
    let parse = make_parser(statement);
    let node = parse("while (x) ;").syntax();

    assert!(node.descendants().any(|n| n.kind() == WHILE_STATEMENT));
}

#[test]
fn recovers_after_invalid_do_while_and_continues() {
    let parse = make_parser(statements);
    let source = "do { }\nfoo";
    let parsed = parse(source);

    assert!(!parsed.errors.is_empty());
    let node = parsed.syntax();
    let count = node.descendants().filter(|n| n.kind() == STATEMENT).count();

    assert_eq!(count, 2);
    insta::assert_snapshot!(
        "recovers_after_invalid_do_while_and_continues",
        parsed.snapshot(),
        source
    );
}

#[test]
fn recovers_if_there_is_no_semis_after_statement() {
    let parse = make_parser(statements);
    let source = "val x = 1 val y = 2";
    let parsed = parse(source);

    assert!(!parsed.errors.is_empty());
    let node = parsed.syntax();
    let count = node.descendants().filter(|n| n.kind() == STATEMENT).count();

    insta::assert_snapshot!(
        "recovers_if_there_is_no_semis_after_statement",
        parsed.snapshot(),
        source
    );
    assert_eq!(count, 2);
}
