use super::expressions::expression;
use crate::test_utils::make_parser;
use ast::syntax::SyntaxKind::*;
use rstest::rstest;

#[test]
fn parses_basic_precedence() {
    let parse = make_parser(expression);
    let node = parse("1 + 2 * 3").syntax();

    assert_eq!(node.kind(), EXPRESSION);

    let additive = node
        .descendants()
        .find(|n| n.kind() == ADDITIVE_EXPRESSION)
        .expect("expected additive expression");

    assert!(
        additive
            .descendants()
            .any(|n| n.kind() == MULTIPLICATIVE_EXPRESSION)
    );

    assert!(
        additive
            .descendants()
            .any(|n| n.kind() == ADDITIVE_OPERATOR)
    );
}

#[test]
fn parses_call_and_postfix_chain() {
    let parse = make_parser(expression);
    let node = parse("foo.bar(1, 2)[0].baz").syntax();

    assert_eq!(node.kind(), EXPRESSION);

    let postfix = node
        .descendants()
        .find(|n| n.kind() == POSTFIX_UNARY_EXPRESSION)
        .expect("expected postfix expression");

    assert!(postfix.descendants().any(|n| n.kind() == CALL_SUFFIX));
    assert!(postfix.descendants().any(|n| n.kind() == INDEXING_SUFFIX));
    assert!(postfix.descendants().any(|n| n.kind() == NAVIGATION_SUFFIX));
}

#[test]
fn parses_elvis_expression() {
    let parse = make_parser(expression);
    let node = parse("a ?: b").syntax();

    let elvis = node
        .descendants()
        .find(|n| n.kind() == ELVIS)
        .expect("expected elvis operator node");

    assert_eq!(elvis.first_token().unwrap().kind(), QUEST_NO_WS);
}

#[test]
fn parses_as_expression() {
    let parse = make_parser(expression);
    let node = parse("x as String").syntax();

    let as_op = node
        .descendants()
        .find(|n| n.kind() == AS_OPERATOR)
        .expect("expected as operator");

    assert_eq!(as_op.first_token().unwrap().kind(), AS);
}

#[rstest]
#[case::prefix("++a", PREFIX_UNARY_OPERATOR)]
#[case::postfix("a++", POSTFIX_UNARY_OPERATOR)]
fn parses_unary_variants(#[case] text: &str, #[case] expected: ast::syntax::SyntaxKind) {
    let parse = make_parser(expression);
    let node = parse(text).syntax();

    assert!(node.descendants().any(|n| n.kind() == expected));
}

#[test]
fn parses_type_args_and_lambda_call() {
    let parse = make_parser(expression);
    let node = parse("foo<Int>(1) { -> } ").syntax();

    let call_suffix = node
        .descendants()
        .find(|n| n.kind() == CALL_SUFFIX)
        .expect("expected call suffix");

    assert!(
        call_suffix
            .descendants()
            .any(|n| n.kind() == TYPE_ARGUMENTS)
    );
    assert!(
        call_suffix
            .descendants()
            .any(|n| n.kind() == ANNOTATED_LAMBDA)
    );
}

#[test]
fn parses_identifier() {
    let parse = make_parser(expression);
    let node = parse("x").syntax();

    let simple_identifier = node
        .descendants()
        .find(|n| n.kind() == SIMPLE_IDENTIFIER)
        .expect("expected simple identifier");

    assert!(simple_identifier.first_token().is_some());
}

#[test]
fn parses_collection_literal() {
    let parse = make_parser(expression);
    let node = parse("[1, 2, 3]").syntax();

    assert!(node.descendants().any(|n| n.kind() == COLLECTION_LITERAL));
}

#[test]
fn parses_this_expression() {
    let parse = make_parser(expression);
    let node = parse("this").syntax();

    assert!(node.descendants().any(|n| n.kind() == THIS_EXPRESSION));
}

#[test]
fn parses_super_expression() {
    let parse = make_parser(expression);
    let node = parse("super").syntax();

    assert!(node.descendants().any(|n| n.kind() == SUPER_EXPRESSION));
}

#[test]
fn parses_range_expression() {
    let parse = make_parser(expression);
    let node = parse("1..2").syntax();

    assert!(node.descendants().any(|n| n.kind() == RANGE_EXPRESSION));
}

#[test]
fn parses_safe_navigation() {
    let parse = make_parser(expression);
    let node = parse("a?.b").syntax();

    assert!(node.descendants().any(|n| n.kind() == SAFE_NAV));
    assert!(node.descendants().any(|n| n.kind() == NAVIGATION_SUFFIX));
}
