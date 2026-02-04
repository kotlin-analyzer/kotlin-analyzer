use super::types::{ty, type_reference};
use crate::test_utils::make_parser;
use ast::syntax::SyntaxKind::*;
use rstest::Context;

#[test]
fn parses_dynamic_type_reference() {
    let parse = make_parser(type_reference);
    let node = parse("dynamic").syntax();

    assert_eq!(node.kind(), TYPE_REFERENCE);
    assert_eq!(node.first_token().unwrap().kind(), DYNAMIC);
}

#[test]
fn parses_user_type_with_type_arguments() {
    let parse = make_parser(type_reference);
    let node = parse("Foo.Bar<Baz>").syntax();

    assert_eq!(node.kind(), TYPE_REFERENCE);

    let user_type = node
        .children()
        .find(|child| child.kind() == USER_TYPE)
        .expect("expected a user type child");

    let segments: Vec<_> = user_type
        .children()
        .filter(|child| child.kind() == SIMPLE_USER_TYPE)
        .collect();

    assert_eq!(segments.len(), 2);
    assert_eq!(segments[0].first_token().unwrap().kind(), IDENTIFIER_TOKEN);
    assert_eq!(segments[1].first_token().unwrap().kind(), IDENTIFIER_TOKEN);

    let type_arguments = segments[1]
        .children()
        .find(|child| child.kind() == TYPE_ARGUMENTS)
        .expect("expected type arguments on second segment");

    assert_eq!(
        type_arguments
            .children()
            .filter(|child| child.kind() == TYPE_PROJECTION)
            .count(),
        1
    );
}

#[test]
fn parses_nullable_type() {
    let parse = make_parser(ty);
    let node = parse("Foo??").syntax();

    assert_eq!(node.kind(), TYPE);

    let nullable = node
        .children()
        .find(|child| child.kind() == NULLABLE_TYPE)
        .expect("expected nullable type child");

    assert_eq!(nullable.children().filter(|c| c.kind() == QUEST).count(), 2);
}

#[test]
fn parses_function_type_without_receiver() {
    let parse = make_parser(ty);
    let node = parse("(Foo, Bar) -> Baz").syntax();

    let fun = node
        .children()
        .find(|child| child.kind() == FUNCTION_TYPE)
        .expect("expected function type");

    let params = fun
        .children()
        .find(|child| child.kind() == FUNCTION_TYPE_PARAMETERS)
        .expect("expected parameters");
    assert!(params.first_token().is_some());

    let ret = fun
        .children()
        .find(|child| child.kind() == TYPE)
        .expect("expected return type");
    assert_eq!(ret.first_token().unwrap().kind(), IDENTIFIER_TOKEN);
}

#[test]
fn parses_function_type_with_receiver() {
    let parse = make_parser(ty);
    let node = parse("Receiver.() -> Result").syntax();

    let fun = node
        .children()
        .find(|child| child.kind() == FUNCTION_TYPE)
        .expect("expected function type");

    let receiver = fun
        .children()
        .find(|child| child.kind() == RECEIVER_TYPE)
        .expect("expected receiver type");
    assert_eq!(receiver.first_token().unwrap().kind(), IDENTIFIER_TOKEN);
}

#[test]
fn parses_projection_with_variance_modifier() {
    let parse = make_parser(ty);
    let node = parse("Box<out Item>").syntax();

    let type_args = node
        .descendants()
        .find(|child| child.kind() == TYPE_ARGUMENTS)
        .expect("expected type arguments");

    let projection = type_args
        .children()
        .find(|c| c.kind() == TYPE_PROJECTION)
        .expect("expected projection child");

    assert!(
        projection
            .children()
            .any(|c| c.kind() == TYPE_PROJECTION_MODIFIERS)
    );
}

#[rstest::rstest]
#[case::simple("Foo", &[TYPE_REFERENCE])]
#[case::qualified_generic("Foo.Bar<Baz>", &[USER_TYPE, SIMPLE_USER_TYPE, TYPE_ARGUMENTS])]
#[case::nullable_chain("Foo??", &[NULLABLE_TYPE])]
#[case::parenthesized_function("((Foo) -> Bar)?", &[PARENTHESIZED_TYPE, FUNCTION_TYPE, NULLABLE_TYPE])]
#[case::function_no_receiver("(Foo, Bar) -> Baz", &[FUNCTION_TYPE, FUNCTION_TYPE_PARAMETERS])]
#[case::function_with_receiver("Receiver.(Foo) -> Result", &[FUNCTION_TYPE, RECEIVER_TYPE])]
#[case::projection_variance("Box<out Item>", &[TYPE_ARGUMENTS, TYPE_PROJECTION, TYPE_PROJECTION_MODIFIERS])]
#[case::star_projection("List<*>", &[TYPE_ARGUMENTS, TYPE_PROJECTION])]
#[case::complex_mix(
    "Receiver.(List<out Foo?>, () -> Bar) -> Map<in Key, Value?>?",
    &[FUNCTION_TYPE, RECEIVER_TYPE, TYPE_ARGUMENTS, TYPE_PROJECTION, NULLABLE_TYPE]
)]
fn ty_parses_diverse_structures(
    #[context] ctx: Context,
    #[case] text: &str,
    #[case] expected_kinds: &[ast::syntax::SyntaxKind],
) {
    let parse = make_parser(ty);
    let node = parse(text).syntax();

    assert_eq!(node.kind(), TYPE);

    let kinds: Vec<_> = node.descendants().map(|child| child.kind()).collect();

    for &expected in expected_kinds {
        assert!(
            kinds.iter().any(|&kind| kind == expected),
            "expected {:?} in `{}`",
            expected,
            text
        );
    }

    insta::assert_debug_snapshot!(
        format!(
            "{}-{}",
            ctx.name,
            ctx.description
                .expect("to provide description for test case")
        ),
        kinds
    );
}
