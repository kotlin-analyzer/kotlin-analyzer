use super::modifiers::{modifier, modifiers, parameter_modifiers, type_parameter_modifiers};
use crate::test_utils::make_parser;
use ast::syntax::SyntaxKind::{self, *};

#[rstest::rstest]
#[case::class_data("data", CLASS_MODIFIER)]
#[case::member_override("override", MEMBER_MODIFIER)]
#[case::visibility_internal("internal", VISIBILITY_MODIFIER)]
#[case::function_inline("inline", FUNCTION_MODIFIER)]
#[case::property_const("const", PROPERTY_MODIFIER)]
#[case::inheritance_open("open", INHERITANCE_MODIFIER)]
#[case::parameter_vararg("vararg", PARAMETER_MODIFIER)]
#[case::platform_expect("expect", PLATFORM_MODIFIER)]
fn modifier_wraps_specific_kind(#[case] text: &str, #[case] expected_child: SyntaxKind) {
    let parse = make_parser(modifier);
    let node = parse(text).syntax();

    assert_eq!(node.kind(), MODIFIER);

    let inner = node
        .children()
        .find(|child| child.kind() == expected_child)
        .expect("expected specific modifier child");

    assert!(inner.first_token().is_some());
}

#[test]
fn modifiers_collect_annotations_and_modifiers() {
    let parse = make_parser(modifiers);
    let node = parse("@Anno public\nopen override").syntax();

    assert_eq!(node.kind(), MODIFIERS);

    assert!(
        node.children().any(|child| child.kind() == ANNOTATION),
        "expected annotation child"
    );

    let modifier_kinds: Vec<_> = node
        .children()
        .filter(|child| child.kind() == MODIFIER)
        .map(|child| {
            child
                .children()
                .find(|nested| {
                    matches!(
                        nested.kind(),
                        VISIBILITY_MODIFIER | INHERITANCE_MODIFIER | MEMBER_MODIFIER
                    )
                })
                .map(|nested| nested.kind())
                .expect("modifier should wrap specific kind")
        })
        .collect();

    assert_eq!(
        modifier_kinds,
        vec![VISIBILITY_MODIFIER, INHERITANCE_MODIFIER, MEMBER_MODIFIER]
    );
}

#[test]
fn parameter_modifiers_accept_annotation_and_parameters() {
    let parse = make_parser(parameter_modifiers);
    let node = parse("@A noinline vararg").syntax();

    assert_eq!(node.kind(), PARAMETER_MODIFIERS);
    assert!(node.children().any(|c| c.kind() == ANNOTATION));

    let tokens: Vec<_> = node
        .children()
        .filter(|child| child.kind() == PARAMETER_MODIFIER)
        .map(|child| child.first_token().unwrap().kind())
        .collect();

    assert_eq!(tokens, vec![NO_INLINE, VAR_ARG]);
}

#[test]
fn type_parameter_modifiers_cover_all_cases() {
    let parse = make_parser(type_parameter_modifiers);
    let node = parse("@A reified out").syntax();

    assert_eq!(node.kind(), TYPE_PARAMETER_MODIFIERS);

    let inner: Vec<_> = node
        .children()
        .filter(|child| child.kind() == TYPE_PARAMETER_MODIFIER)
        .map(|child| child.children().next().unwrap().kind())
        .collect();

    assert_eq!(
        inner,
        vec![ANNOTATION, REIFICATION_MODIFIER, VARIANCE_MODIFIER]
    );
}

#[test]
fn modifiers_tolerate_newlines_between_entries() {
    let parse = make_parser(modifiers);
    let node = parse("public\n\nopen\nfinal").syntax();

    let kinds: Vec<_> = node
        .children()
        .filter(|child| child.kind() == MODIFIER)
        .map(|child| child.children().next().unwrap().kind())
        .collect();

    assert_eq!(
        kinds,
        vec![
            VISIBILITY_MODIFIER,
            INHERITANCE_MODIFIER,
            INHERITANCE_MODIFIER
        ]
    );
}
