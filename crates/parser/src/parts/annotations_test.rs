use super::annotations::annotation;
use crate::test_utils::make_parser;
use ast::syntax::SyntaxKind::*;

#[test]
fn parses_single_annotation() {
    let parse = make_parser(annotation);
    let node = parse("@Foo").syntax();

    assert_eq!(node.kind(), ANNOTATION);

    let single = node
        .children()
        .find(|c| c.kind() == SINGLE_ANNOTATION)
        .expect("expected single annotation node");

    let unescaped = single
        .children()
        .find(|c| c.kind() == UNESCAPED_ANNOTATION)
        .expect("expected unescaped annotation");

    assert_eq!(unescaped.first_token().unwrap().kind(), IDENTIFIER_TOKEN);
}

#[test]
fn parses_use_site_target_annotation() {
    let parse = make_parser(annotation);
    let node = parse("@field:Foo").syntax();

    let target = node
        .descendants()
        .find(|c| c.kind() == ANNOTATION_USE_SITE_TARGET)
        .expect("expected use-site target");

    assert_eq!(target.first_token().unwrap().kind(), AT_NO_WS);

    let unescaped = node
        .descendants()
        .find(|c| c.kind() == UNESCAPED_ANNOTATION)
        .expect("expected unescaped annotation");
    assert_eq!(unescaped.first_token().unwrap().kind(), IDENTIFIER_TOKEN);
}

#[test]
fn parses_multi_annotation_list() {
    let parse = make_parser(annotation);
    let node = parse("@[Foo Bar]").syntax();

    let multi = node
        .children()
        .find(|c| c.kind() == MULTI_ANNOTATION)
        .expect("expected multi annotation");

    let annotations: Vec<_> = multi
        .children()
        .filter(|c| c.kind() == UNESCAPED_ANNOTATION)
        .collect();

    assert_eq!(annotations.len(), 2);
    for ann in annotations {
        assert_eq!(ann.first_token().unwrap().kind(), IDENTIFIER_TOKEN);
    }
}

#[test]
fn recovers_from_missing_annotation_type() {
    let parse = make_parser(annotation);
    let parsed = parse("@");

    assert!(!parsed.errors.is_empty());
    let node = parsed.syntax();
    assert_eq!(node.kind(), ANNOTATION);
}
