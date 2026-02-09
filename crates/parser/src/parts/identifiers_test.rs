use super::identifiers::{identifier, simple_identifier};
use crate::test_utils::{make_bool_parser, make_parser};
use ast::syntax::SyntaxKind::{self, *};

#[rstest::rstest]
#[case::_abstract("abstract", ABSTRACT)]
#[case::annotation("annotation", ANNOTATION)]
#[case::by("by", BY)]
#[case::catch("catch", CATCH)]
#[case::companion("companion", COMPANION)]
#[case::constructor("constructor", CONSTRUCTOR)]
#[case::crossinline("crossinline", CROSS_INLINE)]
#[case::data("data", DATA)]
#[case::dynamic("dynamic", DYNAMIC)]
#[case::_enum("enum", ENUM)]
#[case::external("external", EXTERNAL)]
#[case::_final("final", FINAL)]
#[case::finally("finally", FINALLY)]
#[case::get("get", GET)]
#[case::import("import", IMPORT)]
#[case::infix("infix", INFIX)]
#[case::init("init", INIT)]
#[case::inline("inline", INLINE)]
#[case::inner("inner", INNER)]
#[case::internal("internal", INTERNAL)]
#[case::lateinit("lateinit", LATEINIT)]
#[case::noinline("noinline", NO_INLINE)]
#[case::open("open", OPEN)]
#[case::operator("operator", OPERATOR)]
#[case::out("out", OUT)]
#[case::_override("override", OVERRIDE)]
#[case::private("private", PRIVATE)]
#[case::protected("protected", PROTECTED)]
#[case::public("public", PUBLIC)]
#[case::reified("reified", REIFIED)]
#[case::sealed("sealed", SEALED)]
#[case::tailrec("tailrec", TAILREC)]
#[case::set("set", SET)]
#[case::vararg("vararg", VAR_ARG)]
#[case::field("field", FIELD)]
#[case::property("property", PROPERTY)]
#[case::receiver("receiver", RECEIVER)]
#[case::param("param", PARAM)]
#[case::setparam("setparam", SET_PARAM)]
#[case::delegate("delegate", DELEGATE)]
#[case::file("file", FILE)]
#[case::expect("expect", EXPECT)]
#[case::actual("actual", ACTUAL)]
#[case::_const("const", CONST)]
#[case::value("value", VALUE)]
#[case::_where("where", WHERE)]
#[case::suspend("suspend", SUSPEND)]
#[case::identifier("name", IDENTIFIER_TOKEN)]
#[allow(non_snake_case)]
fn simple_identifier_test(#[case] text: &str, #[case] expected_kind: SyntaxKind) {
    let parse = make_bool_parser(simple_identifier);
    let node = parse(text).syntax();
    let kind = node.kind();
    assert_eq!(kind, SIMPLE_IDENTIFIER);
    let token = node.first_token().unwrap().kind();
    assert_eq!(token, expected_kind);
}
#[rstest::rstest]
#[case::single_identifier("name", &[IDENTIFIER_TOKEN])]
#[case::dotted_identifiers("foo.bar", &[IDENTIFIER_TOKEN, IDENTIFIER_TOKEN])]
#[case::keywords_allowed("abstract.data", &[ABSTRACT, DATA])]
fn identifier_parses_segments(#[case] text: &str, #[case] expected_tokens: &[SyntaxKind]) {
    let parse = make_parser(identifier);
    let node = parse(text).syntax();
    assert_eq!(node.kind(), IDENTIFIER);

    let segments: Vec<SyntaxKind> = node
        .children()
        .filter(|child| child.kind() == SIMPLE_IDENTIFIER)
        .map(|child| child.first_token().unwrap().kind())
        .collect();

    assert_eq!(segments.as_slice(), expected_tokens);
}
