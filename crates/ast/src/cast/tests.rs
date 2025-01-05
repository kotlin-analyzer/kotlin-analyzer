use super::*;
use crate::syntax::{
    FileAnnotation, KotlinFile, Nl, PackageHeader, Root, ShebangLine, SyntaxKind::*, SyntaxNode,
};

macro_rules! make_syntax_node {
    (nl $b:ident) => {
        $b.start_node(NL.into());
        $b.token(NL.into(), r"\n");
        $b.finish_node();
    };
    (shebang_line $b:ident) => {
        $b.start_node(SHEBANG_LINE.into());

        $b.start_node(SHEBANG_LINE_TOKEN.into());
        $b.token(SHEBANG_LINE_TOKEN.into(), "#!bin/bash python -c");
        $b.finish_node();

        make_syntax_node!(nl $b);
        make_syntax_node!(nl $b); // can have extra lines
        $b.finish_node();
    };
    (simple_identifier $b:ident) => {
        $b.start_node(SIMPLE_IDENTIFIER.into());

        $b.start_node(IDENTIFIER_TOKEN.into());
        $b.token(IDENTIFIER_TOKEN.into(), "personName");
        $b.finish_node();

        $b.finish_node();
    };
    (identifier $b:ident) => {
        $b.start_node(IDENTIFIER.into());
        make_syntax_node!(simple_identifier $b);
        // only the first simple_identifier is needed, but we will add more
        // repeating
        make_syntax_node!(nl $b);
        $b.token(DOT.into(), ".");
        make_syntax_node!(simple_identifier $b);
        // end repeating
        $b.finish_node();
    };
    (user_type $b:ident) => {
        $b.start_node(USER_TYPE.into());
        $b.start_node(SIMPLE_USER_TYPE.into());
        make_syntax_node!(simple_identifier $b);
        $b.finish_node();
        $b.finish_node();
    };
    (unescaped_annotation $b:ident) => {
        $b.start_node(UNESCAPED_ANNOTATION.into());
        make_syntax_node!(user_type $b);
        $b.finish_node();
    };
    (file_annotation $b:ident) => {
        $b.start_node(FILE_ANNOTATION.into());

        $b.token(AT_NO_WS.into(), "@");
        $b.token(FILE.into(), "file");

        make_syntax_node!(nl $b);
        make_syntax_node!(nl $b);

        $b.token(COLON.into(), ":");

        // skipping optional lines

        $b.start_node(L_SQUARE.into());
        $b.token(L_SQUARE.into(), "[");
        $b.finish_node();

        make_syntax_node!(unescaped_annotation $b);
        make_syntax_node!(unescaped_annotation $b);

        $b.start_node(R_SQUARE.into());
        $b.token(R_SQUARE.into(), "[");
        $b.finish_node();

        $b.finish_node();
    };
    (package_header $b:ident) => {
        $b.start_node(PACKAGE_HEADER.into());

        $b.token(PACKAGE.into(), "package");
        make_syntax_node!(identifier $b);
        make_syntax_node!(semi $b);

        $b.finish_node();
    };
    (semi $b:ident) => {
        $b.start_node(SEMI.into());
        $b.start_node(SEMICOLON.into());
        $b.token(SEMICOLON.into(), ";");
        $b.finish_node();
        $b.finish_node();
    };
    (kotlin_file [$($ex: ident),*]) => {{
        let mut builder = rowan::GreenNodeBuilder::new();
        builder.start_node(ROOT.into());
        builder.start_node(KOTLIN_FILE.into());

        $(make_syntax_node!($ex builder);)*

        builder.finish_node(); // KOTLIN_FILE
        builder.finish_node(); // ROOT
        SyntaxNode::new_root(builder.finish())
    }};
    (script $($ex: ident),*) => {{
        let mut builder = rowan::GreenNodeBuilder::new();
        builder.start_node(ROOT.into());
        builder.start_node(SCRIPT.into());

        $(make_syntax_node!($ex builder);)*

        builder.finish_node(); // SCRIPT
        builder.finish_node(); // ROOT
        SyntaxNode::new_root(builder.finish())
    }};
}

fn make_green() -> SyntaxNode {
    make_syntax_node!(kotlin_file [shebang_line, nl, nl, file_annotation, file_annotation, package_header])
}

#[test]
fn basic() {
    let node = make_syntax_node!(kotlin_file [nl, shebang_line, nl]);
    assert!(Root::cast(node).is_some())
}

#[test]
fn strict() {
    let kt_file = make_green().first_child().unwrap();

    assert!(matches!(KotlinFile::cast(kt_file), Some(KotlinFile(_))));
}

#[test]
fn tuple() {
    let kt_file = make_green().first_child().unwrap();

    let children = &mut kt_file.children().peekable();

    assert!(matches!(
        <(ShebangLine, Nl, Nl, FileAnnotation)>::inner_cast(children),
        Some((ShebangLine(..), Nl(..), Nl(..), FileAnnotation(..)))
    ));
}

#[test]
fn zero_or_more1() {
    let kt_file = make_green().first_child().unwrap();
    let children = &mut kt_file.children().peekable();

    let res = <(ShebangLine, ZeroOrMore<Nl>)>::inner_cast(children);

    assert!(matches!(res,
        Some((ShebangLine(..), ZeroOrMore(arr)))
        if matches!(arr.as_slice(), [Nl(..), Nl(..)])
    ));
}

#[test]
fn zero_or_more() {
    let kt_file = make_green().first_child().unwrap();

    assert!(matches!(
        <(ShebangLine, ZeroOrMore<Nl>, FileAnnotation)>::inner_cast(
            &mut kt_file.children().peekable()
        ),
        Some((ShebangLine(..), ZeroOrMore(arr), FileAnnotation(..)))
        if matches!(arr.as_slice(), [Nl(..), Nl(..)])
    ));
}

#[test]
fn optional_simple() {
    let kt_file = make_green().first_child().unwrap();

    let res = Option::<ShebangLine>::inner_cast(&mut kt_file.children().peekable());

    assert!(matches!(res.unwrap(), Some(ShebangLine(..))));

    let res = Option::<FileAnnotation>::inner_cast(&mut kt_file.children().peekable());

    assert!(res.unwrap().is_none());

    let res =
        <(Option<ShebangLine>, ZeroOrMore<Nl>)>::inner_cast(&mut kt_file.children().peekable());

    assert!(matches!(
        res.unwrap(),
        (Some(ShebangLine(..)), ZeroOrMore(arr)) if matches!(arr.as_slice(), [Nl(..), Nl(..)])
    ));

    let res =
        <(ZeroOrMore<ShebangLine>, Option<Nl>)>::inner_cast(&mut kt_file.children().peekable());

    assert!(matches!(
        res.unwrap(),
        (ZeroOrMore(arr), Some(Nl(..))) if matches!(arr.as_slice(), [ShebangLine(..)])
    ));

    let res = <(ZeroOrMore<ShebangLine>, Option<FileAnnotation>)>::inner_cast(
        &mut kt_file.children().peekable(),
    );

    assert!(matches!(
        res.unwrap(),
        (ZeroOrMore(arr), None) if matches!(arr.as_slice(), [ShebangLine(..)])
    ));
}

#[test]
fn optional() {
    let kt_file = make_green().first_child().unwrap();

    let res = <(
        ShebangLine,
        ZeroOrMore<Nl>,
        ZeroOrMore<FileAnnotation>,
        Option<PackageHeader>,
    )>::inner_cast(&mut kt_file.children().peekable());

    dbg!(&res);
    assert!(matches!(
        res,
        Some((ShebangLine(..), ZeroOrMore(arr), ZeroOrMore(fas), Some(PackageHeader(..))))
        if matches!(arr.as_slice(), [Nl(..), Nl(..)]) &&  matches!(fas.as_slice(), [FileAnnotation(..), FileAnnotation(..)])
    ));
}

#[test]
fn generated_node_cast_works() {
    let file_annotion = make_green()
        .first_child()
        .unwrap()
        .children()
        .find_map(FileAnnotation::cast);

    dbg!(file_annotion);
}
