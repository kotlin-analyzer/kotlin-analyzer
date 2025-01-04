#![allow(unused)]
use super::*;
use crate::syntax::{
    FileAnnotation, KotlinFile, Nl, PackageHeader, Root, ShebangLine,
    SyntaxKind::{self, *},
};

fn make_green() -> SyntaxNode {
    let mut builder = rowan::GreenNodeBuilder::new();
    builder.start_node(ROOT.into());
    builder.start_node(KOTLIN_FILE.into());

    builder.start_node(SHEBANG_LINE.into());
    builder.token(SHEBANG_LINE.into(), "#!bin/bash python -c");
    builder.finish_node();

    builder.start_node(NL.into());
    builder.token(NL.into(), "\\n");
    builder.finish_node();

    builder.start_node(NL.into());
    builder.token(NL.into(), "\\n");
    builder.finish_node();

    builder.start_node(PACKAGE_HEADER.into());
    builder.token(PACKAGE_HEADER.into(), "package dev.ikeze");
    builder.finish_node();

    builder.finish_node(); // KOTLIN_FILE
    builder.finish_node(); // ROOT

    SyntaxNode::new_root(builder.finish())
}

#[test]
fn basic() {
    let node = make_green();

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

    assert!(matches!(
        <(ShebangLine, Nl, Nl, PackageHeader)>::inner_cast(&mut kt_file.children().peekable()),
        Some((ShebangLine(..), Nl(..), Nl(..), PackageHeader(..)))
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
        <(ShebangLine, ZeroOrMore<Nl>, PackageHeader)>::inner_cast(
            &mut kt_file.children().peekable()
        ),
        Some((ShebangLine(..), ZeroOrMore(arr), PackageHeader(..)))
        if matches!(arr.as_slice(), [Nl(..), Nl(..)])
    ));
}

#[test]
fn optional_simple() {
    let kt_file = make_green().first_child().unwrap();

    let res = Option::<ShebangLine>::inner_cast(&mut kt_file.children().peekable());

    assert!(matches!(res.unwrap(), Some(ShebangLine(..))));

    let res = Option::<FileAnnotation>::inner_cast(&mut kt_file.children().peekable());

    assert!(matches!(res.unwrap(), None));

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
        Option<FileAnnotation>,
        PackageHeader,
    )>::inner_cast(&mut kt_file.children().peekable());

    assert!(matches!(
        res,
        Some((ShebangLine(..), ZeroOrMore(arr), None, PackageHeader(..)))
        if matches!(arr.as_slice(), [Nl(..), Nl(..)])
    ));
}
