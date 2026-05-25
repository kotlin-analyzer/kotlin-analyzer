use syntax::{SyntaxKind::*, T};

use super::classes::constructor_invocation;
use super::types::user_type;
use crate::{CompletedMarker, Parser, TokenSet};

const ANNO_RECOVERY: TokenSet = TokenSet::new(&[R_SQUARE, SEMICOLON, NL, R_CURL, EOF]);

pub(crate) fn annotation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !starts_annotation(parser) {
        return None;
    }
    let m = parser.start();
    if single_or_multi_annotation(parser).is_none() {
        m.abandon(parser);
        return None;
    }
    Some(m.complete(parser, ANNOTATION))
}

fn single_or_multi_annotation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    annotation_use_site_target_or_at(parser);

    if !parser.eat(T!['[']) {
        // test single_annotation
        // fun foo() {
        //    @get:Anno1
        //    val x: Int
        //    @CustomAnno
        //    var y: Int
        // }
        unescaped_annotation(parser);
        return Some(m.complete(parser, SINGLE_ANNOTATION));
    }

    // test multi_annotation
    // fun foo() {
    //    @get:[Anno1 Anno2]
    //    val x: Int
    //    @set:[Anno3]
    //    var y: Int
    // }

    let mut parsed = 0;
    while !matches!(parser.current(), T![']'] | EOF) {
        if unescaped_annotation(parser).is_some() {
            parsed += 1;
        } else {
            break;
        }
    }

    if parsed == 0 {
        parser.error("expected at least one annotation inside brackets");
    }

    if !parser.eat(T![']']) {
        parser.err_recover("expected ']' to close annotation list", ANNO_RECOVERY);
    }

    Some(m.complete(parser, MULTI_ANNOTATION))
}

enum AnnStep {
    At,
    Receiver,
}

pub(crate) fn starts_annotation(parser: &mut Parser<'_>) -> bool {
    matches!(parser.current(), AT_NO_WS | AT_PRE_WS)
}

fn annotation_use_site_target_or_at(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    parser.bump_any();

    if matches!(
        parser.current(),
        FIELD | PROPERTY | GET | SET | RECEIVER | PARAM | SET_PARAM | DELEGATE
    ) {
        parser.bump_any();
    } else {
        m.abandon(parser);
        return None;
    }
    if !parser.eat(COLON) {
        parser.err_recover("expected ':' after use-site target", ANNO_RECOVERY);
    }
    Some(m.complete(parser, ANNOTATION_USE_SITE_TARGET))
}

pub(crate) fn unescaped_annotation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    if let Some(user_type_marker) = user_type(parser) {
        constructor_invocation(parser, user_type_marker, true);
        Some(m.complete(parser, UNESCAPED_ANNOTATION))
    } else {
        m.abandon(parser);
        None
    }
}
