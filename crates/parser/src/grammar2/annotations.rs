use syntax::SyntaxKind::*;

// use super::types::user_type;
use crate::ra::{CompletedMarker, Parser, TokenSet};

const ANNO_RECOVERY: TokenSet = TokenSet::new(&[R_SQUARE, SEMICOLON, NL, R_CURL, EOF]);

pub(crate) fn annotation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    dbg!("annotation: {:?}", parser.current());
    if single_and_multi_annotation(parser).is_none() {
        m.abandon(parser);
        return None;
    }
    Some(m.complete(parser, ANNOTATION))
}

// test multi_annotation
// fun foo() {
//    @get:[Anno1 Anno2]
//    val x: Int
//    @set:[Anno3]
//    var y: Int
// }
fn single_and_multi_annotation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    if annotation_use_site_target(parser).is_none() {
        if let AT_NO_WS | AT_PRE_WS = parser.current() {
            parser.bump_any()
        } else {
            m.abandon(parser);
            return None;
        }
    } else {
        parser.eat_newlines();
    }
    if !parser.eat(L_SQUARE) {
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

    let mut parsed = 0;
    while !matches!(parser.current(), R_SQUARE | EOF) {
        if unescaped_annotation(parser).is_some() {
            parsed += 1;
        } else {
            break;
        }
    }

    if parsed == 0 {
        parser.error("expected at least one annotation inside brackets");
    }

    if !parser.eat(R_SQUARE) {
        parser.err_recover("expected ']' to close annotation list", ANNO_RECOVERY);
    }

    Some(m.complete(parser, MULTI_ANNOTATION))
}

fn annotation_use_site_target(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();

    if let AT_NO_WS | AT_PRE_WS = parser.current() {
        parser.bump_any()
    } else {
        m.abandon(parser);
        return None;
    }

    if matches!(
        parser.current(),
        FIELD | PROPERTY | GET | SET | RECEIVER | PARAM | SET_PARAM | DELEGATE
    ) {
        parser.bump_any();
    } else {
        m.abandon(parser);
        return None;
    }

    parser.eat_newlines();

    if !parser.eat(COLON) {
        parser.err_recover("expected ':' after use-site target", ANNO_RECOVERY);
    }

    Some(m.complete(parser, ANNOTATION_USE_SITE_TARGET))
}

pub(crate) fn unescaped_annotation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    // FIXME: handle constructor invocation
    // user_type(parser);
    Some(m.complete(parser, UNESCAPED_ANNOTATION))
}
