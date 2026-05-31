//! This module contains parsing functions for expressions and their components.
//! Expressions are similar to assigments, this makes parsing them a bit tricky, because of the ambiguity.
//! The code here tries to factor that into their logic.

use crate::{SyntaxKind::*, T};
use casey::shouty;

use super::annotations::{annotation, starts_annotation};
use super::class_members::{
    context_parameter_list, function_body, multi_variable_declaration, parameters_with_opt_type,
    variable_declaration,
};
use super::classes::{class_body, delegation_specifiers, type_constraints};
use super::identifiers::{is_simple_identifier, simple_identifier};
use super::statements::{CaptureStmts, block, control_structure_body, label, semi, statements};
use super::types::{RecvType, UserType, receiver_type, ty, type_projection};
use crate::{CompletedMarker, Parser};

macro_rules! define_operator {
    ($name:ident, $mat:pat) => {
        pub(crate) mod $name {
            use super::*;
            pub(crate) fn parse(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
                match parser.current() {
                    $mat => {
                        let m = parser.start();
                        parser.bump_any();
                        Some(m.complete(parser, shouty!($name)))
                    }
                    _ => None,
                }
            }

            pub(crate) fn is(parser: &mut Parser<'_>) -> bool {
                matches!(parser.current(), $mat)
            }
        }
    };
}

pub(crate) fn expression(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(ex) = disjunction(parser) {
        let m = ex.clone().marker().precede(parser);
        let cm = m.complete(parser, EXPRESSION);
        let res = match ex {
            Expression::Affixed(AffixedExpression::Postfix(_)) => {
                Expression::Affixed(AffixedExpression::Postfix(cm))
            }
            Expression::Affixed(AffixedExpression::Prefix(_)) => {
                Expression::Affixed(AffixedExpression::Postfix(cm))
            }
            Expression::Other(_) => Expression::Other(cm),
        };
        Some(res)
    } else {
        None
    }
}

fn disjunction(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(ex) = conjunction(parser) {
        if parser.at(T![&&]) {
            let m = ex.marker().precede(parser);
            while parser.eat(T![||]) {
                if conjunction(parser).is_none() {
                    parser.error("expected an expression :#DE");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, DISJUNCTION)))
        } else {
            Some(ex)
        }
    } else {
        None
    }
}

fn conjunction(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(ex) = equality(parser) {
        if parser.at(T![&&]) {
            let m = ex.marker().precede(parser);
            while parser.eat(T![&&]) {
                if equality(parser).is_none() {
                    parser.error("expected an expression :#CE");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, CONJUNCTION)))
        } else {
            Some(ex)
        }
    } else {
        None
    }
}

fn equality(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(ex) = comparison(parser) {
        if equality_operator::is(parser) {
            let m = ex.marker().precede(parser);
            while equality_operator::parse(parser).is_some() {
                if comparison(parser).is_none() {
                    parser.error("expected an expression :#EE");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, EQUALITY)))
        } else {
            Some(ex)
        }
    } else {
        None
    }
}

fn comparison(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(ex) = generic_call_like_comparison(parser) {
        if comparison_operator::is(parser) {
            let m = ex.marker().precede(parser);
            while comparison_operator::parse(parser).is_some() {
                if generic_call_like_comparison(parser).is_none() {
                    parser.error("expected an expression :#CCE");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, COMPARISON)))
        } else {
            Some(ex)
        }
    } else {
        None
    }
}

fn generic_call_like_comparison(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(ex) = infix_operation(parser) {
        let m = ex.clone().marker().precede(parser);
        let mut seen = 0;
        while call_suffix(parser, CallSuffix::Full).is_some() {
            seen += 1;
        }
        if seen > 0 {
            Some(Expression::Other(m.complete(parser, GENERIC_CALL_LIKE_COMPARISON)))
        } else {
            m.abandon(parser);
            Some(ex)
        }
    } else {
        None
    }
}

fn infix_operation(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(cm) = elvis_expression(parser) {
        if is_operator::is(parser) || in_operator::is(parser) {
            let m = cm.marker().precede(parser);
            while is_or_in_operator_expr(parser) {}
            Some(Expression::Other(m.complete(parser, INFIX_OPERATION)))
        } else {
            Some(cm)
        }
    } else {
        None
    }
}

fn is_or_in_operator_expr(parser: &mut Parser<'_>) -> bool {
    if is_operator::is(parser) {
        if ty(parser).is_none() {
            parser.error("expected a type");
        } else {
            return true;
        }
    } else if in_operator::is(parser) {
        if elvis_expression(parser).is_none() {
            parser.error("expected an expression :#IOE");
        } else {
            return true;
        }
    }
    false
}

fn elvis_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(cm) = infix_function_call(parser) {
        if is_elvis(parser) {
            let m = cm.marker().precede(parser);
            while elvis(parser).is_some() {
                if infix_function_call(parser).is_none() {
                    parser.error("expected an expression :#ElvE");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, ELVIS_EXPRESSION)))
        } else {
            Some(cm)
        }
    } else {
        None
    }
}

fn is_elvis(parser: &mut Parser<'_>) -> bool {
    parser.at(T![?]) && !parser.nth_at(1, T![:])
}

fn elvis(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_elvis(parser) {
        let m = parser.start();
        parser.bump(T![?]);
        parser.bump(T![:]);
        Some(m.complete(parser, ELVIS))
    } else {
        None
    }
}

fn infix_function_call(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(cm) = range_expression(parser) {
        if is_simple_identifier(parser) {
            let m = cm.marker().precede(parser);
            while simple_identifier(parser).is_some() {
                if range_expression(parser).is_none() {
                    parser.error("expected an expression :#IFC");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, INFIX_FUNCTION_CALL)))
        } else {
            Some(cm)
        }
    } else {
        None
    }
}

fn range_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(cm) = additive_expression(parser) {
        if range_operator::is(parser) {
            let m = cm.marker().precede(parser);
            while range_operator::parse(parser) {
                if additive_expression(parser).is_none() {
                    parser.error("expected an expression :#RE");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, RANGE_EXPRESSION)))
        } else {
            Some(cm)
        }
    } else {
        None
    }
}

fn additive_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(cm) = multiplicative_expression(parser) {
        if additive_operator::is(parser) {
            let m = cm.marker().precede(parser);
            while additive_operator::parse(parser).is_some() {
                if multiplicative_expression(parser).is_none() {
                    parser.error("expected an expression :#AE");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, ADDITIVE_EXPRESSION)))
        } else {
            Some(cm)
        }
    } else {
        None
    }
}

fn multiplicative_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(cm) = as_expression(parser) {
        if multiplicative_operator::is(parser) {
            let m = cm.marker().precede(parser);
            while multiplicative_operator::parse(parser).is_some() {
                if as_expression(parser).is_none() {
                    parser.error("expected an expression :#ME");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, MULTIPLICATIVE_EXPRESSION)))
        } else {
            Some(cm)
        }
    } else {
        None
    }
}

fn as_expression(parser: &mut Parser<'_>) -> Option<Expression> {
    if let Some(cm) = prefix_unary_expression(parser) {
        if as_operator::is(parser) {
            let m = cm.marker().precede(parser);
            while as_operator::parse(parser).is_some() {
                if ty(parser).is_none() {
                    parser.error("expected a type");
                    break;
                }
            }
            Some(Expression::Other(m.complete(parser, AS_EXPRESSION)))
        } else {
            Some(Expression::Affixed(cm))
        }
    } else {
        None
    }
}

fn parenthesized_directly_assignable_expression(
    parser: &mut Parser<'_>,
) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['(']);
    if directly_assignable_expression(parser).is_none() {
        parser.error("expected an expression :#PDAE");
    }
    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }
    Some(m.complete(parser, PARENTHESIZED_DIRECTLY_ASSIGNABLE_EXPRESSION))
}

fn directly_assignable_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    postfix_unary_expression(parser)
        .map(|cm| {
            let m = cm.marker().precede(parser);
            assignable_suffix(parser); // record error if none
            m.complete(parser, DIRECTLY_ASSIGNABLE_EXPRESSION)
        })
        .or_else(|| {
            simple_identifier(parser)
                .or_else(|| parenthesized_directly_assignable_expression(parser))
                .map(|cm| cm.precede(parser).complete(parser, DIRECTLY_ASSIGNABLE_EXPRESSION))
        })
}

fn parenthesized_assignable_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['(']);
    if assignable_expression(parser).is_none() {
        parser.error("expected an expression :#PAE");
    }
    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }
    Some(m.complete(parser, PARENTHESIZED_ASSIGNABLE_EXPRESSION))
}

fn assignable_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    parenthesized_assignable_expression(parser)
        .or_else(|| prefix_unary_expression(parser).map(|pue| pue.marker()))
}

#[derive(Clone)]
pub(crate) enum AffixedExpression {
    Prefix(CompletedMarker),
    Postfix(CompletedMarker),
}

impl AffixedExpression {
    fn marker(self) -> CompletedMarker {
        match self {
            AffixedExpression::Prefix(marker) | AffixedExpression::Postfix(marker) => marker,
        }
    }
}

#[derive(Clone)]
pub(crate) enum Expression {
    Affixed(AffixedExpression),
    Other(CompletedMarker),
}

impl Expression {
    pub(crate) fn marker(self) -> CompletedMarker {
        match self {
            Expression::Affixed(affixed) => affixed.marker(),
            Expression::Other(marker) => marker,
        }
    }
}

pub(crate) fn prefix_unary_expression(parser: &mut Parser<'_>) -> Option<AffixedExpression> {
    let m = parser.start();
    let mut has_prefix = false;

    while unary_prefix(parser).is_some() {
        has_prefix = true;
    }
    let postfix = postfix_unary_expression(parser);

    if postfix.is_some() {
        if has_prefix {
            Some(AffixedExpression::Prefix(m.complete(parser, PREFIX_UNARY_EXPRESSION)))
        } else {
            m.abandon(parser);
            postfix
        }
    } else {
        m.abandon(parser);
        None
    }
}

fn unary_prefix(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    annotation(parser).or_else(|| prefix_unary_operator::parse(parser)).or_else(|| label(parser))
}

fn postfix_unary_expression(parser: &mut Parser<'_>) -> Option<AffixedExpression> {
    primary_expression(parser).map(|aff| {
        let m = aff.precede(parser);
        while postfix_unary_suffix(parser).is_some() {}
        AffixedExpression::Postfix(m.complete(parser, POSTFIX_UNARY_EXPRESSION))
    })
}

fn postfix_unary_suffix(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    call_suffix(parser, CallSuffix::AcceptTypeArguments)
        .or_else(|| postfix_unary_operator::parse(parser))
        .or_else(|| indexing_suffix(parser))
        .or_else(|| navigation_suffix(parser))
}

pub(crate) fn assignable_suffix(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    indexing_suffix(parser).or_else(|| navigation_suffix(parser)).or_else(|| type_arguments(parser))
}

fn indexing_suffix(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['[']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['[']);

    if expression(parser).is_none() {
        parser.error("expected an expression :#IE");
    } else {
        while parser.eat(T![,]) && expression(parser).is_some() {}
    }

    if !parser.eat(T![']']) {
        parser.error("expected `]`");
    }
    Some(m.complete(parser, INDEXING_SUFFIX))
}

fn navigation_suffix(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = member_access_operator(parser) {
        let m = cm.precede(parser);
        if simple_identifier(parser).or_else(|| parenthesized_expression(parser)).is_none()
            && !parser.eat(T![class])
        {
            parser.error("expected an expression or `class`");
        }
        Some(m.complete(parser, NAVIGATION_SUFFIX))
    } else {
        None
    }
}

enum CallSuffix {
    /// Allows call_suffix to succeed with just type arguments.
    AcceptTypeArguments,
    Full,
}

fn starts_call_suffix(parser: &mut Parser<'_>) -> bool {
    parser.at(T!['('])
        || parser.at(T![<])
        || parser.at(T!['{'])
        || (is_simple_identifier(parser) && !parser.nth_at(1, T![@])) // label
        || starts_annotation(parser)
}

fn call_suffix(parser: &mut Parser<'_>, res: CallSuffix) -> Option<CompletedMarker> {
    let m = parser.start();
    let ta = type_arguments(parser);
    let va = value_arguments(parser);
    let lambda = annotated_lambda(parser);

    match (va.is_none(), lambda.is_none(), res) {
        (true, true, CallSuffix::AcceptTypeArguments) => {
            m.abandon(parser);
            ta // it is valid to have just type arguments.
        }
        (true, true, _) => {
            m.abandon(parser);
            None
        }
        (_, _, _) => Some(m.complete(parser, CALL_SUFFIX)),
    }
}

fn annotated_lambda(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    while annotation(parser).is_some() {}
    let has_label = label(parser).is_some();
    if lambda_literal(parser).is_none() && !has_label {
        m.abandon(parser);
        return None;
    }
    Some(m.complete(parser, ANNOTATED_LAMBDA))
}

fn type_arguments(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T![<]) {
        return None;
    }
    let m = parser.start();
    parser.bump(T![<]);
    if type_projection(parser).is_some() {
        while parser.eat(T![,]) && type_projection(parser).is_some() {}
    }
    if !parser.eat(T![>]) {
        parser.error("expected `>`");
    }
    Some(m.complete(parser, TYPE_ARGUMENTS))
}

pub(crate) fn value_arguments(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['(']);
    if value_argument(parser).is_some() {
        while parser.eat(T![,]) && value_argument(parser).is_some() {}
    }
    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }
    Some(m.complete(parser, VALUE_ARGUMENTS))
}

fn value_argument(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    let mut seen = false;
    if annotation(parser).is_some() {
        seen = true;
    }
    if is_simple_identifier(parser) && parser.nth_at(1, T![=]) {
        simple_identifier(parser);
        parser.bump(T![=]);
        seen = true;
    }
    if parser.eat(T![*]) {
        seen = true;
    }
    let expr = expression(parser);

    if expr.is_none() {
        if seen {
            parser.error("expected an expression :#VA");
        } else {
            m.abandon(parser);
            return None;
        }
    }
    Some(m.complete(parser, VALUE_ARGUMENT))
}

fn primary_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    parenthesized_expression(parser)
        .or_else(|| collection_literal(parser))
        .or_else(|| literal_constant(parser))
        .or_else(|| string_literal(parser))
        .or_else(|| function_literal(parser))
        .or_else(|| object_literal(parser))
        .or_else(|| this_expresssion(parser))
        .or_else(|| super_expression(parser))
        .or_else(|| if_expression(parser))
        .or_else(|| when_expression(parser))
        .or_else(|| try_expression(parser))
        .or_else(|| jump_expression(parser))
        .or_else(|| {
            // HGKIC: this is to prevent this from matching callable references, which also start with a simple identifier, but require a `::` after them.
            if is_simple_identifier(parser) && !parser.nth_at(1, T![::]) {
                simple_identifier(parser)
            } else {
                None
            }
        })
        .or_else(|| callable_reference(parser))
}

fn parenthesized_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['(']);
    if expression(parser).is_none() {
        parser.error("expected an expression :#PE");
    }
    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }
    Some(m.complete(parser, PARENTHESIZED_EXPRESSION))
}

fn collection_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['[']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['[']);
    if expression(parser).is_some() {
        while parser.eat(T![,]) && expression(parser).is_some() {}
    }
    if !parser.eat(T![']']) {
        parser.error("expected `]`");
    }
    Some(m.complete(parser, COLLECTION_LITERAL))
}

fn literal_constant(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        BOOL | INT | HEX | BIN | CHAR | REAL | NULL_KW | LONG | UNSIGNED => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, LITERAL_CONSTANT))
        }
        _ => None,
    }
}

fn string_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    line_string_literal(parser).or_else(|| multi_line_string_literal(parser))
}

fn line_string_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(QUOTE) {
        return None;
    }
    let m = parser.start();
    parser.bump(QUOTE);
    while line_string_content(parser).is_some() || line_string_expr(parser).is_some() {}
    if !parser.eat(QUOTE) {
        parser.error(r#"expected `"`"#);
    }
    Some(m.complete(parser, LINE_STRING_LITERAL))
}

fn multi_line_string_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(TRIPLE_QUOTE) {
        return None;
    }
    let m = parser.start();
    parser.bump(TRIPLE_QUOTE);
    while multi_line_string_content(parser).is_some()
        || multi_line_string_expr(parser).is_some()
        || parser.eat(MULTI_LINE_STRING_QUOTE)
    {}
    if !parser.eat(TRIPLE_QUOTE) {
        parser.error(r#"expected `"""`"#);
    }
    Some(m.complete(parser, MULTI_LINE_STRING_LITERAL))
}

fn line_string_content(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        TEXT | ESCAPED_CHAR | STR_REF => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, LINE_STRING_CONTENT))
        }
        _ => None,
    }
}

fn line_string_expr(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(STR_EXPR_START) {
        let m = parser.start();
        parser.bump(STR_EXPR_START);
        if expression(parser).is_none() {
            parser.error("expected an expression :#LSE");
        }
        if !parser.eat(T!['}']) {
            parser.error("expected `}`");
        }
        Some(m.complete(parser, LINE_STRING_EXPRESSION))
    } else {
        None
    }
}

fn multi_line_string_content(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        TEXT | MULTI_LINE_STRING_QUOTE | STR_REF => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, MULTI_LINE_STRING_CONTENT))
        }
        _ => None,
    }
}

fn multi_line_string_expr(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(STR_EXPR_START) {
        let m = parser.start();
        parser.bump(STR_EXPR_START);
        if expression(parser).is_none() {
            parser.error("expected an expression :#MLSE");
        }
        if !parser.eat(T!['}']) {
            parser.error("expected `}`");
        }
        Some(m.complete(parser, MULTI_LINE_STRING_EXPRESSION))
    } else {
        None
    }
}

fn function_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    lambda_literal(parser).or_else(|| anonymous_function(parser))
}

fn lambda_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['{']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['{']);
    if lambda_parameters(parser).is_some() && !parser.eat(T![->]) {
        parser.error("expected `->`");
    }
    statements(parser, CaptureStmts::Capture);
    if !parser.eat(T!['}']) {
        parser.error("expected `}`");
    }
    Some(m.complete(parser, LAMBDA_LITERAL))
}

fn lambda_parameters(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = lambda_parameter(parser) {
        let m = cm.precede(parser);
        while parser.eat(T![,]) && lambda_parameter(parser).is_some() {}
        Some(m.complete(parser, LAMBDA_PARAMETERS))
    } else {
        None
    }
}

fn lambda_parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    multi_variable_declaration(parser)
        .or_else(|| variable_declaration(parser))
        .map(|cm| cm.precede(parser).complete(parser, LAMBDA_PARAMETER))
}

fn anonymous_function(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();

    match (parser.current(), parser.nth(1)) {
        (T![fun], _) => {
            parser.bump(T![fun]);
        }
        (T![suspend], T![fun]) => {
            parser.bump(T![suspend]);
            parser.bump(T![fun]);
        }
        (T![suspend], T![context]) => {
            parser.bump(T![suspend]);
            if context_parameter_list(parser).is_none() {
                parser.error("expected `context` parameters");
            }
            if !parser.eat(T![fun]) {
                parser.error("expected `fun` keyword");
                // TODO: error recovery if `fun` is missing after context parameters
                m.abandon(parser);
                return None;
            }
        }
        (T![context], T!['(']) => {
            if context_parameter_list(parser).is_none() {
                parser.error("expected `context` parameters");
            }
            parser.eat(T![suspend]); // optional
            if !parser.eat(T![fun]) {
                parser.error("expected `fun` keyword");
            }
        }
        _ => {
            m.abandon(parser);
            return None;
        }
    }

    if ty(parser).is_some() && !parser.eat(T![.]) {
        parser.error("expected `.`");
    }
    if parameters_with_opt_type(parser).is_none() {
        parser.error("expected `(`");
    }
    if parser.at(T![:]) {
        parser.bump(T![:]);
        if ty(parser).is_none() {
            parser.error("expected a return type");
        }
    }

    type_constraints(parser);
    function_body(parser);
    Some(m.complete(parser, ANONYMOUS_FUNCTION))
}

fn object_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !(parser.at(T![object]) || (parser.at(T![data]) && parser.nth_at(1, T![object]))) {
        return None;
    }
    let m = parser.start();

    parser.eat(T![data]);
    parser.bump(T![object]);

    if parser.at(T![:]) {
        parser.bump(T![:]);
        delegation_specifiers(parser);
    }

    class_body(parser, None, None);
    Some(m.complete(parser, OBJECT_LITERAL))
}

fn this_expresssion(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![this]) || parser.at(T![this@]) {
        let m = parser.start();
        parser.bump_any();
        Some(m.complete(parser, THIS_EXPRESSION))
    } else {
        None
    }
}

fn super_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![super]) {
        let m = parser.start();
        parser.bump(T![super]);
        if parser.at(T![<]) {
            parser.bump(T![<]);
            if ty(parser).is_none() {
                parser.error("expected a type");
            }
            if !parser.eat(T![>]) {
                parser.error("expected `>`");
            }
        }
        if parser.eat(T![@]) && simple_identifier(parser).is_none() {
            parser.error("expected an identifier after `@`");
        }
        Some(m.complete(parser, SUPER_EXPRESSION))
    } else if parser.at(T![super@]) {
        let m = parser.start();
        parser.bump(T![super@]);
        Some(m.complete(parser, SUPER_EXPRESSION))
    } else {
        None
    }
}

fn if_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T![if]) {
        return None;
    }
    let m = parser.start();
    parser.bump(T![if]);
    if !parser.eat(T!['(']) {
        parser.error("expected `(`");
    }
    if expression(parser).is_none() {
        parser.error("expected an expression :#IFE");
    }
    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }

    control_structure_body(parser);
    parser.eat(T![;]);
    if parser.eat(T![else]) && !parser.eat(T![;]) {
        control_structure_body(parser);
    }
    Some(m.complete(parser, IF_EXPRESSION))
}

fn when_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T![when]) {
        return None;
    }
    let m = parser.start();
    parser.bump(T![when]);
    when_subject(parser);
    if !parser.eat(T!['{']) {
        parser.error("expected `{`");
    }
    while when_entry(parser).is_some() {}
    if !parser.eat(T!['}']) {
        parser.error("expected `}`");
    }
    Some(m.complete(parser, WHEN_EXPRESSION))
}

fn when_subject(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['(']);
    while annotation(parser).is_some() {}
    if parser.at(T![val]) {
        parser.bump(T![val]);
        if variable_declaration(parser).is_none() {
            parser.error("expected an identifier");
        }
        if !parser.eat(T![=]) {
            parser.error("expected `=`");
        }
    }
    if expression(parser).is_none() {
        parser.error("expected an expression :#WSE");
    }

    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }
    Some(m.complete(parser, WHEN_SUBJECT))
}

fn when_entry(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    if parser.at(T![else]) {
        parser.bump(T![else]);
        if parser.eat(T![->]) {
            control_structure_body(parser);
        }
        semi(parser);
    } else if when_condition(parser).is_some() {
        while parser.eat(T![,]) && when_condition(parser).is_some() {}
        if parser.eat(T![->]) {
            control_structure_body(parser);
        }
        semi(parser);
    } else {
        m.abandon(parser);
        return None;
    }
    Some(m.complete(parser, WHEN_ENTRY))
}

fn when_condition(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    range_test(parser)
        .or_else(|| type_test(parser))
        .or_else(|| expression(parser).map(|e| e.marker()))
}

fn range_test(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = in_operator::parse(parser) {
        let m = cm.precede(parser);
        if expression(parser).is_none() {
            parser.error("expected an expression :#RTE");
        }
        Some(m.complete(parser, RANGE_TEST))
    } else {
        None
    }
}

fn type_test(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = is_operator::parse(parser) {
        let m = cm.precede(parser);
        if ty(parser).is_none() {
            parser.error("expected a type");
        }
        Some(m.complete(parser, TYPE_TEST))
    } else {
        None
    }
}

enum CallableReference {
    Partial(CompletedMarker),
    Full(CompletedMarker),
}

fn try_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T![try]) {
        return None;
    }
    let m = parser.start();
    parser.bump(T![try]);

    if block(parser).is_none() {
        parser.error("expected `{`");
    }
    let mut catch_or_finally = 0;
    while catch_block(parser).is_some() {
        catch_or_finally += 1;
    }
    finally_block(parser).inspect(|_| catch_or_finally += 1);
    if catch_or_finally == 0 {
        parser.error("expected at least one `catch` or `finally` block");
    }
    Some(m.complete(parser, TRY_EXPRESSION))
}

fn catch_block(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T![catch]) {
        return None;
    }
    let m = parser.start();
    parser.bump(T![catch]);

    if !parser.eat(T!['(']) {
        parser.error("expected `(`");
    } else {
        while annotation(parser).is_some() {}
        if simple_identifier(parser).is_some() {
            if !parser.eat(T![:]) {
                parser.error("expected `:`");
            }
            if ty(parser).is_none() {
                parser.error("expected a type");
            }
            parser.eat(T![,]);
        } else {
            parser.error("missing the exception to be caught");
        }
        if !parser.eat(T![')']) {
            parser.error("expected `)`");
        }
    }
    if block(parser).is_none() {
        parser.error("expected `{`");
    }
    Some(m.complete(parser, CATCH_BLOCK))
}

fn finally_block(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T![finally]) {
        return None;
    }
    let m = parser.start();
    parser.bump(T![finally]);
    if block(parser).is_none() {
        parser.error("expected `{`");
    }
    Some(m.complete(parser, FINALLY_BLOCK))
}

fn jump_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    match parser.current() {
        T![break] | T![continue] | T![break@] | T![continue@] => {
            parser.bump_any();
        }
        T![return] | T![return@] => {
            parser.bump_any();
            expression(parser);
        }
        T![throw] => {
            parser.bump(T![throw]);
            if expression(parser).is_none() {
                parser.error("expected an expression after `throw`");
            }
        }
        _ => {
            m.abandon(parser);
            return None;
        }
    }
    Some(m.complete(parser, JUMP_EXPRESSION))
}

fn callable_reference(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    let recv = receiver_type(parser, RecvType::NotDotted(UserType::All));
    if !parser.eat(T![::]) {
        m.abandon(parser);
        return recv;
    }
    if parser.at(T![class]) {
        parser.bump(T![class]);
    } else if simple_identifier(parser).is_some() {
    } else {
        parser.error("expected an identifier or `class`");
    }
    Some(m.complete(parser, CALLABLE_REFERENCE))
}

define_operator!(assignment_and_operator, T![+=] | T![-=] | T![/=] | T![*=] | T![%=]);
define_operator!(equality_operator, T![==] | T![!=] | T![===] | T![!==]);
define_operator!(comparison_operator, T![<] | T![>] | T![<=] | T![>=]);
define_operator!(in_operator, T![in] | T![!in]);
define_operator!(is_operator, T![is] | T![!is]);
define_operator!(additive_operator, T![+] | T![-]);
define_operator!(multiplicative_operator, T![*] | T![/] | T![%]);
define_operator!(as_operator, T![as] | T![as?]);

mod prefix_unary_operator {
    use super::*;
    pub(crate) fn parse(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
        if is(parser) {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, PREFIX_UNARY_OPERATOR))
        } else {
            None
        }
    }
    pub(crate) fn is(parser: &mut Parser<'_>) -> bool {
        matches!(parser.current(), T![++] | T![--] | T![+] | T![-] | T![!])
    }
}
mod postfix_unary_operator {
    use super::*;
    pub(crate) fn parse(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
        if is(parser) {
            let m = parser.start();
            if parser.eat(T![!]) {
                excl(parser);
            } else {
                parser.bump_any();
            }
            Some(m.complete(parser, POSTFIX_UNARY_OPERATOR))
        } else {
            None
        }
    }
    pub(crate) fn is(parser: &mut Parser<'_>) -> bool {
        matches!((parser.current(), parser.nth(1)), (T![++] | T![--], _) | (T![!], T![!]))
    }
}

pub(crate) mod range_operator {
    use super::*;
    pub(crate) fn parse(parser: &mut Parser<'_>) -> bool {
        if is(parser) {
            parser.bump_any();
            true
        } else {
            false
        }
    }
    pub(crate) fn is(parser: &mut Parser<'_>) -> bool {
        parser.at(T![..]) || parser.at(T![..<])
    }
}

fn excl(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![!]) {
        let m = parser.start();
        parser.bump(T![!]);
        Some(m.complete(parser, EXCL))
    } else {
        None
    }
}

fn member_access_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    if safe_nav(parser).is_none() {
        if parser.at(T![.]) || parser.at(T![::]) {
            parser.bump_any();
        } else {
            m.abandon(parser);
            return None;
        }
    }
    Some(m.complete(parser, MEMBER_ACCESS_OPERATOR))
}

fn safe_nav(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![?]) && parser.nth_at(1, T![.]) {
        let m = parser.start();
        parser.bump(T![?]);
        parser.bump(T![.]);
        Some(m.complete(parser, SAFE_NAV))
    } else {
        None
    }
}
