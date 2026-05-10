use syntax::{SyntaxKind::*, T};

use super::annotations::annotation;
use super::class_members::{
    function_body, multi_variable_declaration, parameters_with_opt_type, variable_declaration,
};
use super::classes::{class_body, delegation_specifiers, type_constraints};
use super::identifiers::{is_simple_identifier, simple_identifier};
use super::statements::{block, control_structure_body, label, semi, statements};
use super::types::{RecvType, receiver_type, ty, type_projection};
use crate::ra::{CompletedMarker, Parser};

pub(crate) fn expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    disjunction(parser).map(|cm| cm.precede(parser).complete(parser, EXPRESSION))
}

fn disjunction(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = conjunction(parser) {
        let m = cm.precede(parser);
        while parser.eat(T![||]) {
            if conjunction(parser).is_none() {
                parser.error("expected an expression");
            } else {
                break;
            }
        }
        Some(m.complete(parser, DISJUNCTION))
    } else {
        None
    }
}

fn conjunction(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = equality(parser) {
        let m = cm.precede(parser);
        while parser.eat(T![&&]) {
            if equality(parser).is_none() {
                parser.error("expected an expression");
            } else {
                break;
            }
        }
        Some(m.complete(parser, CONJUNCTION))
    } else {
        None
    }
}

fn equality(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = comparison(parser) {
        let m = cm.precede(parser);
        while equality_operator(parser).is_some() {
            if comparison(parser).is_none() {
                parser.error("expected an expression");
            } else {
                break;
            }
        }
        Some(m.complete(parser, EQUALITY))
    } else {
        None
    }
}

fn comparison(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = generic_call_like_comparison(parser) {
        let m = cm.precede(parser);
        while comparison_operator(parser).is_some() {
            if generic_call_like_comparison(parser).is_none() {
                parser.error("expected an expression");
            } else {
                break;
            }
        }
        Some(m.complete(parser, COMPARISON))
    } else {
        None
    }
}

fn generic_call_like_comparison(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = infix_operation(parser) {
        let m = cm.precede(parser);
        while call_suffix(parser, CallSuffix::Full).is_some() {}
        Some(m.complete(parser, GENERIC_CALL_LIKE_COMPARISON))
    } else {
        None
    }
}

fn infix_operation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = elvis_expression(parser) {
        let m = cm.precede(parser);
        while is_or_in_operator_expr(parser) {}
        Some(m.complete(parser, INFIX_OPERATION))
    } else {
        None
    }
}

fn is_or_in_operator_expr(parser: &mut Parser<'_>) -> bool {
    if is_operator(parser).is_some() {
        if ty(parser).is_none() {
            parser.error("expected a type");
        } else {
            return true;
        }
    } else if in_operator(parser).is_some() {
        if elvis_expression(parser).is_none() {
            parser.error("expected an expression");
        } else {
            return true;
        }
    }
    false
}

fn elvis_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = infix_function_call(parser) {
        let m = cm.precede(parser);
        while elvis(parser).is_some() {
            if infix_function_call(parser).is_none() {
                parser.error("expected an expression");
            } else {
                break;
            }
        }
        Some(m.complete(parser, ELVIS_EXPRESSION))
    } else {
        None
    }
}

fn elvis(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![?]) && !parser.nth_at(1, T![:]) {
        let m = parser.start();
        parser.bump(T![?]);
        parser.bump(T![:]);
        Some(m.complete(parser, ELVIS))
    } else {
        None
    }
}

fn infix_function_call(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = range_expression(parser) {
        let m = cm.precede(parser);
        while simple_identifier(parser).is_some() {
            if range_expression(parser).is_none() {
                parser.error("expected an expression");
            } else {
                break;
            }
        }
        Some(m.complete(parser, INFIX_FUNCTION_CALL))
    } else {
        None
    }
}

fn range_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = additive_expression(parser) {
        let m = cm.precede(parser);
        while parser.eat(T![..]) || parser.eat(T![..<]) {
            if additive_expression(parser).is_none() {
                parser.error("expected an expression");
            } else {
                break;
            }
        }
        Some(m.complete(parser, RANGE_EXPRESSION))
    } else {
        None
    }
}

fn additive_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = multiplicative_expression(parser) {
        let m = cm.precede(parser);
        while additive_operator(parser).is_some() {
            if multiplicative_expression(parser).is_none() {
                parser.error("expected an expression");
            } else {
                break;
            }
        }
        Some(m.complete(parser, ADDITIVE_EXPRESSION))
    } else {
        None
    }
}

fn multiplicative_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = as_expression(parser) {
        let m = cm.precede(parser);
        while multiplicative_operator(parser).is_some() {
            if as_expression(parser).is_none() {
                parser.error("expected an expression");
            } else {
                break;
            }
        }
        Some(m.complete(parser, MULTIPLICATIVE_EXPRESSION))
    } else {
        None
    }
}

fn as_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = prefix_unary_expression(parser).map(|s| s.marker()) {
        let m = cm.precede(parser);
        while as_operator(parser).is_some() {
            if ty(parser).is_none() {
                parser.error("expected a type");
            } else {
                break;
            }
        }
        Some(m.complete(parser, AS_EXPRESSION))
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
        parser.error("expected an expression");
    }
    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }
    Some(m.complete(parser, PARENTHESIZED_DIRECTLY_ASSIGNABLE_EXPRESSION))
}

fn directly_assignable_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    postfix_unary_expression(parser)
        .map(|cm| {
            let m = cm.precede(parser);
            assignable_suffix(parser); // record error if none
            m.complete(parser, DIRECTLY_ASSIGNABLE_EXPRESSION)
        })
        .or_else(|| {
            simple_identifier(parser)
                .or_else(|| parenthesized_directly_assignable_expression(parser))
                .map(|cm| {
                    cm.precede(parser)
                        .complete(parser, DIRECTLY_ASSIGNABLE_EXPRESSION)
                })
        })
}

fn parenthesized_assignable_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['(']);
    if assignable_expression(parser).is_none() {
        parser.error("expected an expression");
    }
    if !parser.eat(T![')']) {
        parser.error("expected `)`");
    }
    Some(m.complete(parser, PARENTHESIZED_ASSIGNABLE_EXPRESSION))
}

fn assignable_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    parenthesized_assignable_expression(parser)
        .or_else(|| prefix_unary_expression(parser).map(|pue| pue.marker()))
        .map(|cm| cm.precede(parser).complete(parser, ASSIGNABLE_EXPRESSION))
}

enum PrefixUnaryExpression {
    Prefix(CompletedMarker),
    Postfix(CompletedMarker),
}

impl PrefixUnaryExpression {
    fn marker(self) -> CompletedMarker {
        match self {
            PrefixUnaryExpression::Prefix(cm) | PrefixUnaryExpression::Postfix(cm) => cm,
        }
    }
}

fn prefix_unary_expression(parser: &mut Parser<'_>) -> Option<PrefixUnaryExpression> {
    let m = parser.start();
    let mut has_prefix = false;

    while unary_prefix(parser).is_some() {
        has_prefix = true;
    }
    let postfix = postfix_unary_expression(parser);
    let has_postfix = postfix.is_some();

    if has_postfix {
        if has_prefix {
            Some(PrefixUnaryExpression::Prefix(m.complete(parser, PREFIX_UNARY_EXPRESSION)))
        } else {
           postfix.map(PrefixUnaryExpression::Postfix)
        }
    } else {
        m.abandon(parser);
        None
    }
}

fn unary_prefix(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    annotation(parser)
        .or_else(|| prefix_unary_operator(parser))
        .or_else(|| label(parser))
        .map(|cm| cm.precede(parser).complete(parser, UNARY_PREFIX))
}

fn postfix_unary_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    primary_expression(parser).map(|cm| {
        let m = cm.marker().precede(parser);
        while postfix_unary_suffix(parser).is_some() {}
        m.complete(parser, POSTFIX_UNARY_EXPRESSION)
    })
}

fn postfix_unary_suffix(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    call_suffix(parser, CallSuffix::AcceptTypeArguments)
        .or_else(|| postfix_unary_operator(parser))
        .or_else(|| indexing_suffix(parser))
        .or_else(|| navigation_suffix(parser))
        .map(|cm| cm.precede(parser).complete(parser, POSTFIX_UNARY_SUFFIX))
}

fn assignable_suffix(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    indexing_suffix(parser)
        .or_else(|| navigation_suffix(parser))
        .or_else(|| type_arguments(parser))
        .map(|cm| cm.precede(parser).complete(parser, ASSIGNABLE_SUFFIX))
}

fn indexing_suffix(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['[']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['[']);

    if expression(parser).is_none() {
        parser.error("expected an expression");
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
        if simple_identifier(parser)
            .or_else(|| parenthesized_expression(parser))
            .is_none()
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
    Full
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
    annotation(parser);
    if simple_identifier(parser).is_some() && !parser.eat(T![=]) {
        parser.error("expected `=`");
    }
    parser.eat(T![*]);

    if expression(parser).is_none() {
        parser.error("expected an expression");
    }
    Some(m.complete(parser, VALUE_ARGUMENT))
}

enum PrimaryExpression {
        Parenthesized(CompletedMarker),
        CollectionLiteral(CompletedMarker),
        LiteralConstant(CompletedMarker),
        StringLiteral(CompletedMarker),
        FunctionLiteral(CompletedMarker),
        ObjectLiteral(CompletedMarker),
        ThisExpression(CompletedMarker),
        SuperExpression(CompletedMarker),
        IfExpression(CompletedMarker),
        WhenExpression(CompletedMarker),
        TryExpression(CompletedMarker),
        JumpExpression(CompletedMarker),
        SimpleIdentifier(CompletedMarker),
        CallableReference(CompletedMarker)
}

impl PrimaryExpression {
    fn marker(self) -> CompletedMarker {
        match self {
            PrimaryExpression::Parenthesized(cm)
            | PrimaryExpression::CollectionLiteral(cm)
            | PrimaryExpression::LiteralConstant(cm)
            | PrimaryExpression::StringLiteral(cm)
            | PrimaryExpression::FunctionLiteral(cm)
            | PrimaryExpression::ObjectLiteral(cm)
            | PrimaryExpression::ThisExpression(cm)
            | PrimaryExpression::SuperExpression(cm)
            | PrimaryExpression::IfExpression(cm)
            | PrimaryExpression::WhenExpression(cm)
            | PrimaryExpression::TryExpression(cm)
            | PrimaryExpression::JumpExpression(cm)
            | PrimaryExpression::SimpleIdentifier(cm)
            | PrimaryExpression::CallableReference(cm) => cm,
        }
    }
}

fn primary_expression(parser: &mut Parser<'_>) -> Option<PrimaryExpression> {
    use PrimaryExpression::*;
    parenthesized_expression(parser).map(|cm| Parenthesized(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION)))
        .or_else(|| collection_literal(parser).map(|cm| CollectionLiteral(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| literal_constant(parser).map(|cm| LiteralConstant(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| string_literal(parser).map(|cm| StringLiteral(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| function_literal(parser).map(|cm| FunctionLiteral(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| object_literal(parser).map(|cm| ObjectLiteral(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| this_expresssion(parser).map(|cm| ThisExpression(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| super_expression(parser).map(|cm| SuperExpression(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| if_expression(parser).map(|cm| IfExpression(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| when_expression(parser).map(|cm| WhenExpression(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| try_expression(parser).map(|cm| TryExpression(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| jump_expression(parser).map(|cm| JumpExpression(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
        .or_else(|| {
            if is_simple_identifier(parser) && !parser.nth_at(1, T![::]) {
                simple_identifier(parser).map(|cm| SimpleIdentifier(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION)))
            } else {
                None
            }
        })
        .or_else(|| callable_reference(parser).map(|cm| CallableReference(cm.precede(parser).complete(parser, PRIMARY_EXPRESSION))))
}

fn parenthesized_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(T!['(']) {
        return None;
    }
    let m = parser.start();
    parser.bump(T!['(']);
    if expression(parser).is_none() {
        parser.error("expected an expression");
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
        BOOLEAN_LITERAL | INTEGER_LITERAL | HEX_LITERAL | BIN_LITERAL | CHARACTER_LITERAL
        | REAL_LITERAL | NULL_LITERAL | LONG_LITERAL | UNSIGNED_LITERAL => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, LITERAL_CONSTANT))
        }
        _ => None,
    }
}

fn string_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    line_string_literal(parser)
        .or_else(|| multi_line_string_literal(parser))
        .map(|cm| cm.precede(parser).complete(parser, STRING_LITERAL))
}

fn line_string_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(QUOTE_OPEN) {
        return None;
    }
    let m = parser.start();
    parser.bump(QUOTE_OPEN);
    while line_string_content(parser).is_some() || line_string_expr(parser).is_some() {}
    if !parser.eat(QUOTE_CLOSE) {
        parser.error(r#"expected `"`"#);
    }
    Some(m.complete(parser, LINE_STRING_LITERAL))
}

fn multi_line_string_literal(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !parser.at(TRIPLE_QUOTE_OPEN) {
        return None;
    }
    let m = parser.start();
    parser.bump(TRIPLE_QUOTE_OPEN);
    while multi_line_string_content(parser).is_some()
        || multi_line_string_expr(parser).is_some()
        || parser.eat(MULTI_LINE_STRING_QUOTE)
    {}
    if !parser.eat(TRIPLE_QUOTE_CLOSE) {
        parser.error(r#"expected `"""`"#);
    }
    Some(m.complete(parser, MULTI_LINE_STRING_LITERAL))
}

fn line_string_content(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        LINE_STR_TEXT | LINE_STR_ESCAPED_CHAR | LINE_STR_REF => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, LINE_STRING_CONTENT))
        }
        _ => None,
    }
}

fn line_string_expr(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(LINE_STR_EXPR_START) {
        let m = parser.start();
        parser.bump(LINE_STR_EXPR_START);
        if expression(parser).is_none() {
            parser.error("expected an expression");
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
        MULTI_LINE_STR_TEXT | MULTI_LINE_STRING_QUOTE | MULTI_LINE_STR_REF => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, MULTI_LINE_STRING_CONTENT))
        }
        _ => None,
    }
}

fn multi_line_string_expr(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(MULTI_STR_EXPR_START) {
        let m = parser.start();
        parser.bump(MULTI_STR_EXPR_START);
        if expression(parser).is_none() {
            parser.error("expected an expression");
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
    lambda_literal(parser)
        .or_else(|| anonymous_function(parser))
        .map(|cm| cm.precede(parser).complete(parser, FUNCTION_LITERAL))
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
    statements(parser);
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
    if !(parser.at(T![fun]) || (parser.at(T![suspend]) && parser.nth_at(1, T![fun]))) {
        return None;
    }
    let m = parser.start();

    parser.eat(T![suspend]);
    parser.bump(T![fun]);

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
        parser.error("expected an expression");
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
        parser.error("expected an expression");
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
        .or_else(|| expression(parser))
        .map(|cm| cm.precede(parser).complete(parser, WHEN_CONDITION))
}

fn range_test(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = in_operator(parser) {
        let m = cm.precede(parser);
        if expression(parser).is_none() {
            parser.error("expected an expression");
        }
        Some(m.complete(parser, RANGE_TEST))
    } else {
        None
    }
}

fn type_test(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = is_operator(parser) {
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
    let recv = receiver_type(parser, RecvType::NotDotted);
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

fn assignment_and_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![+=] | T![-=] | T![/=] | T![*=] | T![%=] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, ASSIGNMENT_AND_OPERATOR))
        }
        _ => None,
    }
}

fn equality_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![==] | T![!=] | T![===] | T![!==] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, EQUALITY_OPERATOR))
        }
        _ => None,
    }
}

fn comparison_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![<] | T![>] | T![<=] | T![>=] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, COMPARISON_OPERATOR))
        }
        _ => None,
    }
}

fn in_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![in] | T![!in] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, IN_OPERATOR))
        }
        _ => None,
    }
}

fn is_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![is] | T![!is] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, IS_OPERATOR))
        }
        _ => None,
    }
}

fn additive_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![+] | T![-] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, ADDITIVE_OPERATOR))
        }
        _ => None,
    }
}

fn multiplicative_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    match parser.current() {
        T![*] | T![/] | T![%] => {
            let m = parser.start();
            parser.bump_any();
            Some(m.complete(parser, MULTIPLICATIVE_OPERATOR))
        }
        _ => None,
    }
}

fn as_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![as]) || parser.at(T![as?]) {
        let m = parser.start();
        parser.bump_any();
        Some(m.complete(parser, AS_OPERATOR))
    } else {
        None
    }
}

fn prefix_unary_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    match parser.current() {
        T![++] | T![--] | T![+] | T![-] => {
            parser.bump_any();
        }
        _ if safe_nav(parser).is_some() => {}
        _ => {
            m.abandon(parser);
            return None;
        }
    }
    Some(m.complete(parser, PREFIX_UNARY_OPERATOR))
}

fn postfix_unary_operator(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    match parser.current() {
        T![++] | T![--] => {
            parser.bump_any();
        }
        T![!] => {
            if excl(parser).is_none() {
                parser.error("missing `!`, non-null assertion requires two `!`");
            }
        }
        _ => {
            m.abandon(parser);
            return None;
        }
    }
    Some(m.complete(parser, POSTFIX_UNARY_OPERATOR))
}

fn excl(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![!]) || parser.at(EXCL_WS) {
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
