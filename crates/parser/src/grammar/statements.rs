use crate::SyntaxKind::*;
use crate::T;

use super::annotations::annotation;
use super::class_members::{multi_variable_declaration, variable_declaration};
use super::expressions::expression;
use super::general::declaration;
use super::identifiers::{is_simple_identifier, simple_identifier};
use crate::{CompletedMarker, Parser};

pub(crate) fn semi(parser: &mut Parser<'_>) -> bool {
    if parser.at(T![;]) {
        parser.eat(T![;]);
        true
    } else {
        parser.has_nl_before()
    }
}

pub(crate) fn semis(parser: &mut Parser<'_>) -> bool {
    let mut found = false;
    while parser.eat(T![;]) {
        found = true;
    }
    if found { true } else { parser.has_nl_before() }
}

pub(super) fn statements(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(first) = statement(parser) {
        let m = first.precede(parser);
        while statement(parser).is_some() && semis(parser) {}
        Some(m.complete(parser, STATEMENTS))
    } else {
        None
    }
}

pub(super) fn statement(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.has_dangling_parens() {
        return assignment::assignment_or_expression(parser);
    }

    let m = parser.start_with_annotation();
    let mut has_label = false;

    while label(parser)
        .inspect(|_| {
            has_label = true;
        })
        .or_else(|| annotation(parser))
        .is_some()
    {}

    if has_label {
        m.forget(parser);
    } else {
        // This is hack to let the downstream parsers to pick up the annotations.
        // We need to capture the annotations as dangling if there is no label, otherwise we will not be able to attach them to the statement.
        m.abandon(parser);
    }

    if let Some(cm) = loop_statement(parser) {
        Some(cm.precede(parser).complete(parser, STATEMENT))
    } else {
        match declaration(parser, true) {
            // NB: we are not capturing declarations as statements
            Some(cm) => Some(cm),
            None => assignment::assignment_or_expression(parser)
                .map(|cm| cm.precede(parser).complete(parser, STATEMENT)),
        }
    }
}

fn loop_statement(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    for_statement(parser).or_else(|| while_statement(parser)).or_else(|| do_while_statement(parser))
}

fn for_statement(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![for]) {
        let m = parser.start();
        parser.eat(T![for]);

        if !parser.eat(T!['(']) {
            parser.error("expected '('");
            return Some(m.complete(parser, FOR_STATEMENT));
        }

        while annotation(parser).is_some() {}
        let mut has_variable_declaration = true;
        if variable_declaration(parser).or_else(|| multi_variable_declaration(parser)).is_none() {
            has_variable_declaration = false;
            parser.error("expected variable declaration");
        }

        if has_variable_declaration && !parser.eat(T![in]) {
            parser.error("expected `in`");
        } else {
            if expression(parser).is_none() {
                parser.error("expected expression");
            }
        }

        if !parser.eat(T![')']) {
            parser.error("expected ')'");
            return Some(m.complete(parser, FOR_STATEMENT));
        }

        control_structure_body(parser);

        Some(m.complete(parser, FOR_STATEMENT))
    } else {
        None
    }
}

fn while_statement(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![while]) {
        let m = parser.start();
        parser.eat(T![while]);

        if !parser.eat(T!['(']) {
            parser.error("expected '('");
            return Some(m.complete(parser, WHILE_STATEMENT));
        }

        expression(parser);

        if !parser.eat(T![')']) {
            parser.error("expected ')'");
            return Some(m.complete(parser, WHILE_STATEMENT));
        }

        if control_structure_body(parser)
            .map(|_| ())
            .or_else(|| parser.eat(T![;]).then_some(()))
            .is_none()
        {
            parser.error("expected `{...}` or `;`");
        }

        Some(m.complete(parser, WHILE_STATEMENT))
    } else {
        None
    }
}

fn do_while_statement(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![do]) {
        let m = parser.start();
        parser.eat(T![do]);

        control_structure_body(parser);

        if !parser.eat(T![while]) {
            parser.error("expected 'while'");
            return Some(m.complete(parser, DO_WHILE_STATEMENT));
        }

        if !parser.eat(T!['(']) {
            parser.error("expected '('");
            return Some(m.complete(parser, DO_WHILE_STATEMENT));
        }

        expression(parser);

        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }

        Some(m.complete(parser, DO_WHILE_STATEMENT))
    } else {
        None
    }
}

pub(crate) fn label(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_simple_identifier(parser) && parser.nth_at(1, T![@]) {
        let m = parser.start();
        simple_identifier(parser);
        parser.bump_any();
        Some(m.complete(parser, LABEL))
    } else {
        None
    }
}

pub(crate) fn control_structure_body(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    block(parser).or_else(|| statements(parser))
}

pub(crate) fn single_stmt_control_structure_body(
    parser: &mut Parser<'_>,
) -> Option<CompletedMarker> {
    // HKGIC: This is supposed to be control_structure_body(parser);
    // but it would parse multiple statements, which is not allowed in a when entry.
    block(parser)
        .or_else(|| statement(parser).map(|cm| cm.precede(parser).complete(parser, STATEMENTS)))
}

pub(crate) fn block(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T!['{']) {
        let m = parser.start();
        parser.eat(T!['{']);

        statements(parser);

        if !parser.eat(T!['}']) {
            parser.error("expected '}'");
        }
        Some(m.complete(parser, BLOCK))
    } else {
        None
    }
}

// REFACTOR: use dangling strategy
mod assignment {
    use crate::grammar::expressions::{
        AffixedExpression, Expression, assignable_suffix, assignment_and_operator,
    };

    use super::*;

    enum AssignmentFragment {
        DirectlyAssignableExpression(CompletedMarker),
        AssignableExpression(CompletedMarker),
        UnAssignable(CompletedMarker), // normal expression
    }

    impl AssignmentFragment {
        fn marker(self) -> CompletedMarker {
            match self {
                AssignmentFragment::DirectlyAssignableExpression(m)
                | AssignmentFragment::AssignableExpression(m)
                | AssignmentFragment::UnAssignable(m) => m,
            }
        }
    }

    pub(super) fn assignment_or_expression(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
        let left = entry(parser)?;

        match left {
            AssignmentFragment::DirectlyAssignableExpression(cm) => {
                let m = cm.precede(parser);
                if !parser.eat(T![=]) {
                    parser.error("expected '='");
                } else if expression(parser).is_none() {
                    parser.error("expected an expression");
                }
                Some(m.complete(parser, ASSIGNMENT))
            }
            AssignmentFragment::AssignableExpression(cm) => {
                let m = cm.precede(parser);
                assignment_and_operator::parse(parser);
                if expression(parser).is_none() {
                    parser.error("expected an expression");
                }
                Some(m.complete(parser, ASSIGNMENT))
            }
            AssignmentFragment::UnAssignable(cm) => Some(cm),
        }
    }

    fn entry(p: &mut Parser<'_>) -> Option<AssignmentFragment> {
        if p.at(T!['(']) || p.has_dangling_parens() {
            return parenthesized(p);
        }

        let expr = expression(p)?;

        match expr {
            // The interesting thing is that postfix expr can also be prefix expr
            Expression::Affixed(AffixedExpression::Prefix(cm) | AffixedExpression::Postfix(cm))
                if assignment_and_operator::is(p) =>
            {
                Some(AssignmentFragment::AssignableExpression(cm))
            }
            Expression::Affixed(AffixedExpression::Postfix(cm))
                if assignable_suffix(p).is_some() || p.at(T![=]) =>
            {
                Some(AssignmentFragment::DirectlyAssignableExpression(
                    cm.precede(p).complete(p, DIRECTLY_ASSIGNABLE_EXPRESSION),
                ))
            }
            e => Some(AssignmentFragment::UnAssignable(e.marker())),
        }
    }

    fn parenthesized(p: &mut Parser<'_>) -> Option<AssignmentFragment> {
        if p.at(T!['(']) || p.has_dangling_parens() {
            let m = p.start_with_paren();

            let Some(frag) = entry(p) else {
                m.abandon(p);
                return None;
            };

            if !p.eat(T![')']) {
                p.error("expected ')'");
            }

            match frag {
                AssignmentFragment::DirectlyAssignableExpression(_) => {
                    Some(AssignmentFragment::DirectlyAssignableExpression(
                        m.complete(p, PARENTHESIZED_DIRECTLY_ASSIGNABLE_EXPRESSION),
                    ))
                }
                AssignmentFragment::AssignableExpression(_) => {
                    Some(AssignmentFragment::AssignableExpression(
                        m.complete(p, PARENTHESIZED_ASSIGNABLE_EXPRESSION),
                    ))
                }
                AssignmentFragment::UnAssignable(_) => Some(AssignmentFragment::UnAssignable(
                    m.complete(p, PARENTHESIZED_EXPRESSION).precede(p).complete(p, EXPRESSION),
                )),
            }
        } else {
            None
        }
    }
}
