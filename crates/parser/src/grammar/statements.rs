use syntax::{SyntaxKind::*, T};

use super::annotations::annotation;
use super::class_members::{multi_variable_declaration, variable_declaration};
use super::expressions::expression;
use super::general::declaration;
use super::identifiers::{is_simple_identifier, simple_identifier};
use super::modifiers::modifiers;
use crate::ra::{CompletedMarker, Parser};

pub(crate) fn semi(parser: &mut Parser<'_>) -> bool {
    if parser.at(T![;]) {
        let m = parser.start();
        parser.eat(T![;]);
        m.complete(parser, SEMI);
        true
    } else {
        parser.has_ws_before() // TODO: check for newlines but not for other whitespace
    }
}

pub(crate) fn semis(parser: &mut Parser<'_>) -> bool {
    let m = parser.start();
    let mut found = false;
    while parser.eat(T![;]) {
        found = true;
    }
    if found {
        m.complete(parser, SEMIS);
        true
    } else {
        m.abandon(parser);
        parser.has_ws_before() // TODO: check for newlines but not for other whitespace
    }
}

pub(crate) fn statements(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(first) = statement(parser) {
        let m = first.precede(parser);
        loop {
            if !semis(parser) {
                break;
            }
            if statement(parser).is_none() {
                break;
            }
        }
        semis(parser);
        Some(m.complete(parser, STATEMENTS))
    } else {
        None
    }
}

pub(crate) fn statement(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    while label(parser).or_else(|| annotation(parser)).is_some() {}

    if loop_statement(parser)
        .or_else(|| {
            let modifiers = modifiers(parser);
            declaration(parser, modifiers)
        })
        .or_else(|| assignment::assignment_or_expression(parser))
        .is_none()
    {
        m.abandon(parser);
        None
    } else {
        Some(m.complete(parser, STATEMENT))
    }
}

fn loop_statement(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    for_statement(parser)
        .or_else(|| while_statement(parser))
        .or_else(|| do_while_statement(parser))
        .map(|cm| cm.precede(parser).complete(parser, LOOP_STATEMENT))
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
        if variable_declaration(parser)
            .or_else(|| multi_variable_declaration(parser))
            .is_none()
        {
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
    if is_simple_identifier(parser) && (parser.nth_at(1, T![@]) || parser.nth_at(1, AT_POST_WS)) {
        let m = parser.start();
        simple_identifier(parser);
        parser.bump_any(); // either T![@] or AT_POST_WS
        Some(m.complete(parser, LABEL))
    } else {
        None
    }
}

pub(crate) fn control_structure_body(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    block(parser)
        .or_else(|| statements(parser))
        .map(|cm| cm.precede(parser).complete(parser, CONTROL_STRUCTURE_BODY))
}

pub(crate) fn block(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T!['{']) {
        let m = parser.start();
        parser.eat(T!['{']);

        while statements(parser).is_some() {}

        if !parser.eat(T!['}']) {
            parser.error("expected '}'");
        }
        Some(m.complete(parser, BLOCK))
    } else {
        None
    }
}

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
        let m = parser.start();
        let Some(left) = entry(parser) else {
            m.abandon(parser);
            return None;
        };

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
        if p.at(T!['(']) {
            return parenthesized(p);
        }
        let m = p.start();
        let Some(expr) = expression(p) else {
            m.abandon(p);
            return None;
        };

        match expr {
            // The interesting thing is that postfix expr can also be prefic expr
            Expression::Affixed(AffixedExpression::Prefix(_) | AffixedExpression::Postfix(_))
                if assignment_and_operator::is(p) =>
            {
                Some(AssignmentFragment::AssignableExpression(
                    m.complete(p, ASSIGNABLE_EXPRESSION),
                ))
            }
            Expression::Affixed(AffixedExpression::Postfix(_))
                if assignable_suffix(p).is_some() || p.at(T![=]) =>
            {
                Some(AssignmentFragment::DirectlyAssignableExpression(
                    m.complete(p, DIRECTLY_ASSIGNABLE_EXPRESSION),
                ))
            }
            e => {
                m.abandon(p);
                Some(AssignmentFragment::UnAssignable(e.marker()))
            }
        }
    }

    fn parenthesized(p: &mut Parser<'_>) -> Option<AssignmentFragment> {
        if p.at(T!['(']) {
            let m = p.start();
            p.eat(T!['(']);

            let Some(frag) = entry(p) else {
                p.error("expected an expression");
                p.eat(T![')']); // try to eat the closing paren to avoid cascading errors
                m.abandon(p);
                return None;
            };

            if !p.eat(T![')']) {
                p.error("expected ')'");
            }

            match frag {
                AssignmentFragment::DirectlyAssignableExpression(cm) => {
                    Some(AssignmentFragment::DirectlyAssignableExpression(
                        cm.precede(p)
                            .complete(p, PARENTHESIZED_DIRECTLY_ASSIGNABLE_EXPRESSION),
                    ))
                }
                AssignmentFragment::AssignableExpression(cm) => {
                    Some(AssignmentFragment::AssignableExpression(
                        cm.precede(p)
                            .complete(p, PARENTHESIZED_ASSIGNABLE_EXPRESSION),
                    ))
                }
                AssignmentFragment::UnAssignable(cm) => Some(AssignmentFragment::UnAssignable(
                    cm.precede(p)
                        .complete(p, PARENTHESIZED_EXPRESSION)
                        .precede(p)
                        .complete(p, EXPRESSION),
                )),
            }
        } else {
            None
        }
    }
}
