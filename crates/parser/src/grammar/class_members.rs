use crate::{DanglingMarker, SyntaxKind::*, T};

use super::classes::{class_body, delegation_specifiers, type_constraints};
use super::general::declaration;
use super::identifiers::{is_simple_ident_at, is_simple_identifier};
use super::statements::{block, semi, semis};
use super::types::{RecvType, UserType, receiver_type, user_type};
use crate::{CompletedMarker, Parser, TokenSet};

use super::classes::type_parameters;
use super::expressions::{expression, value_arguments};
use super::identifiers::simple_identifier;
use super::modifiers::parameter_modifiers;
use super::types::ty;

pub(crate) fn class_member_declarations(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = class_member_declaration(parser) {
        let m = cm.precede(parser);
        semis(parser);

        while class_member_declaration(parser).is_some() {
            semis(parser);
        }

        Some(m.complete(parser, CLASS_MEMBER_DECLARATIONS))
    } else {
        None
    }
}

fn class_member_declaration(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = anonymous_initializer(parser) {
        return Some(cm);
    }

    companion_object(parser)
        .or_else(|| secondary_constructor(parser))
        .or_else(|| declaration(parser, false))
}

fn anonymous_initializer(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![init]) {
        let m = parser.start();
        parser.eat(T![init]);
        block(parser);
        Some(m.complete(parser, ANONYMOUS_INITIALIZER))
    } else {
        None
    }
}

fn companion_object(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();

    if !parser.eat(T![companion]) {
        m.abandon(parser);
        return None;
    }

    parser.eat(T![data]);

    if !parser.eat(T![object]) {
        parser.error("expected 'object' keyword");
    }

    simple_identifier(parser);

    if parser.eat(T![:]) && delegation_specifiers(parser).is_none() {
        parser.error("expected delegation specifiers");
    }
    class_body(parser, None);
    Some(m.complete(parser, COMPANION_OBJECT))
}

pub(crate) fn starts_fn_declaration(parser: &mut Parser<'_>) -> bool {
    parser.at(T![fun]) && !parser.nth_at(1, T![interface])
}

// test fn_declaration
// fun foo() {}
// fun foo(bar: Int) {}
// fun foo(bar: Int = 2, baz: Int, meh: String = "") {}
// fun <T> foo(@Anno bar: T) {}
// fun <T> foo(): Int {}
// fun <T> Receiver.foo(): Int {}
// fun <T> T.foo(bar: Int, baz: T): Int where T: Any, T: Serializable {}
// infix fun Int.shl(x: Int): Int
// fun <T> asList(vararg ts: T): List<T>
// fun double(x: Int): Int = x * 2
pub(super) fn function_declaration(
    parser: &mut Parser<'_>,
    allow_expressions: bool,
) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();

    if !starts_fn_declaration(parser) {
        m.abandon(parser);
        return None;
    }

    parser.bump(T![fun]);
    type_parameters(parser);
    // HKGIC: In the grammar, function name is before receiver type,
    // but we want to parse it before, if the function does not contain a receiver type, as function name is a valid receiver type
    if is_simple_identifier(parser) && parser.nth_at(1, T!['(']) {
        simple_identifier(parser);
    } else {
        receiver_type(parser, RecvType::Dotted(UserType::BeforeName));

        if simple_identifier(parser).is_none() {
            if allow_expressions {
                // this is an anonymous function expression. This will fallback to expression parser.
                m.map(|m| DanglingMarker::Func { marker: m, valid: true }).abandon(parser);
                return None;
            } else {
                parser.error("expected a name for function: #FN");
            }
        }
    }
    if function_value_parameters(parser).is_none() {
        parser.error("expected '('");
    }
    parameters_with_opt_type(parser);

    if parser.eat(T![:]) && ty(parser).is_none() {
        parser.error("expected a type");
    }
    type_constraints(parser);
    function_body(parser);
    Some(m.complete(parser, FUNCTION_DECLARATION))
}

pub(super) fn function_body(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = block(parser) {
        Some(cm.precede(parser).complete(parser, FUNCTION_BODY))
    } else {
        if parser.eat(T![=]) {
            if expression(parser).is_none() {
                parser.error("expected an expression");
            }
            Some(parser.start().complete(parser, FUNCTION_BODY))
        } else {
            None
        }
    }
}

// test object_declaration
// object Foo
// object Foo : Bar by baz
// object Foo {}
// object Foo : Something() {}
// data object Foo
// object Foo : Boo by Bae, Bar(), Baz, B.() -> Unit by A {}
pub(super) fn object_declaration(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();

    if !parser.at(T![object]) || !is_simple_ident_at(parser, 1) {
        m.abandon(parser);
        return None;
    }

    parser.bump(T![object]);
    simple_identifier(parser);
    if parser.eat(T![:]) && delegation_specifiers(parser).is_none() {
        parser.error("expected delegation specifiers");
    }
    class_body(parser, None);

    Some(m.complete(parser, OBJECT_DECLARATION))
}

// can also parse userType
fn constructor_invocation(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if let Some(cm) = user_type(parser, UserType::All) {
        let m = cm.precede(parser);
        if value_arguments(parser).is_some() {
            Some(m.complete(parser, CONSTRUCTOR_INVOCATION))
        } else {
            m.abandon(parser);
            None
        }
    } else {
        None
    }
}

fn property_delegate(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![by]) {
        let m = parser.start();
        parser.eat(T![by]);
        if expression(parser).is_none() {
            parser.error("expected an expression :#1");
        }
        Some(m.complete(parser, PROPERTY_DELEGATE))
    } else {
        None
    }
}

pub(super) const PROPERTY_DECLARATION_START: TokenSet = TokenSet::new(&[VAL_KW, VAR_KW]);

pub(super) fn multi_variable_declaration(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T!['(']) {
        let m = parser.start();
        parser.eat(T!['(']);
        if variable_declaration(parser).is_some() {
            while parser.eat(T![,]) && variable_declaration(parser).is_some() {}
        }
        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        Some(m.complete(parser, MULTI_VARIABLE_DECLARATION))
    } else {
        None
    }
}

pub(super) fn variable_declaration(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_annotation();
    // Optimization
    if is_simple_identifier(parser) && parser.nth_at(1, T!['(']) {
        m.abandon(parser);
        return None;
    }
    if simple_identifier(parser).is_some() {
        if parser.eat(T![:]) && ty(parser).is_none() {
            parser.error("expected a type");
        }
        Some(m.complete(parser, VARIABLE_DECLARATION))
    } else {
        m.forget(parser);
        None
    }
}

const AFTER_PROP_NAME: TokenSet =
    TokenSet::new(&[T![=], T![:], T![where], T![by], T![get], T![set]]);

/// Starts with either 'val' or 'var' keyword
// test property_declaration
// val x: Int
// val (@Anno x: A.B, @Deco x: T.() -> Unit) = listOf(a, b)
// val count by remember { mutableStateOf(0) }
// val <T> List<T>.lastIndex: Int
//    get() = this.size - 1
// val greet: String.() -> Unit = { }
// @Anno var <T> T.foo: T
//    get() = this
//    set(value) { this = value }
pub(super) fn property_declaration(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();

    if !parser.at_ts(PROPERTY_DECLARATION_START) {
        m.abandon(parser);
        return None;
    }

    parser.bump_any();
    type_parameters(parser);
    // HKGIC: In the grammar, property name is after receiver type, but we want to parse it before, as property name is a valid receiver type
    if is_simple_identifier(parser) && parser.nth_ats(1, AFTER_PROP_NAME) {
        multi_variable_declaration(parser).or_else(|| variable_declaration(parser));
    } else {
        if !parser.at(T!['(']) {
            // HKGIC: This is quite rare, and makes the grammar more complex, but can be fixed later
            // by parsing contents of (...) and deciding if it is a receiver type or
            // a multi variable declaration based on whether it contains a variable declaration or not.
            // Would look somewhat like this:
            // `val ((a: A) -> B).foo` or `val (Foo).baz = ...`
            // and also `val (arg: (a: A) -> B).foo`, where arg can be variable name or an argument until we see the dot,
            // but not `val (arg: Foo).baz = ...` as that would be grammatically incorrect.
            // NB that we cannot have `val (a: A) -> B.foo` as `.foo` would be ambiguous even though the grammar allows that.
            receiver_type(parser, RecvType::Dotted(UserType::BeforeName));
        }
        multi_variable_declaration(parser).or_else(|| variable_declaration(parser));
    }

    type_constraints(parser);
    if parser.eat(T![=]) {
        if expression(parser).is_none() {
            parser.error("expected an expression :#2");
        }
    } else {
        property_delegate(parser);
    }
    parser.eat(T![;]);

    let mut cm = m.complete(parser, PROPERTY_DECLARATION);

    if getter(parser).is_some() {
        semi(parser);

        cm = cm.extend_right(parser);

        if setter(parser).is_some() {
            return Some(cm.extend_right(parser));
        }
    } else if setter(parser).is_some() {
        semi(parser);

        cm = cm.extend_right(parser);

        if getter(parser).is_some() {
            return Some(cm.extend_right(parser));
        }
    }
    Some(cm)
}

fn getter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();
    if !parser.at(T![get]) {
        m.abandon(parser);
        return None;
    }

    parser.bump(T![get]);
    if parser.eat(T!['(']) {
        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        if parser.eat(T![:]) && ty(parser).is_none() {
            parser.error("expected a type");
        }
        if function_body(parser).is_none() {
            parser.error("expected a function body");
        }
    }
    Some(m.complete(parser, GETTER))
}

fn setter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();

    if !parser.at(T![set]) {
        m.abandon(parser);
        return None;
    }

    parser.bump(T![set]);
    if parser.eat(T!['(']) {
        if function_value_parameter_with_optional_type(parser).is_some() {
            parser.eat(T![,]);
        }
        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        if parser.eat(T![:]) && ty(parser).is_none() {
            parser.error("expected a type");
        }
        if function_body(parser).is_none() {
            parser.error("expected a function body");
        }
    }
    Some(m.complete(parser, SETTER))
}

pub(crate) fn context_parameter_list(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T![context]) && parser.nth_at(1, T!['(']) {
        let m = parser.start();
        parser.bump(T![context]);
        parser.bump(T!['(']);

        if function_value_parameter(parser).is_some() {
            while parser.eat(T![,]) && function_value_parameter(parser).is_some() {}
        } else {
            parser.error("expected a context parameter");
        }

        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        Some(m.complete(parser, CONTEXT_PARAMETER_LIST))
    } else {
        None
    }
}

fn function_value_parameters(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T!['(']) {
        let m = parser.start();
        parser.eat(T!['(']);

        if function_value_parameter(parser).is_some() {
            while parser.eat(T![,]) && function_value_parameter(parser).is_some() {}
        }

        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        Some(m.complete(parser, FUNCTION_VALUE_PARAMETERS))
    } else {
        None
    }
}
fn function_value_parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    let modifier = parameter_modifiers(parser);
    let param = parameter(parser);
    if modifier.is_none() && param.is_none() {
        m.abandon(parser);
        return None;
    }
    if param.is_none() {
        parser.error("expected a parameter");
    }

    if parser.eat(T![=]) && expression(parser).is_none() {
        parser.error("expected an expression :#3");
    }
    Some(m.complete(parser, FUNCTION_VALUE_PARAMETER))
}

fn function_value_parameter_with_optional_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start();
    parameter_modifiers(parser);
    if parameter_with_opt_type(parser).is_none() {
        parser.error("expected a parameter");
    }

    if parser.eat(T![=]) && expression(parser).is_none() {
        parser.error("expected an expression :#4");
    }
    Some(m.complete(parser, FUNCTION_VALUE_PARAMETER_WITH_OPTIONAL_TYPE))
}

fn secondary_constructor(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = parser.start_with_modifiers();

    if !parser.eat(T![constructor]) {
        m.abandon(parser);
        return None;
    }

    function_value_parameters(parser);
    if parser.eat(T![:]) {
        constructor_delegation_call(parser); // maybe record an error here if none
    }
    block(parser);
    Some(m.complete(parser, SECONDARY_CONSTRUCTOR))
}

pub(crate) fn parameter(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_simple_identifier(parser) && parser.nth_at(1, T![:]) {
        let m = parser.start();
        simple_identifier(parser);
        parser.eat(T![:]);
        if ty(parser).is_none() {
            parser.error("expected a type");
        }
        Some(m.complete(parser, PARAMETER))
    } else {
        None
    }
}

pub(crate) fn parameters_with_opt_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if parser.at(T!['(']) {
        let m = parser.start();
        parser.eat(T!['(']);

        if function_value_parameter_with_optional_type(parser).is_some() {
            while parser.eat(T![,]) && function_value_parameter_with_optional_type(parser).is_some()
            {
            }
        }

        if !parser.eat(T![')']) {
            parser.error("expected ')'");
        }
        Some(m.complete(parser, PARAMETERS_WITH_OPTIONAL_TYPE))
    } else {
        None
    }
}

fn parameter_with_opt_type(parser: &mut Parser<'_>) -> Option<CompletedMarker> {
    if is_simple_identifier(parser) {
        let m = parser.start();
        simple_identifier(parser);
        if parser.eat(T![:]) && ty(parser).is_none() {
            parser.error("expected a type");
        }
        Some(m.complete(parser, PARAMETER_WITH_OPTIONAL_TYPE))
    } else {
        None
    }
}

fn constructor_delegation_call(parser: &mut Parser<'_>) -> Option<()> {
    if parser.eat(T![this]) || parser.eat(T![super]) {
        if value_arguments(parser).is_none() {
            parser.error("expected an expression :#5");
        }
        Some(())
    } else {
        None
    }
}
