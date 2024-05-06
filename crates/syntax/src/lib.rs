use tokens::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
/// This represents only composite nodes and
/// tokens are wrapped inside [SyntaxKind::TOKEN]
/// Adapted from https://kotlinlang.org/spec/syntax-and-grammar.html#syntax-grammar
/// ! Note that the first item starts from 154, so that [tokens::Token] can be part of this
/// ! as [tokens::Token] starts from 0 and ends at 153. So we have to keep both in sync
pub enum Syntax {
    IDENTIFIER = 154,
    SIMPLE_IDENTIFIER,
    UNESCAPED_ANNOTATION,
    ANNOTATION_USE_SITE_TARGET,
    MULTI_ANNOTATION,
    SINGLE_ANNOTATION,
    ANNOTATION,
    PLATFORM_MODIFIER,
    REIFICATION_MODIFIER,
    PARAMETER_MODIFIER,
    INHERITANCE_MODIFIER,
    PROPERTY_MODIFIER,
    FUNCTION_MODIFIER,
    TYPE_PARAMETER_MODIFIER,
    TYPE_PARAMETER_MODIFIERS,
    VARIANCE_MODIFIER,
    VISIBILITY_MODIFIER,
    MEMBER_MODIFIER,
    CLASS_MODIFIER,
    TYPE_MODIFIER,
    TYPE_MODIFIERS,
    MODIFIER,
    PARAMETER_MODIFIERS,
    MODIFIERS,
    SAFE_NAV,
    MEMBER_ACCESS_OPERATOR,
    EXCL,
    POSTFIX_UNARY_OPERATOR,
    PREFIX_UNARY_OPERATOR,
    AS_OPERATOR,
    MULTIPLICATIVE_OPERATOR,
    ADDITIVE_OPERATOR,
    IS_OPERATOR,
    IN_OPERATOR,
    COMPARISON_OPERATOR,
    EQUALITY_OPERATOR,
    ASSIGNMENT_AND_OPERATOR,
    CALLABLE_REFERENCE,
    JUMP_EXPRESSION,
    FINALLY_BLOCK,
    CATCH_BLOCK,
    TRY_EXPRESSION,
    TYPE_TEST,
    RANGE_TEST,
    WHEN_CONDITION,
    WHEN_ENTRY,
    WHEN_EXPRESSION,
    WHEN_SUBJECT,
    IF_EXPRESSION,
    SUPER_EXPRESSION,
    THIS_EXPRESSION,
    OBJECT_LITERAL,
    FUNCTION_LITERAL,
    ANONYMOUS_FUNCTION,
    LAMBDA_PARAMETER,
    LAMBDA_PARAMETERS,
    LAMBDA_LITERAL,
    MULTI_LINE_STRING_EXPRESSION,
    MULTI_LINE_STRING_CONTENT,
    LINE_STRING_EXPRESSION,
    LINE_STRING_CONTENT,
    MULTI_LINE_STRING_LITERAL,
    LINE_STRING_LITERAL,
    STRING_LITERAL,
    LITERAL_CONSTANT,
    COLLECTION_LITERAL,
    PARENTHESIZED_EXPRESSION,
    PRIMARY_EXPRESSION,
    VALUE_ARGUMENT,
    VALUE_ARGUMENTS,
    TYPE_ARGUMENTS,
    ANNOTATED_LAMBDA,
    CALL_SUFFIX,
    NAVIGATION_SUFFIX,
    INDEXING_SUFFIX,
    ASSIGNABLE_SUFFIX,
    PARENTHESIZED_ASSIGNABLE_EXPRESSION,
    ASSIGNABLE_EXPRESSION,
    PARENTHESIZED_DIRECTLY_ASSIGNABLE_EXPRESSION,
    DIRECTLY_ASSIGNABLE_EXPRESSION,
    POSTFIX_UNARY_SUFFIX,
    POSTFIX_UNARY_EXPRESSION,
    UNARY_PREFIX,
    PREFIX_UNARY_EXPRESSION,
    AS_EXPRESSION,
    MULTIPLICATIVE_EXPRESSION,
    ADDITIVE_EXPRESSION,
    RANGE_EXPRESSION,
    INFIX_FUNCTION_CALL,
    ELVIS,
    ELVIS_EXPRESSION,
    INFIX_OPERATION,
    GENERIC_CALL_LIKE_COMPARISON,
    COMPARISON,
    EQUALITY,
    CONJUNCTION,
    DISJUNCTION,
    EXPRESSION,
    SEMIS,
    SEMI,
    ASSIGNMENT,
    DO_WHILE_STATEMENT,
    WHILE_STATEMENT,
    FOR_STATEMENT,
    LOOP_STATEMENT,
    BLOCK,
    CONTROL_STRUCTURE_BODY,
    LABEL,
    STATEMENT,
    STATEMENTS,
    DEFINITELY_NON_NULLABLE_TYPE,
    PARENTHESIZED_USER_TYPE,
    RECEIVER_TYPE,
    PARENTHESIZED_TYPE,
    FUNCTION_TYPE_PARAMETERS,
    FUNCTION_TYPE,
    TYPE_PROJECTION_MODIFIER,
    TYPE_PROJECTION_MODIFIERS,
    TYPE_PROJECTION,
    SIMPLE_USER_TYPE,
    USER_TYPE,
    QUEST,
    NULLABLE_TYPE,
    TYPE_REFERENCE,
    TYPE,
    ENUM_ENTRY,
    ENUM_ENTRIES,
    ENUM_CLASS_BODY,
    CONSTRUCTOR_DELEGATION_CALL,
    SECONDARY_CONSTRUCTOR,
    OBJECT_DECLARATION,
    PARAMETER,
    PARAMETER_WITH_OPTIONAL_TYPE,
    FUNCTION_VALUE_PARAMETER_WITH_OPTIONAL_TYPE,
    PARAMETERS_WITH_OPTIONAL_TYPE,
    SETTER,
    GETTER,
    PROPERTY_DELEGATE,
    PROPERTY_DECLARATION,
    MULTI_VARIABLE_DECLARATION,
    VARIABLE_DECLARATION,
    FUNCTION_BODY,
    FUNCTION_DECLARATION,
    FUNCTION_VALUE_PARAMETER,
    FUNCTION_VALUE_PARAMETERS,
    COMPANION_OBJECT,
    ANONYMOUS_INITIALIZER,
    CLASS_MEMBER_DECLARATION,
    CLASS_MEMBER_DECLARATIONS,
    TYPE_CONSTRAINT,
    TYPE_CONSTRAINTS,
    TYPE_PARAMETER,
    TYPE_PARAMETERS,
    EXPLICIT_DELEGATION,
    ANNOTATED_DELEGATION_SPECIFIER,
    CONSTRUCTOR_INVOCATION,
    DELEGATION_SPECIFIER,
    DELEGATION_SPECIFIERS,
    CLASS_PARAMETER,
    CLASS_PARAMETERS,
    CLASS_BODY,
    PRIMARY_CONSTRUCTOR,
    CLASS_DECLARATION,
    DECLARATION,
    TYPE_ALIAS,
    TOP_LEVEL_OBJECT,
    IMPORT_ALIAS,
    IMPORT_HEADER,
    IMPORT_LIST,
    PACKAGE_HEADER,
    FILE_ANNOTATION,
    SHEBANG_LINE,
    SCRIPT,
    KOTLIN_FILE,
    ROOT,
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
pub type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntaxKind {
    Token(Token),
    Syntax(Syntax),
}

impl From<Syntax> for rowan::SyntaxKind {
    fn from(kind: Syntax) -> Self {
        Self(kind as u16)
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        match kind {
            SyntaxKind::Token(token) => Self(token as u16),
            SyntaxKind::Syntax(kind) => Self(kind as u16),
        }
    }
}

use Syntax::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lang;

impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        if raw.0 < 154 {
            // safety: Token is repr u16
            SyntaxKind::Token(unsafe { std::mem::transmute::<u16, Token>(raw.0) })
        } else {
            assert!(raw.0 <= ROOT as u16);
            // safety: SyntaxKind is repr u16
            SyntaxKind::Syntax(unsafe { std::mem::transmute::<u16, Syntax>(raw.0) })
        }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

#[cfg(test)]
mod test {
    use rowan::Language;

    use super::*;

    #[test]
    fn test_conversion() {
        assert_eq!(
            Lang::kind_from_raw(rowan::SyntaxKind(ROOT as u16)),
            SyntaxKind::Syntax(ROOT)
        );
        assert_eq!(
            Lang::kind_from_raw(rowan::SyntaxKind(Token::IDENTIFIER as u16)),
            SyntaxKind::Token(Token::IDENTIFIER)
        );
        assert_eq!(
            Lang::kind_from_raw(rowan::SyntaxKind(SHEBANG_LINE as u16)),
            SyntaxKind::Syntax(SHEBANG_LINE)
        );
    }
}
