use crate::nodes::Cast;
use casey::pascal;
use tokens::Token;

macro_rules! define_syntax {
    (
        $(#[$attr:meta])*
        $vis:vis enum $name:ident {
            tokens {$($token:ident),+}
            syntax {$($syntax:ident),+}
        }
    ) => {
        $(#[$attr])*
        $vis enum $name {
            $($token),*,
            $($syntax),*
        }

        $(define_syntax!($token in $name);)*
        $(define_syntax!($syntax in $name);)*

        impl From<Token> for $name {
            fn from(value: Token) -> Self {
                match value {
                    $(Token::$token => $name::$token),*
                }
            }
        }
    };
    ($id:ident in $name:ident) => {
        pascal!{
            #[derive(PartialEq, Eq, Hash, Clone, Debug)]
            #[repr(transparent)]
            pub struct $id(pub SyntaxNode);
        }

        impl Cast for pascal!($id) {
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == $name::$id {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }
    };
}

define_syntax! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[allow(non_camel_case_types)]
    #[repr(u16)]
    pub enum SyntaxKind {
        tokens {
            SHEBANG_LINE_TOKEN,
            DELIMITED_COMMENT,
            LINE_COMMENT,
            WS,
            NL,
            RESERVED,
            DOT,
            COMMA,
            L_PAREN,
            R_PAREN,
            L_SQUARE,
            R_SQUARE,
            L_CURL,
            R_CURL,
            MULT,
            MOD,
            DIV,
            ADD,
            SUB,
            INCR,
            DECR,
            CONJ,
            DISJ,
            EXCL_WS,
            EXCL_NO_WS,
            COLON,
            SEMICOLON,
            ASSIGNMENT_TOKEN,
            ADD_ASSIGNMENT,
            SUB_ASSIGNMENT,
            MULT_ASSIGNMENT,
            DIV_ASSIGNMENT,
            MOD_ASSIGNMENT,
            ARROW,
            DOUBLE_ARROW,
            RANGE,
            RANGE_LESS,
            COLON_COLON,
            DOUBLE_SEMICOLON,
            HASH,
            AT_NO_WS,
            AT_POST_WS,
            AT_PRE_WS,
            AT_BOTH_WS,
            QUEST_WS,
            QUEST_NO_WS,
            L_ANGLE,
            R_ANGLE,
            LE,
            GE,
            EXCL_EQ,
            EXCL_EQ_EQ,
            AS_SAFE,
            EQ_EQ,
            EQ_EQ_EQ,
            SINGLE_QUOTE,
            RETURN_AT,
            CONTINUE_AT,
            BREAK_AT,
            THIS_AT,
            SUPER_AT,
            FILE,
            FIELD,
            PROPERTY,
            GET,
            SET,
            RECEIVER,
            PARAM,
            SET_PARAM,
            DELEGATE,
            PACKAGE,
            IMPORT,
            CLASS,
            INTERFACE,
            FUN,
            OBJECT,
            VAL,
            VAR,
            TYPE_ALIAS,
            CONSTRUCTOR,
            BY,
            COMPANION,
            INIT,
            THIS,
            SUPER,
            TYPEOF,
            WHERE,
            IF,
            ELSE,
            WHEN,
            TRY,
            CATCH,
            FINALLY,
            FOR,
            DO,
            WHILE,
            THROW,
            RETURN,
            CONTINUE,
            BREAK,
            AS,
            IS,
            IN,
            NOT_IS,
            NOT_IN,
            OUT,
            DYNAMIC,
            PUBLIC,
            PRIVATE,
            PROTECTED,
            INTERNAL,
            ENUM,
            SEALED,
            ANNOTATION,
            DATA,
            INNER,
            TAILREC,
            OPERATOR,
            INLINE,
            INFIX,
            EXTERNAL,
            SUSPEND,
            OVERRIDE,
            ABSTRACT,
            FINAL,
            OPEN,
            CONST,
            LATEINIT,
            VAR_ARG,
            NO_INLINE,
            CROSS_INLINE,
            REIFIED,
            EXPECT,
            ACTUAL,
            VALUE,
            INTEGER_LITERAL,
            REAL_LITERAL,
            HEX_LITERAL,
            BIN_LITERAL,
            LONG_LITERAL,
            BOOLEAN_LITERAL,
            NULL_LITERAL,
            CHARACTER_LITERAL,
            QUOTE_OPEN,
            QUOTE_CLOSE,
            TRIPLE_QUOTE_OPEN,
            TRIPLE_QUOTE_CLOSE,
            IDENTIFIER,
            LINE_STR_REF,
            MULTI_LINE_STR_REF,
            MULTI_LINE_STRING_QUOTE,
            LINE_STR_TEXT,
            MULTI_LINE_STR_TEXT,
            LINE_STR_ESCAPED_CHAR,
            LINE_STR_EXPR_START,
            MULTI_STR_EXPR_START,
            EOF,
            ERR
        }
        syntax {
            SIMPLE_IDENTIFIER,
            UNESCAPED_ANNOTATION,
            ANNOTATION_USE_SITE_TARGET,
            MULTI_ANNOTATION,
            SINGLE_ANNOTATION,
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
            TOP_LEVEL_OBJECT,
            IMPORT_ALIAS,
            IMPORT_HEADER,
            IMPORT_LIST,
            PACKAGE_HEADER,
            FILE_ANNOTATION,
            SHEBANG_LINE,
            SCRIPT,
            KOTLIN_FILE,
            ROOT
        }
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
pub type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lang;

impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::ROOT as u16);
        // safety: SyntaxKind is repr u16
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

#[cfg(test)]
mod test {
    use rowan::Language;
    use SyntaxKind::*;

    use super::*;

    #[test]
    fn test_conversion() {
        assert_eq!(Lang::kind_from_raw(rowan::SyntaxKind(ROOT as u16)), ROOT);
        assert_eq!(
            Lang::kind_from_raw(rowan::SyntaxKind(SHEBANG_LINE as u16)),
            SHEBANG_LINE
        );
    }
}
