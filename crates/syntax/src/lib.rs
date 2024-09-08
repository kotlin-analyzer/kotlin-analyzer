use tokens::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
/// This represents tokens and composite nodes
/// Adapted from https://kotlinlang.org/spec/syntax-and-grammar.html#syntax-grammar
pub enum SyntaxKind {
    // tokens
    SHEBANG_LINE_TOKEN = 0,
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
    ERR,
    // Main syntax starts from here
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
    ROOT,
}

impl From<Token> for SyntaxKind {
    fn from(value: Token) -> Self {
        match value {
            Token::SHEBANG_LINE_TOKEN => SHEBANG_LINE_TOKEN,
            Token::DELIMITED_COMMENT => DELIMITED_COMMENT,
            Token::LINE_COMMENT => LINE_COMMENT,
            Token::WS => WS,
            Token::NL => NL,
            Token::RESERVED => RESERVED,
            Token::DOT => DOT,
            Token::COMMA => COMMA,
            Token::L_PAREN => L_PAREN,
            Token::R_PAREN => R_PAREN,
            Token::L_SQUARE => L_SQUARE,
            Token::R_SQUARE => R_SQUARE,
            Token::L_CURL => L_CURL,
            Token::R_CURL => R_CURL,
            Token::MULT => MULT,
            Token::MOD => MOD,
            Token::DIV => DIV,
            Token::ADD => ADD,
            Token::SUB => SUB,
            Token::INCR => INCR,
            Token::DECR => DECR,
            Token::CONJ => CONJ,
            Token::DISJ => DISJ,
            Token::EXCL_WS => EXCL_WS,
            Token::EXCL_NO_WS => EXCL_NO_WS,
            Token::COLON => COLON,
            Token::SEMICOLON => SEMICOLON,
            Token::ASSIGNMENT_TOKEN => ASSIGNMENT_TOKEN,
            Token::ADD_ASSIGNMENT => ADD_ASSIGNMENT,
            Token::SUB_ASSIGNMENT => SUB_ASSIGNMENT,
            Token::MULT_ASSIGNMENT => MULT_ASSIGNMENT,
            Token::DIV_ASSIGNMENT => DIV_ASSIGNMENT,
            Token::MOD_ASSIGNMENT => MOD_ASSIGNMENT,
            Token::ARROW => ARROW,
            Token::DOUBLE_ARROW => DOUBLE_ARROW,
            Token::RANGE => RANGE,
            Token::RANGE_LESS => RANGE_LESS,
            Token::COLON_COLON => COLON_COLON,
            Token::DOUBLE_SEMICOLON => DOUBLE_SEMICOLON,
            Token::HASH => HASH,
            Token::AT_NO_WS => AT_NO_WS,
            Token::AT_POST_WS => AT_POST_WS,
            Token::AT_PRE_WS => AT_PRE_WS,
            Token::AT_BOTH_WS => AT_BOTH_WS,
            Token::QUEST_WS => QUEST_WS,
            Token::QUEST_NO_WS => QUEST_NO_WS,
            Token::L_ANGLE => L_ANGLE,
            Token::R_ANGLE => R_ANGLE,
            Token::LE => LE,
            Token::GE => GE,
            Token::EXCL_EQ => EXCL_EQ,
            Token::EXCL_EQ_EQ => EXCL_EQ_EQ,
            Token::AS_SAFE => AS_SAFE,
            Token::EQ_EQ => EQ_EQ,
            Token::EQ_EQ_EQ => EQ_EQ_EQ,
            Token::SINGLE_QUOTE => SINGLE_QUOTE,
            Token::RETURN_AT => RETURN_AT,
            Token::CONTINUE_AT => CONTINUE_AT,
            Token::BREAK_AT => BREAK_AT,
            Token::THIS_AT => THIS_AT,
            Token::SUPER_AT => SUPER_AT,
            Token::FILE => FILE,
            Token::FIELD => FIELD,
            Token::PROPERTY => PROPERTY,
            Token::GET => GET,
            Token::SET => SET,
            Token::RECEIVER => RECEIVER,
            Token::PARAM => PARAM,
            Token::SET_PARAM => SET_PARAM,
            Token::DELEGATE => DELEGATE,
            Token::PACKAGE => PACKAGE,
            Token::IMPORT => IMPORT,
            Token::CLASS => CLASS,
            Token::INTERFACE => INTERFACE,
            Token::FUN => FUN,
            Token::OBJECT => OBJECT,
            Token::VAL => VAL,
            Token::VAR => VAR,
            Token::TYPE_ALIAS => TYPE_ALIAS,
            Token::CONSTRUCTOR => CONSTRUCTOR,
            Token::BY => BY,
            Token::COMPANION => COMPANION,
            Token::INIT => INIT,
            Token::THIS => THIS,
            Token::SUPER => SUPER,
            Token::TYPEOF => TYPEOF,
            Token::WHERE => WHERE,
            Token::IF => IF,
            Token::ELSE => ELSE,
            Token::WHEN => WHEN,
            Token::TRY => TRY,
            Token::CATCH => CATCH,
            Token::FINALLY => FINALLY,
            Token::FOR => FOR,
            Token::DO => DO,
            Token::WHILE => WHILE,
            Token::THROW => THROW,
            Token::RETURN => RETURN,
            Token::CONTINUE => CONTINUE,
            Token::BREAK => BREAK,
            Token::AS => AS,
            Token::IS => IS,
            Token::IN => IN,
            Token::NOT_IS => NOT_IS,
            Token::NOT_IN => NOT_IN,
            Token::OUT => OUT,
            Token::DYNAMIC => DYNAMIC,
            Token::PUBLIC => PUBLIC,
            Token::PRIVATE => PRIVATE,
            Token::PROTECTED => PROTECTED,
            Token::INTERNAL => INTERNAL,
            Token::ENUM => ENUM,
            Token::SEALED => SEALED,
            Token::ANNOTATION => ANNOTATION,
            Token::DATA => DATA,
            Token::INNER => INNER,
            Token::TAILREC => TAILREC,
            Token::OPERATOR => OPERATOR,
            Token::INLINE => INLINE,
            Token::INFIX => INFIX,
            Token::EXTERNAL => EXTERNAL,
            Token::SUSPEND => SUSPEND,
            Token::OVERRIDE => OVERRIDE,
            Token::ABSTRACT => ABSTRACT,
            Token::FINAL => FINAL,
            Token::OPEN => OPEN,
            Token::CONST => CONST,
            Token::LATEINIT => LATEINIT,
            Token::VAR_ARG => VAR_ARG,
            Token::NO_INLINE => NO_INLINE,
            Token::CROSS_INLINE => CROSS_INLINE,
            Token::REIFIED => REIFIED,
            Token::EXPECT => EXPECT,
            Token::ACTUAL => ACTUAL,
            Token::VALUE => VALUE,
            Token::INTEGER_LITERAL => INTEGER_LITERAL,
            Token::REAL_LITERAL => REAL_LITERAL,
            Token::HEX_LITERAL => HEX_LITERAL,
            Token::BIN_LITERAL => BIN_LITERAL,
            Token::LONG_LITERAL => LONG_LITERAL,
            Token::BOOLEAN_LITERAL => BOOLEAN_LITERAL,
            Token::NULL_LITERAL => NULL_LITERAL,
            Token::CHARACTER_LITERAL => CHARACTER_LITERAL,
            Token::QUOTE_OPEN => QUOTE_OPEN,
            Token::QUOTE_CLOSE => QUOTE_CLOSE,
            Token::TRIPLE_QUOTE_OPEN => TRIPLE_QUOTE_OPEN,
            Token::TRIPLE_QUOTE_CLOSE => TRIPLE_QUOTE_CLOSE,
            Token::IDENTIFIER => IDENTIFIER,
            Token::LINE_STR_REF => LINE_STR_REF,
            Token::MULTI_LINE_STR_REF => MULTI_LINE_STR_REF,
            Token::MULTI_LINE_STRING_QUOTE => MULTI_LINE_STRING_QUOTE,
            Token::LINE_STR_TEXT => LINE_STR_TEXT,
            Token::MULTI_LINE_STR_TEXT => MULTI_LINE_STR_TEXT,
            Token::LINE_STR_ESCAPED_CHAR => LINE_STR_ESCAPED_CHAR,
            Token::LINE_STR_EXPR_START => LINE_STR_EXPR_START,
            Token::MULTI_STR_EXPR_START => MULTI_STR_EXPR_START,
            Token::EOF => EOF,
            Token::ERR => ERR,
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

use SyntaxKind::*;

pub fn cast_syntax_kind(syntax: SyntaxKind) -> impl Fn(SyntaxNode) -> Option<SyntaxNode> {
    move |node| {
        if node.kind() == syntax {
            Some(node)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lang;

impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ROOT as u16);
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
