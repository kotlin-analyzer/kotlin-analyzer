#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum Token {
    DELIMITED_COMMENT,
    LINE_COMMENT,
    SHEBANG_LINE,
    WHITESPACE,
    NEWLINE,
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
    ASSIGNMENT,
    ADD_ASSIGNMENT,
    SUB_ASSIGNMENT,
    MULT_ASSIGNMENT,
    DIV_ASSIGNMENT,
    MOD_ASSIGNMENT,
    ARROW,
    RANGE,
    RANGE_UNTIL,
    COLON_COLON,
    // listed in the spec, but not actually used in the grammar
    // DOUBLE_SEMICOLON,
    // DOUBLE_ARROW,
    // HASH,
    AT,
    QUEST,
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
    AMP,
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
    SETPARAM,
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
    CONTEXT,
    INTEGER_LITERAL,
    REAL_LITERAL,
    HEX_LITERAL,
    BIN_LITERAL,
    LONG_LITERAL,
    UNSIGNED_LITERAL,
    BOOLEAN_LITERAL,
    NULL_LITERAL,
    CHARACTER_LITERAL,
    IDENTIFIER_TOKEN,
    QUOTE_OPEN,
    QUOTE_CLOSE,
    TRIPLE_QUOTE_OPEN,
    TRIPLE_QUOTE_CLOSE,
    LINE_STR_REF,
    MULTI_LINE_STR_REF,
    MULTI_LINE_STRING_QUOTE,
    LINE_STR_TEXT,
    MULTI_LINE_STR_TEXT,
    LINE_STR_ESCAPED_CHAR,
    LINE_STR_EXPR_START,
    MULTI_STR_EXPR_START,
    EOF,
    // FIXME: rename to Unknown
    ERR,
}

use Token::*;

impl Token {
    pub fn from_operator(raw: &str) -> Option<Self> {
        match raw {
            "..." => Some(RESERVED),

            "." => Some(DOT),

            "," => Some(COMMA),

            "(" => Some(L_PAREN),

            ")" => Some(R_PAREN),

            "[" => Some(L_SQUARE),

            "]" => Some(R_SQUARE),

            "{" => Some(L_CURL),

            "}" => Some(R_CURL),

            "*" => Some(MULT),

            "%" => Some(MOD),

            "/" => Some(DIV),

            "+" => Some(ADD),

            "-" => Some(SUB),
            "-=" => Some(SUB_ASSIGNMENT),

            "++" => Some(INCR),

            "--" => Some(DECR),

            "&&" => Some(CONJ),

            "||" => Some(DISJ),

            "!" => Some(EXCL_NO_WS),

            ":" => Some(COLON),

            ";" => Some(SEMICOLON),

            "=" => Some(ASSIGNMENT),

            "+=" => Some(ADD_ASSIGNMENT),

            "*=" => Some(MULT_ASSIGNMENT),

            "/=" => Some(DIV_ASSIGNMENT),

            "%=" => Some(MOD_ASSIGNMENT),

            "->" => Some(ARROW),

            // "=>" => Some(DOUBLE_ARROW),
            ".." => Some(RANGE),

            "..<" => Some(RANGE_UNTIL),

            "::" => Some(COLON_COLON),

            // ";;" => Some(DOUBLE_SEMICOLON),

            // "#" => Some(HASH),
            "@" => Some(AT),

            "?" => Some(QUEST),

            "<" => Some(L_ANGLE),

            ">" => Some(R_ANGLE),

            "<=" => Some(LE),

            ">=" => Some(GE),

            "!=" => Some(EXCL_EQ),

            "!==" => Some(EXCL_EQ_EQ),

            "==" => Some(EQ_EQ),

            "===" => Some(EQ_EQ_EQ),

            "'" => Some(SINGLE_QUOTE),

            "&" => Some(AMP),

            // activates string mode
            "\"" => Some(QUOTE_OPEN),

            // activates multiline string mode
            r#"""""# => Some(TRIPLE_QUOTE_OPEN),

            // whitespaces
            "\u{0020}" => Some(WHITESPACE),
            "\u{0009}" => Some(WHITESPACE),
            "\u{000C}" => Some(WHITESPACE),

            // new lines
            "\u{000A}" => Some(NEWLINE),
            "\u{000D}" => Some(NEWLINE),
            "\u{000A}\u{000D}" => Some(NEWLINE),
            _ => None,
        }
    }
    pub fn from_hard_keyword(raw: &str) -> Option<Self> {
        match raw {
            "!in" => Some(NOT_IN),
            "!is" => Some(NOT_IS),
            "annotation" => Some(ANNOTATION),
            "as" => Some(AS),
            "as?" => Some(AS_SAFE),
            "break" => Some(BREAK),
            "class" => Some(CLASS),
            "do" => Some(DO),
            "else" => Some(ELSE),
            "false" => Some(BOOLEAN_LITERAL),
            "for" => Some(FOR),
            "fun" => Some(FUN),
            "if" => Some(IF),
            "in" => Some(IN),
            "interface" => Some(INTERFACE),
            "is" => Some(IS),
            "null" => Some(NULL_LITERAL),
            "object" => Some(OBJECT),
            "package" => Some(PACKAGE),
            "param" => Some(PARAM),
            "return" => Some(RETURN),
            "super" => Some(SUPER),
            "this" => Some(THIS),
            "throw" => Some(THROW),
            "true" => Some(BOOLEAN_LITERAL),
            "try" => Some(TRY),
            "typealias" => Some(TYPE_ALIAS),
            "typeof" => Some(TYPEOF),
            "val" => Some(VAL),
            "var" => Some(VAR),
            "when" => Some(WHEN),
            "while" => Some(WHILE),
            "continue" => Some(CONTINUE),
            _ => None,
        }
    }
    pub fn from_soft_keyword(raw: &str) -> Option<Self> {
        match raw {
            "abstract" => Some(ABSTRACT),
            "actual" => Some(ACTUAL),
            "annotation" => Some(ANNOTATION),
            "by" => Some(BY),
            "catch" => Some(CATCH),
            "companion" => Some(COMPANION),
            "const" => Some(CONST),
            "constructor" => Some(CONSTRUCTOR),
            "crossinline" => Some(CROSS_INLINE),
            "data" => Some(DATA),
            "delegate" => Some(DELEGATE),
            "dynamic" => Some(DYNAMIC),
            "enum" => Some(ENUM),
            "expect" => Some(EXPECT),
            "external" => Some(EXTERNAL),
            "field" => Some(FIELD),
            "file" => Some(FILE),
            "final" => Some(FINAL),
            "finally" => Some(FINALLY),
            "get" => Some(GET),
            "import" => Some(IMPORT),
            "infix" => Some(INFIX),
            "init" => Some(INIT),
            "inline" => Some(INLINE),
            "inner" => Some(INNER),
            "internal" => Some(INTERNAL),
            "lateinit" => Some(LATEINIT),
            "noinline" => Some(NO_INLINE),
            "open" => Some(OPEN),
            "operator" => Some(OPERATOR),
            "out" => Some(OUT),
            "override" => Some(OVERRIDE),
            "param" => Some(PARAM),
            "private" => Some(PRIVATE),
            "property" => Some(PROPERTY),
            "protected" => Some(PROTECTED),
            "public" => Some(PUBLIC),
            "receiver" => Some(RECEIVER),
            "reified" => Some(REIFIED),
            "sealed" => Some(SEALED),
            "set" => Some(SET),
            "setparam" => Some(SETPARAM),
            "suspend" => Some(SUSPEND),
            "tailrec" => Some(TAILREC),
            "value" => Some(VALUE),
            "context" => Some(CONTEXT),
            "vararg" => Some(VAR_ARG),
            "where" => Some(WHERE),
            _ => None,
        }
    }
    pub fn resolve_token(key: &str) -> Option<Self> {
        Self::from_hard_keyword(key)
            .or_else(|| Self::from_soft_keyword(key))
            .or_else(|| Self::from_operator(key))
    }
    pub fn from_keyword(key: &str) -> Option<Self> {
        Self::from_hard_keyword(key).or_else(|| Self::from_soft_keyword(key))
    }
}

#[cfg(test)]
mod token_id_test {

    use super::*;

    #[test]
    fn from_operator_test() {
        assert!(Token::from_operator(":").is_some());
        assert!(Token::from_operator("final").is_none());
        assert!(Token::from_operator("fun").is_none());
    }

    #[test]
    fn from_keyword_test() {
        assert!(Token::from_keyword(":").is_none());
        assert!(Token::from_keyword("final").is_some());
        assert!(Token::from_keyword("fun").is_some());
    }

    #[test]
    fn from_soft_keyword_test() {
        assert!(Token::from_soft_keyword(":").is_none());
        assert!(Token::from_soft_keyword("final").is_some());
        assert!(Token::from_soft_keyword("fun").is_none());
    }
}
