#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
pub enum Token {
    ///! White Space and Comments    
    SHEBANG_LINE_TOKEN,
    DELIMITED_COMMENT,
    LINE_COMMENT,
    WS,
    NL,

    //# REGION KEYWORDS AND OPERATORS
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
    // ! not named in docs
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

    //# ENDREGION
    /// LITERALS
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
}

use phf::phf_map;
use Token::*;

pub fn is_keyword(slice: &str) -> bool {
    get_keyword(&slice.to_lowercase()).is_some()
}

pub fn is_soft_keyword(slice: &str) -> bool {
    SOFT_KEYWORDS.get(&slice.to_lowercase()).is_some()
}

pub fn is_operator(slice: &str) -> bool {
    OPERATORS.get(&slice.to_lowercase()).is_some()
}

pub fn get_keyword(key: &str) -> Option<&Token> {
    HARD_KEYWORDS.get(key).or(SOFT_KEYWORDS.get(key))
}

// keep this sorted
pub static HARD_KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "!in" => NOT_IN,
    "!is" => NOT_IS,
    "annotation" => ANNOTATION,
    "as" => AS,
    "as?" => AS_SAFE,
    "break" => BREAK,
    "class" => CLASS,
    "do" => DO,
    "else" => ELSE,
    "false" => BOOLEAN_LITERAL,
    "for" => FOR,
    "fun" => FUN,
    "if" => IF,
    "in" => IN,
    "interface" => INTERFACE,
    "is" => IS,
    "null" => NULL_LITERAL,
    "object" => OBJECT,
    "package" => PACKAGE,
    "param" => PARAM,
    "return" => RETURN,
    "super" => SUPER,
    "this" => THIS,
    "throw" => THROW,
    "true" => BOOLEAN_LITERAL,
    "try" => TRY,
    "typealias" => TYPE_ALIAS,
    "typeof" => TYPEOF,
    "val" => VAL,
    "var" => VAR,
    "when" => WHEN,
    "while" => WHILE,
    "continue" => CONTINUE,
};

pub static OPERATORS: phf::Map<&'static str, Token> = phf_map! {
   "..." => RESERVED,

   "." => DOT,

   "," => COMMA,

   "(" => L_PAREN,

   ")" => R_PAREN,

   "[" => L_SQUARE,

   "]" => R_SQUARE,

   "{" => L_CURL,

   "}" => R_CURL,

   "*" => MULT,

   "%" => MOD,

   "/" => DIV,

   "+" => ADD,

   "-" => SUB,
   "-=" => SUB_ASSIGNMENT,

   "++" => INCR,

   "--" => DECR,

   "&&" => CONJ,

   "||" => DISJ,

   "!" => EXCL_NO_WS,

   ":" => COLON,

   ";" => SEMICOLON,

   "=" => ASSIGNMENT_TOKEN,

   "+=" => ADD_ASSIGNMENT,

   "*=" => MULT_ASSIGNMENT,

   "/=" => DIV_ASSIGNMENT,

   "%=" => MOD_ASSIGNMENT,

   "->" => ARROW,

   "=>" => DOUBLE_ARROW,

   ".." => RANGE,

   "..<" => RANGE_LESS,

   "::" => COLON_COLON,

   ";;" => DOUBLE_SEMICOLON,

   "#" => HASH,

   "@" => AT_NO_WS,

   "?" => QUEST_NO_WS,

   "<" => L_ANGLE,

   ">" => R_ANGLE,

   "<=" => LE,

   ">=" => GE,

   "!=" => EXCL_EQ,

   "!==" => EXCL_EQ_EQ,

   "==" => EQ_EQ,

   "===" => EQ_EQ_EQ,

   "'" => SINGLE_QUOTE,

   // activates string mode
   "\"" => QUOTE_OPEN,

    // activates multiline string mode
   r#"""""# => TRIPLE_QUOTE_OPEN,

   // whitespaces
   "\u{0020}" => WS,
   "\u{0009}" => WS,
   "\u{000C}" => WS,

   // new lines
   "\u{000A}" => NL,
   "\u{000D}" => NL,
   "\u{000A}\u{000D}" => NL,
};

pub static SOFT_KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "abstract" => ABSTRACT,
    "actual" => ACTUAL,
    "annotation" => ANNOTATION,
    "by" => BY,
    "catch" => CATCH,
    "companion" => COMPANION,
    "const" => CONST,
    "constructor" => CONSTRUCTOR,
    "crossinline" => CROSS_INLINE,
    "data" => DATA,
    "delegate" => DELEGATE,
    "dynamic" => DYNAMIC,
    "enum" => ENUM,
    "expect" => EXPECT,
    "external" => EXTERNAL,
    "field" => FIELD,
    "file" => FILE,
    "final" => FINAL,
    "finally" => FINALLY,
    "get" => GET,
    "import" => IMPORT,
    "infix" => INFIX,
    "init" => INIT,
    "inline" => INLINE,
    "inner" => INNER,
    "internal" => INTERNAL,
    "lateinit" => LATEINIT,
    "noinline" => NO_INLINE,
    "open" => OPEN,
    "operator" => OPERATOR,
    "out" => OUT,
    "override" => OVERRIDE,
    "param" => PARAM,
    "private" => PRIVATE,
    "property" => PROPERTY,
    "protected" => PROTECTED,
    "public" => PUBLIC,
    "receiver" => RECEIVER,
    "reified" => REIFIED,
    "sealed" => SEALED,
    "set" => SET,
    "setparam" => SET_PARAM,
    "suspend" => SUSPEND,
    "tailrec" => TAILREC,
    "value" => VALUE,
    "vararg" => VAR_ARG,
    "where" => WHERE,
};

#[macro_export]
macro_rules! T {
    ($lit: tt) => {{
        let key = format!("{}", stringify!($lit));
        let token = resolve_token(&key).expect(&format!("expected a valid token, but got {key}"));
        token
    }};
}

pub fn resolve_token(key: &str) -> Option<Token> {
    HARD_KEYWORDS
        .get(key)
        .or(SOFT_KEYWORDS.get(key))
        .or(OPERATORS.get(key))
        .copied()
}

#[cfg(test)]
mod token_id_test {

    use assert_panic::assert_panic;

    use super::*;

    #[test]
    fn is_operator_test() {
        assert!(is_operator(":"));
        assert!(!is_operator("final"));
        assert!(!is_operator("fun"));
    }

    #[test]
    fn is_keyword_test() {
        assert!(!is_keyword(":"));
        assert!(is_keyword("final"));
        assert!(is_keyword("fun"));
    }

    #[test]
    fn is_soft_keyword_test() {
        assert!(!is_soft_keyword(":"));
        assert!(is_soft_keyword("final"));
        assert!(!is_soft_keyword("fun"));
    }

    #[test]
    fn test_t() {
        assert_eq!(T![;], Token::SEMICOLON);
        assert_panic!(
            {
                let _ = T![foo];
            },
            String,
            "expected a valid token, but got foo"
        );
    }
}
