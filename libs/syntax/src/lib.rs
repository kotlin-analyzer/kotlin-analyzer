mod entries;

pub use entries::Cast;
pub use entries::*;

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

            "=>" => Some(DOUBLE_ARROW),

            ".." => Some(RANGE),

            "..<" => Some(RANGE_LESS),

            "::" => Some(COLON_COLON),

            ";;" => Some(DOUBLE_SEMICOLON),

            "#" => Some(HASH),

            "@" => Some(AT_NO_WS),

            "?" => Some(QUEST_NO_WS),

            "<" => Some(L_ANGLE),

            ">" => Some(R_ANGLE),

            "<=" => Some(LE),

            ">=" => Some(GE),

            "!=" => Some(EXCL_EQ),

            "!==" => Some(EXCL_EQ_EQ),

            "==" => Some(EQ_EQ),

            "===" => Some(EQ_EQ_EQ),

            "'" => Some(SINGLE_QUOTE),

            // activates string mode
            "\"" => Some(QUOTE_OPEN),

            // activates multiline string mode
            r#"""""# => Some(TRIPLE_QUOTE_OPEN),

            // whitespaces
            "\u{0020}" => Some(WS),
            "\u{0009}" => Some(WS),
            "\u{000C}" => Some(WS),

            // new lines
            "\u{000A}" => Some(NL),
            "\u{000D}" => Some(NL),
            "\u{000A}\u{000D}" => Some(NL),
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
            "setparam" => Some(SET_PARAM),
            "suspend" => Some(SUSPEND),
            "tailrec" => Some(TAILREC),
            "value" => Some(VALUE),
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
