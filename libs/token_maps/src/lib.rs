use phf::{phf_map, phf_set};
use tokens::Token::{self, *};

pub fn is_keyword(slice: &str) -> bool {
    KEYWORDS.get(&slice.to_lowercase()).is_some()
}

pub fn is_soft_keyword(slice: &str) -> bool {
    SOFT_KEYWORDS.contains(&slice.to_lowercase())
}

pub fn is_operator(slice: &str) -> bool {
    OPERATORS.get(&slice.to_lowercase()).is_some()
}

pub static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {

   "file" => FILE,

   "field" => FIELD,

   "property" => PROPERTY,

   "get" => GET,

   "set" => SET,

   "receiver" => RECEIVER,

   "param" => PARAM,

   "setparam" => SET_PARAM,

   "delegate" => DELEGATE,

   "package" => PACKAGE,

   "import" => IMPORT,

   "class" => CLASS,

   "interface" => INTERFACE,

   "fun" => FUN,

   "object" => OBJECT,

   "val" => VAL,

   "var" => VAR,

   "typealias" => TYPE_ALIAS,

   "constructor" => CONSTRUCTOR,

   "by" => BY,

   "companion" => COMPANION,

   "init" => INIT,

   "this" => THIS,

   "super" => SUPER,

   "typeof" => TYPEOF,

   "where" => WHERE,

   "if" => IF,

   "else" => ELSE,

   "when" => WHEN,

   "try" => TRY,

   "catch" => CATCH,

   "finally" => FINALLY,

   "for" => FOR,

   "do" => DO,

   "while" => WHILE,

   "throw" => THROW,

   "return" => RETURN,

   "continue" => CONTINUE,

   "break" => BREAK,

   "as" => AS,

   "as?" => AS_SAFE,

   "is" => IS,

   "in" => IN,

   "!is" => NOT_IS,

   "!in" => NOT_IN,

   "out" => OUT,

   "dynamic" => DYNAMIC,

   "public" => PUBLIC,

   "private" => PRIVATE,

   "protected" => PROTECTED,

   "internal" => INTERNAL,

   "enum" => ENUM,

   "sealed" => SEALED,

   "annotation" => ANNOTATION,

   "data" => DATA,

   "inner" => INNER,

   "tailrec" => TAILREC,

   "operator" => OPERATOR,

   "inline" => INLINE,

   "infix" => INFIX,

   "external" => EXTERNAL,

   "suspend" => SUSPEND,

   "override" => OVERRIDE,

   "abstract" => ABSTRACT,

   "final" => FINAL,

   "open" => OPEN,

   "const" => CONST,

   "lateinit" => LATEINIT,

   "vararg" => VAR_ARG,

   "noinline" => NO_INLINE,

   "crossinline" => CROSS_INLINE,

   "reified" => REIFIED,

   "expect" => EXPECT,

   "actual" => ACTUAL,

   "value" => VALUE,

   // literals
   "null" => NULL_LITERAL,
   "true" => BOOLEAN_LITERAL,
   "false" => BOOLEAN_LITERAL,
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

   "++" => INCR,

   "--" => DECR,

   "||" => DISJ,

   "!" => EXCL_NO_WS,

   ":" => COLON,

   ";" => SEMICOLON,

   "=" => ASSIGNMENT,

   "+=" => ADD_ASSIGNMENT,

   "*=" => MULT_ASSIGNMENT,

   "/=" => DIV_ASSIGNMENT,

   "%=" => MOD_ASSIGNMENT,

   "->" => ARROW,

   "=>" => DOUBLE_ARROW,

   ".." => RANGE,

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

pub static SOFT_KEYWORDS: phf::Set<&'static str> = phf_set! {
   "abstract",
   "annotation",
   "by",
   "catch",
   "companion",
   "constructor",
   "crossinline",
   "data",
   "dynamic",
   "enum",
   "external",
   "final",
   "finally",
   "import",
   "infix",
   "init",
   "inline",
   "inner",
   "internal",
   "lateinit",
   "noinline",
   "open",
   "operator",
   "out",
   "override",
   "private",
   "protected",
   "public",
   "reified",
   "sealed",
   "tailrec",
   "vararg",
   "where",
   "get",
   "set",
   "field",
   "property",
   "receiver",
   "param",
   "setparam",
   "delegate",
   "file",
   "expect",
   "actual",
   "const",
   "suspend",
   "value"
};

#[cfg(test)]
mod token_id_test {

    use super::*;

    #[test]
    fn is_operator_test() {
        assert_eq!(is_operator(":"), true);
        assert_eq!(is_operator("final"), false);
        assert_eq!(is_operator("fun"), false);
    }

    #[test]
    fn is_keyword_test() {
        assert_eq!(is_keyword(":"), false);
        assert_eq!(is_keyword("final"), true);
        assert_eq!(is_keyword("fun"), true);
    }

    #[test]
    fn is_soft_keyword_test() {
        assert_eq!(is_soft_keyword(":"), false);
        assert_eq!(is_soft_keyword("final"), true);
        assert_eq!(is_soft_keyword("fun"), false);
    }
}
