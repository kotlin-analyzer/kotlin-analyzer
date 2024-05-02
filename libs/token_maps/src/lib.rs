use phf::phf_map;
use tokens::Token;

// Attempt 1 - do it manually

pub static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {

   "file" => Token::FILE,

   "field" => Token::FIELD,

   "property" => Token::PROPERTY,

   "get" => Token::GET,

   "set" => Token::SET,

   "receiver" => Token::RECEIVER,

   "param" => Token::PARAM,

   "setparam" => Token::SET_PARAM,

   "delegate" => Token::DELEGATE,

   "package" => Token::PACKAGE,

   "import" => Token::IMPORT,

   "class" => Token::CLASS,

   "interface" => Token::INTERFACE,

   "fun" => Token::FUN,

   "object" => Token::OBJECT,

   "val" => Token::VAL,

   "var" => Token::VAR,

   "typealias" => Token::TYPE_ALIAS,

   "constructor" => Token::CONSTRUCTOR,

   "by" => Token::BY,

   "companion" => Token::COMPANION,

   "init" => Token::INIT,

   "this" => Token::THIS,

   "super" => Token::SUPER,

   "typeof" => Token::TYPEOF,

   "where" => Token::WHERE,

   "if" => Token::IF,

   "else" => Token::ELSE,

   "when" => Token::WHEN,

   "try" => Token::TRY,

   "catch" => Token::CATCH,

   "finally" => Token::FINALLY,

   "for" => Token::FOR,

   "do" => Token::DO,

   "while" => Token::WHILE,

   "throw" => Token::THROW,

   "return" => Token::RETURN,

   "continue" => Token::CONTINUE,

   "break" => Token::BREAK,

   "as" => Token::AS,

   "as?" => Token::AS_SAFE,

   "is" => Token::IS,

   "in" => Token::IN,

   "!is" => Token::NOT_IS,

   "!in" => Token::NOT_IN,

   "out" => Token::OUT,

   "dynamic" => Token::DYNAMIC,

   "public" => Token::PUBLIC,

   "private" => Token::PRIVATE,

   "protected" => Token::PROTECTED,

   "internal" => Token::INTERNAL,

   "enum" => Token::ENUM,

   "sealed" => Token::SEALED,

   "annotation" => Token::ANNOTATION,

   "data" => Token::DATA,

   "inner" => Token::INNER,

   "tailrec" => Token::TAILREC,

   "operator" => Token::OPERATOR,

   "inline" => Token::INLINE,

   "infix" => Token::INFIX,

   "external" => Token::EXTERNAL,

   "suspend" => Token::SUSPEND,

   "override" => Token::OVERRIDE,

   "abstract" => Token::ABSTRACT,

   "final" => Token::FINAL,

   "open" => Token::OPEN,

   "const" => Token::CONST,

   "lateinit" => Token::LATEINIT,

   "vararg" => Token::VAR_ARG,

   "noinline" => Token::NO_INLINE,

   "crossinline" => Token::CROSS_INLINE,

   "reified" => Token::REIFIED,

   "expect" => Token::EXPECT,

   "actual" => Token::ACTUAL,

   // literals
   "null" => Token::NULL_LITERAL,
   "true" => Token::BOOLEAN_LITERAL,
   "false" => Token::BOOLEAN_LITERAL,
};

pub static OPERATORS: phf::Map<&'static str, Token> = phf_map! {
   "..." => Token::RESERVED,

   "." => Token::DOT,

   "," => Token::COMMA,

   "(" => Token::L_PAREN,

   ")" => Token::R_PAREN,

   "[" => Token::L_SQUARE,

   "]" => Token::R_SQUARE,

   "{" => Token::L_CURL,

   "}" => Token::R_CURL,

   "*" => Token::MULT,

   "%" => Token::MOD,

   "/" => Token::DIV,

   "+" => Token::ADD,

   "-" => Token::SUB,

   "++" => Token::INCR,

   "--" => Token::DECR,

   "||" => Token::DISJ,

   "!" => Token::EXCL_NO_WS,

   ":" => Token::COLON,

   ";" => Token::SEMICOLON,

   "=" => Token::ASSIGNMENT,

   "+=" => Token::ADD_ASSIGNMENT,

   "*=" => Token::MULT_ASSIGNMENT,

   "/=" => Token::DIV_ASSIGNMENT,

   "%=" => Token::MOD_ASSIGNMENT,

   "->" => Token::ARROW,

   "=>" => Token::DOUBLE_ARROW,

   ".." => Token::RANGE,

   "::" => Token::COLON_COLON,

   ";;" => Token::DOUBLE_SEMICOLON,

   "#" => Token::HASH,

   "@" => Token::AT_NO_WS,

   "?" => Token::QUEST_NO_WS,

   "<" => Token::L_ANGLE,

   ">" => Token::R_ANGLE,

   "<=" => Token::LE,

   ">=" => Token::GE,

   "!=" => Token::EXCL_EQ,

   "!==" => Token::EXCL_EQ_EQ,

   "==" => Token::EQ_EQ,

   "===" => Token::EQ_EQ_EQ,

   "'" => Token::SINGLE_QUOTE,

   // activates string mode
   "\"" => Token::QUOTE_OPEN,

    // activates multiline string mode
   r#"""""# => Token::TRIPLE_QUOTE_OPEN,

   // whitespaces
   "\u{0020}" => Token::WS,
   "\u{0009}" => Token::WS,
   "\u{000C}" => Token::WS,

   // new lines
   "\u{000A}" => Token::NL,
   "\u{000D}" => Token::NL,
   "\u{000A}\u{000D}" => Token::NL,
};
