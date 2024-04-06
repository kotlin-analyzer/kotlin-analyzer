use phf::phf_map;
use tokens::Token;

// Attempt 1 - do it manually

pub static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {

   "file" => Token::File,

   "field" => Token::Field,

   "property" => Token::Property,

   "get" => Token::Get,

   "set" => Token::Set,

   "receiver" => Token::Receiver,

   "param" => Token::Param,

   "setparam" => Token::SetParam,

   "delegate" => Token::Delegate,

   "package" => Token::Package,

   "import" => Token::Import,

   "class" => Token::Class,

   "interface" => Token::Interface,

   "fun" => Token::Fun,

   "object" => Token::Object,

   "val" => Token::Val,

   "var" => Token::Var,

   "typealias" => Token::TypeAlias,

   "constructor" => Token::Constructor,

   "by" => Token::By,

   "companion" => Token::Companion,

   "init" => Token::Init,

   "this" => Token::This,

   "super" => Token::Super,

   "typeof" => Token::Typeof,

   "where" => Token::Where,

   "if" => Token::If,

   "else" => Token::Else,

   "when" => Token::When,

   "try" => Token::Try,

   "catch" => Token::Catch,

   "finally" => Token::Finally,

   "for" => Token::For,

   "do" => Token::Do,

   "while" => Token::While,

   "throw" => Token::Throw,

   "return" => Token::Return,

   "continue" => Token::Continue,

   "break" => Token::Break,

   "as" => Token::As,

   "is" => Token::Is,

   "in" => Token::In,

   "out" => Token::Out,

   "dynamic" => Token::Dynamic,

   "public" => Token::Public,

   "private" => Token::Private,

   "protected" => Token::Protected,

   "internal" => Token::Internal,

   "enum" => Token::Enum,

   "sealed" => Token::Sealed,

   "annotation" => Token::Annotation,

   "data" => Token::Data,

   "inner" => Token::Inner,

   "tailrec" => Token::Tailrec,

   "operator" => Token::Operator,

   "inline" => Token::Inline,

   "infix" => Token::Infix,

   "external" => Token::External,

   "suspend" => Token::Suspend,

   "override" => Token::Override,

   "abstract" => Token::Abstract,

   "final" => Token::Final,

   "open" => Token::Open,

   "const" => Token::Const,

   "lateinit" => Token::Lateinit,

   "vararg" => Token::VarArg,

   "noinline" => Token::NoInline,

   "crossinline" => Token::CrossInline,

   "reified" => Token::Reified,

   "expect" => Token::Expect,

   "actual" => Token::Actual,

   // literals
   "null" => Token::NullLiteral,
};

pub static OPERATORS: phf::Map<&'static str, Token> = phf_map! {
   "..." => Token::Reserved,

   "." => Token::Dot,

   "," => Token::Comma,

   "(" => Token::LParen,

   ")" => Token::RParen,

   "[" => Token::LSquare,

   "]" => Token::RSquare,

   "{" => Token::LCurl,

   "}" => Token::RCurl,

   "*" => Token::Mult,

   "%" => Token::Mod,

   "/" => Token::Div,

   "+" => Token::Add,

   "-" => Token::Sub,

   "++" => Token::Incr,

   "--" => Token::Decr,

   "||" => Token::Disj,

   "!" => Token::ExclNoWs,

   ":" => Token::Colon,

   ";" => Token::Semicolon,

   "=" => Token::Assignment,

   "+=" => Token::AddAssignment,

   "*=" => Token::MultAssignment,

   "/=" => Token::DivAssignment,

   "%=" => Token::ModAssignment,

   "->" => Token::Arrow,

   "=>" => Token::DoubleArrow,

   ".." => Token::Range,

   "::" => Token::ColonColon,

   ";;" => Token::DoubleSemicolon,

   "#" => Token::Hash,

   "@" => Token::AtNoWs,

   "?" => Token::QuestNoWs,

   "<" => Token::LAngle,

   ">" => Token::RAngle,

   "<=" => Token::LE,

   ">=" => Token::GE,

   "!=" => Token::ExclEq,

   "!==" => Token::ExclEqEq,

   "as?" => Token::AsSafe,

   "==" => Token::EqEq,

   "===" => Token::EqEqEq,

   "'" => Token::SingleQuote,

   // activates string mode
   "\"" => Token::QuoteOpen,

    // activates multiline string mode
   r#"""""# => Token::TripleQuoteOpen,

   // whitespaces
   "\u{0020}" => Token::Ws,
   "\u{0009}" => Token::Ws,
   "\u{000C}" => Token::Ws,

   // new lines
   "\u{000A}" => Token::Nl,
   "\u{000D}" => Token::Nl,
   "\u{000A}\u{000D}" => Token::Nl,
};

pub enum PrefixForComment {
    ShebangLine,
    DelimitedComment,
    LineComment,
}

pub static WHITESPACE_AND_COMMENTS: phf::Map<&'static str, PrefixForComment> = phf_map! {
    "#!" => PrefixForComment::ShebangLine,
    "/*" => PrefixForComment::DelimitedComment,
    "//" => PrefixForComment::DelimitedComment,
};

// TODO: ensure we factore fieldRef
pub static STRING_MODE: phf::Map<&'static str, Token> = phf_map! {
    "${" => Token::LineStrExprStart,
     // deactivates string mode
    "\"" => Token::QuoteClose,
};

// TODO: ensure we factore fieldRef
pub static MULTILINE_STRING_MODE: phf::Map<&'static str, Token> = phf_map! {
    "${" => Token::MultiStrExprStart,
     // deactivates multiline string mode
   r#"""""# => Token::TripleQuoteClose,
};
