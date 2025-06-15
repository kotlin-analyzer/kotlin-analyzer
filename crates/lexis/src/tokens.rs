use lady_deirdre::lexis::{Token, TokenSet};

#[cfg(test)]
mod tests;

#[derive(Token, Clone, Copy, PartialEq, Eq, Debug)]
// Numbers
#[define(DEC_DIGIT = ['0'..'9'])]
#[define(DEC_DIGIT_NO_ZERO = ['1'..'9'])]
#[define(DEC_DIGIT_OR_SEPARATOR = DEC_DIGIT | '_')]
#[define(DEC_DIGITS =  DEC_DIGIT DEC_DIGIT_OR_SEPARATOR* DEC_DIGIT | DEC_DIGIT)]
#[define(DOUBLE_EXPONENT = ['e', 'E'] ['+', '-']? DEC_DIGITS)]
#[define(INTEGER_LITERAL = DEC_DIGIT_NO_ZERO DEC_DIGIT_OR_SEPARATOR* DEC_DIGIT | DEC_DIGIT)]
#[define(DOUBLE_LITERAL = DEC_DIGITS? '.' DEC_DIGITS DOUBLE_EXPONENT? | DEC_DIGITS DOUBLE_EXPONENT)]
#[define(FLOAT_LITERAL = DOUBLE_LITERAL ['f', 'F'] | DEC_DIGITS ['f', 'F'])]
// Hex
#[define(HexDigit = ['A'..'F', 'a'..'f', '0'..'9'])]
#[define(HexDigitOrSeparator = HexDigit | '_')]
#[define(HEX_LITERAL = '0' ['x', 'X'] HexDigit HexDigitOrSeparator* HexDigit | '0' ['x', 'X'] HexDigit)]
// Bin
#[define(BinDigit = ['0', '1'])]
#[define(BinDigitOrSeparator = BinDigit | '_')]
#[define(BIN_LITERAL = '0' ['b', 'B'] BinDigit BinDigitOrSeparator* BinDigit | '0' ['b', 'B'] BinDigit)]
// Char
#[define(UNI_CHARACTER_LITERAL = '\\' 'u' HexDigit HexDigit HexDigit HexDigit)]
// Escapes
#[define(ESCAPE_IDENTIFIER = '\\' ('"' | '\'' | '\\' | 'n' | 'r' | 't' | 'b' | '$' ))]
#[define(ESCAPE_SEQ = UNI_CHARACTER_LITERAL | ESCAPE_IDENTIFIER)]
#[lookback(2)]
#[repr(u8)]
pub enum KotlinToken {
    EOI = 0,
    MisMatch = 1,

    #[rule(['\u{0020}', '\u{0009}', '\u{000C}'])]
    Whitespace,

    #[rule('\n' | '\r' '\n'?)]
    Newline,

    // SECTION: separatorsAndOperations
    #[rule("...")]
    Reserved,

    #[rule(".")]
    Dot,

    #[rule(",")]
    Comma,

    #[rule("(")]
    LParen,

    #[rule(")")]
    RParen,

    #[rule("[")]
    LSquare,

    #[rule("]")]
    RSquare,

    #[rule("{")]
    LCurl,

    #[rule("}")]
    RCurl,

    #[rule("*")]
    Mult,

    #[rule("%")]
    Mod,

    #[rule("/")]
    Div,

    #[rule("+")]
    Add,

    #[rule("-")]
    Sub,

    #[rule("++")]
    Incr,

    #[rule("--")]
    Decr,

    #[rule("&&")]
    Conj,

    #[rule("||")]
    Disj,

    #[rule("!")]
    ExclNoWs,

    #[rule(":")]
    Colon,

    #[rule(";")]
    Semicolon,

    #[rule("=")]
    Assignment,

    #[rule("+=")]
    AddAssignment,

    #[rule("-=")]
    SubAssignment,

    #[rule("*=")]
    MultAssignment,

    #[rule("/=")]
    DivAssignment,

    #[rule("%=")]
    ModAssignment,

    #[rule("->")]
    Arrow,

    #[rule("=>")]
    DoubleArrow,

    #[rule("..")]
    Range,

    #[rule("..<")]
    RangeUntil,

    #[rule("::")]
    Coloncolon,

    #[rule(";;")]
    DoubleSemicolon,

    #[rule("#")]
    Hash,

    #[rule("@")]
    AtNoWs,

    #[rule("?")]
    QuestNoWs,

    #[rule("<")]
    LAngle,

    #[rule(">")]
    RAngle,

    #[rule("<=")]
    Le,

    #[rule(">=")]
    Ge,

    #[rule("!=")]
    ExclEq,

    #[rule("!==")]
    ExclEqeq,

    #[rule("as?")]
    AsSafe,

    #[rule("==")]
    Eqeq,

    #[rule("===")]
    Eqeqeq,

    #[rule("\"")]
    SingleQuote,

    #[rule("&")]
    Amp,

    // SECTION: keywords
    #[rule("file")]
    File,

    #[rule("field")]
    Field,

    #[rule("property")]
    Property,

    #[rule("get")]
    Get,

    #[rule("set")]
    Set,

    #[rule("receiver")]
    Receiver,

    #[rule("param")]
    Param,

    #[rule("setparam")]
    Setparam,

    #[rule("delegate")]
    Delegate,

    #[rule("package")]
    Package,

    #[rule("import")]
    Import,

    #[rule("class")]
    Class,

    #[rule("interface")]
    Interface,

    #[rule("fun")]
    Fun,

    #[rule("object")]
    Object,

    #[rule("val")]
    Val,

    #[rule("var")]
    Var,

    #[rule("typealias")]
    TypeAlias,

    #[rule("constructor")]
    Constructor,

    #[rule("by")]
    By,

    #[rule("companion")]
    Companion,

    #[rule("init")]
    Init,

    #[rule("this")]
    This,

    #[rule("super")]
    Super,

    #[rule("typeof")]
    Typeof,

    #[rule("where")]
    Where,

    #[rule("if")]
    If,

    #[rule("else")]
    Else,

    #[rule("when")]
    When,

    #[rule("try")]
    Try,

    #[rule("catch")]
    Catch,

    #[rule("finally")]
    Finally,

    #[rule("for")]
    For,

    #[rule("do")]
    Do,

    #[rule("while")]
    While,

    #[rule("throw")]
    Throw,

    #[rule("return")]
    Return,

    #[rule("continue")]
    Continue,

    #[rule("break")]
    Break,

    #[rule("as")]
    As,

    #[rule("is")]
    Is,

    #[rule("in")]
    In,

    #[rule("out")]
    Out,

    #[rule("dynamic")]
    Dynamic,

    // SECTION: lexicalModifiers
    #[rule("public")]
    Public,

    #[rule("private")]
    Private,

    #[rule("protected")]
    Protected,

    #[rule("internal")]
    Internal,

    #[rule("enum")]
    Enum,

    #[rule("sealed")]
    Sealed,

    #[rule("annotation")]
    Annotation,

    #[rule("data")]
    Data,

    #[rule("inner")]
    Inner,

    #[rule("value")]
    Value,

    #[rule("tailrec")]
    Tailrec,

    #[rule("operator")]
    Operator,

    #[rule("inline")]
    Inline,

    #[rule("infix")]
    Infix,

    #[rule("external")]
    External,

    #[rule("suspend")]
    Suspend,

    #[rule("override")]
    Override,

    #[rule("abstract")]
    Abstract,

    #[rule("final")]
    Final,

    #[rule("open")]
    Open,

    #[rule("const")]
    Const,

    #[rule("lateinit")]
    Lateinit,

    #[rule("vararg")]
    Vararg,

    #[rule("noinline")]
    Noinline,

    #[rule("crossinline")]
    Crossinline,

    #[rule("reified")]
    Reified,

    #[rule("expect")]
    Expect,

    #[rule("actual")]
    Actual,

    // SECTION: literals
    #[rule(INTEGER_LITERAL)]
    IntegerLiteral,

    #[rule(DOUBLE_LITERAL)]
    DoubleLiteral,

    #[rule(DOUBLE_LITERAL ['f', 'F'] | DEC_DIGITS ['f', 'F'])]
    FloatLiteral,

    // Ambiguous, so ignored
    // #[rule(FLOAT_LITERAL | DOUBLE_LITERAL)]
    // RealLiteral,
    #[rule(HEX_LITERAL)]
    HexLiteral,

    #[rule(BIN_LITERAL)]
    BinLiteral,

    #[rule((INTEGER_LITERAL | HEX_LITERAL | BIN_LITERAL) ['u', 'U'] ['l', 'L']?)]
    UnsignedLiteral,

    #[rule((INTEGER_LITERAL | HEX_LITERAL | BIN_LITERAL) ['l', 'L'])]
    LongLiteral,

    #[rule("true" | "false")]
    BoolLiteral,

    #[rule("null")]
    NullLiteral,

    #[rule('\'' (ESCAPE_SEQ | ^['\r', '\n', '\'', '\\']) '\'')]
    CharLiteral,

    // List of Kotlin defined tokens, that we cannot tokenise and moved to parsing phase
    // ShebangLine, LineComment, DelimitedComment

    // TODO: implement in parsing phase

    // separators and operations with Hidden
    /// Matches `"@" (Hidden | NL)`
    /// Parsed in the syntax grammar phase because it depends on Hidden
    AtPostWs,

    /// Matches `(Hidden | NL) "@"`
    /// Parsed in the syntax grammar phase because it depends on Hidden
    AtPreWs,

    /// Matches `(Hidden | NL) "@" (Hidden | NL)`
    /// Parsed in the syntax grammar phase because it depends on Hidden
    AtBothWs,

    /// Matches `"!" Hidden`
    /// Parsed in the syntax grammar phase because it depends on Hidden
    ExclWs,

    /// Matches `"!is" (Hidden | NL)`
    /// Parsed in the syntax grammar phase because it depends on Hidden
    NotIs,

    /// Matches `"!in" (Hidden | NL)`
    /// Parsed in the syntax grammar phase because it depends on Hidden
    NotIn,

    /// Matches `"?" Hidden`
    /// Parsed in the syntax grammar phase because it depends on Hidden
    QuestWs,

    // Keywords with Identifier
    /// Matches `"return@" Identifier`
    /// Parsed in the syntax grammar phase because it depends on Identifier
    ReturnAt,

    /// Matches `"continue@" Identifier`
    /// Parsed in the syntax grammar phase because it depends on Identifier
    ContinueAt,

    /// Matches `"break@" Identifier`
    /// Parsed in the syntax grammar phase because it depends on Identifier
    BreakAt,

    /// Matches `"this@" Identifier`
    /// Parsed in the syntax grammar phase because it depends on Identifier
    ThisAt,

    /// Matches `"super@" Identifier`
    /// Parsed in the syntax grammar phase because it depends on Identifier
    SuperAt,

    // Section: Identifiers
    /// Matches `(Letter | '_') (Letter | '_' | UnicodeDigit)*``
    /// | '`' ~([\r\n] | '`')+ '`'
    /// Parsed in the syntax grammar phase
    Identifier,

    /// Matches `Identifier` or any of the soft keywords or hard keywords
    /// Parsed in the syntax grammar phase
    IdentifierOrSoftKey,

    /// Matches `'$' IdentifierOrSoftKey`
    /// Parsed in the syntax grammar phase
    FieldIdentifier,

    // Section: Strings
    /// Matches `"` at the beginning of a line string
    /// Parsed in the syntax grammar phase as it conflict with [Self::QuoteClose] & [Self::MultiLineStringQuote]
    QuoteOpen,

    /// Matches `"` at the end of a line string
    /// Parsed in the syntax grammar phase as it conflict with [Self::QuoteOpen] & [Self::MultiLineStringQuote]
    QuoteClose,

    /// Matches `FieldIdentifier` inside a line string
    LineStrRef,

    /// Matches `~('\\' | '"' | '$')+ | '$'` inside a line string
    LineStrText,

    /// Matches `EscapedIdentifier| UniCharacterLiteral` inside a line string
    LineStrEscapedChar,

    /// Matches `${` inside a line string
    LineStrExprStart,

    /// Matches `"""` at the beginning of a multiline string
    TripleQuoteOpen,

    /// Matches `MultiLineStringQuote? """` at the close of a multiline string
    TripleQuoteClose,

    /// Matches `"+` inside a multiline string
    /// Parsed in the syntax grammar phase as it conflict with [Self::QuoteOpen] & [Self::QuoteClose]
    MultiLineStringQuote,

    /// Matches `FieldIdentifier` inside a line string
    MultiLineStrRef,

    /// Matches `~('"' | '$')+ | '$'` inside a line string
    MultiLineStrText,

    /// Matches `${` inside a line string
    MultiLineStrExprStart,

    // Section: Used only to help implementing custom parsers
    /// For implementing Line comments
    #[rule("//")]
    LineCommentStart,

    /// For implementing Line comments
    #[rule("/*")]
    DelimitedCommentStart,
}

pub static SOFT_KEYWORDS: TokenSet = TokenSet::inclusive(&[
    KotlinToken::Abstract as u8,
    KotlinToken::Annotation as u8,
    KotlinToken::By as u8,
    KotlinToken::Catch as u8,
    KotlinToken::Companion as u8,
    KotlinToken::Constructor as u8,
    KotlinToken::Crossinline as u8,
    KotlinToken::Data as u8,
    KotlinToken::Dynamic as u8,
    KotlinToken::Enum as u8,
    KotlinToken::External as u8,
    KotlinToken::Final as u8,
    KotlinToken::Finally as u8,
    KotlinToken::Import as u8,
    KotlinToken::Infix as u8,
    KotlinToken::Init as u8,
    KotlinToken::Inline as u8,
    KotlinToken::Inner as u8,
    KotlinToken::Internal as u8,
    KotlinToken::Lateinit as u8,
    KotlinToken::Noinline as u8,
    KotlinToken::Open as u8,
    KotlinToken::Operator as u8,
    KotlinToken::Out as u8,
    KotlinToken::Override as u8,
    KotlinToken::Private as u8,
    KotlinToken::Protected as u8,
    KotlinToken::Public as u8,
    KotlinToken::Reified as u8,
    KotlinToken::Sealed as u8,
    KotlinToken::Tailrec as u8,
    KotlinToken::Vararg as u8,
    KotlinToken::Where as u8,
    KotlinToken::Get as u8,
    KotlinToken::Set as u8,
    KotlinToken::Field as u8,
    KotlinToken::Property as u8,
    KotlinToken::Receiver as u8,
    KotlinToken::Param as u8,
    KotlinToken::Setparam as u8,
    KotlinToken::Delegate as u8,
    KotlinToken::File as u8,
    KotlinToken::Expect as u8,
    KotlinToken::Actual as u8,
    KotlinToken::Value as u8,
]);

pub static HARD_KEYWORDS: TokenSet =
    TokenSet::inclusive(&[KotlinToken::Const as u8, KotlinToken::Suspend as u8]);

// TODO: add more token sets for operators, keywords, etc.
