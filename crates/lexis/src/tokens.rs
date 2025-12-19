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
#[define(IDENTIFIER = (('_' | $xid_start) $xid_continue*) | ('`' ^['\n', '\r', '`']+ "`" ))]
// Escapes
#[define(ESCAPE_IDENTIFIER = '\\' ('"' | '\'' | '\\' | 'n' | 'r' | 't' | 'b' | '$' ))]
#[define(ESCAPE_SEQ = UNI_CHARACTER_LITERAL | ESCAPE_IDENTIFIER)]
#[lookback(2)]
#[repr(u8)]
/// List of Kotlin defined tokens, that we cannot tokenise and moved to parsing phase =>
/// ShebangLine, LineComment, DelimitedComment, ThisAt, SuperAt.

/// List of Kotlin defined tokens that we chose to ignore because they are covered by a token variant
/// and trivia in parsing phase => ExclWs, QuestWs.

/// List of Kotlin defined tokens that we chose to ignore because they are covered by a token variants
/// and trivia with a combination of optional newline in parsing phase => AtPostWs, AtPreWs

/// List of Kotlin defined tokens that we ignored becuase they are not used at all => AtBothWs.
/// IdentifierOrSoftKey is covered by SimpleIdentifier node
pub enum KotlinToken {
    EOI = 0,
    MisMatch = 1,

    #[rule(['\u{0020}', '\u{0009}', '\u{000C}'])]
    Whitespace,

    #[rule('\n' | '\r' '\n'?)]
    NL,

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
    ColonColon,

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
    ExclEqEq,

    #[rule("as?")]
    AsSafe,

    #[rule("==")]
    EqEq,

    #[rule("===")]
    EqEqEq,

    #[rule("\"")]
    SingleQuote,

    #[rule("&")]
    Amp,

    // SECTION: keywords
    #[rule("file")]
    #[priority(10)]
    File,

    #[rule("field")]
    #[priority(10)]
    Field,

    #[rule("property")]
    #[priority(10)]
    Property,

    #[rule("get")]
    #[priority(10)]
    Get,

    #[rule("set")]
    #[priority(10)]
    Set,

    #[rule("receiver")]
    #[priority(10)]
    Receiver,

    #[rule("param")]
    #[priority(10)]
    Param,

    #[rule("@field")]
    #[priority(10)]
    AtField,

    #[rule("@property")]
    #[priority(10)]
    AtProperty,

    #[rule("@get")]
    #[priority(10)]
    AtGet,

    #[rule("@set")]
    #[priority(10)]
    AtSet,

    #[rule("@receiver")]
    #[priority(10)]
    AtReceiver,

    #[rule("@param")]
    #[priority(10)]
    AtParam,

    #[rule("@setparam")]
    #[priority(10)]
    AtSetparam,

    #[rule("@delegate")]
    #[priority(10)]
    AtDelegate,

    #[rule("setparam")]
    #[priority(10)]
    Setparam,

    #[rule("delegate")]
    #[priority(10)]
    Delegate,

    #[rule("package")]
    #[priority(10)]
    Package,

    #[rule("import")]
    #[priority(10)]
    Import,

    #[rule("class")]
    #[priority(10)]
    Class,

    #[rule("interface")]
    #[priority(10)]
    Interface,

    #[rule("fun")]
    #[priority(10)]
    Fun,

    #[rule("object")]
    #[priority(10)]
    Object,

    #[rule("val")]
    #[priority(10)]
    Val,

    #[rule("var")]
    #[priority(10)]
    Var,

    #[rule("typealias")]
    #[priority(10)]
    TypeAlias,

    #[rule("constructor")]
    #[priority(10)]
    Constructor,

    #[rule("by")]
    #[priority(10)]
    By,

    #[rule("companion")]
    #[priority(10)]
    Companion,

    #[rule("init")]
    #[priority(10)]
    Init,

    #[rule("this")]
    #[priority(10)]
    This,

    #[rule("this@" IDENTIFIER)]
    #[priority(10)]
    ThisAt,

    #[rule("super")]
    #[priority(10)]
    Super,

    #[rule("super@" IDENTIFIER)]
    #[priority(10)]
    SuperAt,

    #[rule("typeof")]
    #[priority(10)]
    Typeof,

    #[rule("where")]
    #[priority(10)]
    Where,

    #[rule("if")]
    #[priority(10)]
    If,

    #[rule("else")]
    #[priority(10)]
    Else,

    #[rule("when")]
    #[priority(10)]
    When,

    #[rule("try")]
    #[priority(10)]
    Try,

    #[rule("catch")]
    #[priority(10)]
    Catch,

    #[rule("finally")]
    #[priority(10)]
    Finally,

    #[rule("for")]
    #[priority(10)]
    For,

    #[rule("do")]
    #[priority(10)]
    Do,

    #[rule("while")]
    #[priority(10)]
    While,

    #[rule("throw")]
    #[priority(10)]
    Throw,

    #[rule("return")]
    #[priority(10)]
    Return,

    #[rule("return@" IDENTIFIER)]
    #[priority(10)]
    ReturnAt,

    #[rule("continue")]
    #[priority(10)]
    Continue,

    #[rule("continue@" IDENTIFIER)]
    #[priority(10)]
    ContinueAt,

    #[rule("break")]
    #[priority(10)]
    Break,

    #[rule("break@" IDENTIFIER)]
    #[priority(10)]
    BreakAt,

    #[rule("as")]
    #[priority(10)]
    As,

    #[rule("is")]
    #[priority(10)]
    Is,

    #[rule("!is")]
    #[priority(10)]
    NotIs,

    #[rule("in")]
    #[priority(10)]
    In,

    #[rule("!in")]
    #[priority(10)]
    NotIn,

    #[rule("out")]
    #[priority(10)]
    Out,

    #[rule("dynamic")]
    #[priority(10)]
    Dynamic,

    // SECTION: lexicalModifiers
    #[rule("public")]
    #[priority(10)]
    Public,

    #[rule("private")]
    #[priority(10)]
    Private,

    #[rule("protected")]
    #[priority(10)]
    Protected,

    #[rule("internal")]
    #[priority(10)]
    Internal,

    #[rule("enum")]
    #[priority(10)]
    Enum,

    #[rule("sealed")]
    #[priority(10)]
    Sealed,

    #[rule("annotation")]
    #[priority(10)]
    Annotation,

    #[rule("data")]
    #[priority(10)]
    Data,

    #[rule("inner")]
    #[priority(10)]
    Inner,

    #[rule("value")]
    #[priority(10)]
    Value,

    #[rule("tailrec")]
    #[priority(10)]
    Tailrec,

    #[rule("operator")]
    #[priority(10)]
    Operator,

    #[rule("inline")]
    #[priority(10)]
    Inline,

    #[rule("infix")]
    #[priority(10)]
    Infix,

    #[rule("external")]
    #[priority(10)]
    External,

    #[rule("suspend")]
    #[priority(10)]
    Suspend,

    #[rule("override")]
    #[priority(10)]
    Override,

    #[rule("abstract")]
    #[priority(10)]
    Abstract,

    #[rule("final")]
    #[priority(10)]
    Final,

    #[rule("open")]
    #[priority(10)]
    Open,

    #[rule("const")]
    #[priority(10)]
    Const,

    #[rule("lateinit")]
    #[priority(10)]
    Lateinit,

    #[rule("vararg")]
    #[priority(10)]
    Vararg,

    #[rule("noinline")]
    #[priority(10)]
    Noinline,

    #[rule("crossinline")]
    #[priority(10)]
    Crossinline,

    #[rule("reified")]
    #[priority(10)]
    Reified,

    #[rule("expect")]
    #[priority(10)]
    Expect,

    #[rule("actual")]
    #[priority(10)]
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
    // #[priority(5)]
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
    #[priority(10)]
    BooleanLiteral,

    #[rule("null")]
    #[priority(10)]
    NullLiteral,

    #[rule('\'' (ESCAPE_SEQ | ^['\r', '\n', '\'', '\\']) '\'')]
    CharacterLiteral,

    // TODO: implement in parsing phase

    // Section: Strings
    // NEEDED FOR STRING SYNTAX
    #[rule('\\')]
    Escape,

    #[rule('$')]
    Dollar,

    /// Matches `EscapedIdentifier| UniCharacterLiteral` inside a line string
    #[rule(ESCAPE_SEQ)]
    LineStrEscapedChar,

    #[rule("${")]
    StrExprStart,

    #[rule("\"\"\"")]
    TripleQuote,

    /// TODO: add tests to ensure all supported tokens are allowed
    /// TODO: add diagonistics for unsupported chars that are captured by this
    #[priority(0)]
    #[rule(IDENTIFIER)]
    Identifier,

    // Section: Used only to help implementing custom parsers
    /// For implementing Line comments
    #[rule("//")]
    LineCommentStart,

    /// For implementing delimited comments
    #[rule("/*")]
    DelimitedCommentStart,

    /// For implementing delimited comments
    #[rule("*/")]
    DelimitedCommentEnd,

    /// For implementing SehbangLine comments
    #[rule("#!")]
    ShebangLineStart,
}

// TODO: conisder removing if not used
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
    // strong keywords
    KotlinToken::Const as u8,
    KotlinToken::Suspend as u8,
]);
