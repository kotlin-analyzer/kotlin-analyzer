//! Source: https://kotlinlang.org/spec/syntax-and-grammar.html#tokens

use logos::Logos;

/// Tokenizer generated by logos using matching string tokens and regular expressions
///! Try as much as possible to not use "priority" here when adding a new variant,
///! it usually shows that something is wrong
#[derive(Debug, Logos, PartialEq)]
pub enum Token {
    ///! White Space and Comments

    #[regex(r"#![^\u{000A}\u{000D}]*")]
    ShebangLine,

    #[regex(r"/\*[^\*/]*\*/")]
    DelimitedComment,

    #[regex(r"//[^\u{000A}\u{000D}]*")]
    LineComment,

    #[regex(r"\u{0020}|\u{0009}|\u{000C}")]
    Ws,

    #[regex(r"\u{000A}|(?:\u{000D}\u{000A}?)")]
    Nl,

    ///! Keywords and operators

    #[token("...")]
    Reserved,

    #[token(".")]
    Dot,

    #[token(",")]
    Comma,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LSquare,

    #[token("]")]
    RSquare,

    #[token("{")]
    LCurl,

    #[token("}")]
    RCurl,

    #[token("*")]
    Mult,

    #[token("%")]
    Mod,

    #[token("/")]
    Div,

    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("++")]
    Incr,

    #[token("--")]
    Decr,

    #[token("||")]
    Disj,

    #[regex(r#"!(?:/\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|[\u{0020}\u{0009}\u{000C}])"#)]
    ExclWs,

    #[token("!")]
    ExclNoWs,

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,

    #[token("=")]
    Assignment,

    #[token("+=")]
    AddAssignment,

    #[token("*=")]
    MultAssignment,

    #[token("/=")]
    DivAssignment,

    #[token("%=")]
    ModAssignment,

    #[token("->")]
    Arrow,

    #[token("=>")]
    DoubleArrow,

    #[token("..")]
    Range,

    #[token("::")]
    ColonColon,

    #[token(";;")]
    DoubleSemicolon,

    #[token("#")]
    Hash,

    #[token("@")]
    AtNoWs,

    #[regex(r#"@(?:\u{000A}|(?:\u{000D}\u{000A}?)|/\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|[\u{0020}\u{0009}\u{000C}])"#)]
    AtPostWs,

    #[regex(r#"(?:\u{000A}|(?:\u{000D}\u{000A}?)|/\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|[\u{0020}\u{0009}\u{000C}])@"#, priority = 3)]
    AtPreWs,

    #[regex(r#"(?:\u{000A}|(?:\u{000D}\u{000A}?)|/\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|[\u{0020}\u{0009}\u{000C}])@(?:\u{000A}|(?:\u{000D}\u{000A}?)|/\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|[\u{0020}\u{0009}\u{000C}])"#)]
    AtBothWs,

    #[regex(r#"\?(?:\u{000A}|(?:\u{000D}\u{000A}?)|/\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|[\u{0020}\u{0009}\u{000C}])"#)]
    QuestWs,

    #[token("?")]
    QuestNoWs,

    #[token("<")]
    LAngle,

    #[token(">")]
    RAngle,

    #[token("<=")]
    LE,

    #[token(">=")]
    GE,

    #[token("!=")]
    ExclEq,

    #[token("!==")]
    ExclEqEq,

    #[token("as?")]
    AsSafe,

    #[token("==")]
    EqEq,

    #[token("===")]
    EqEqEq,

    #[token("'")]
    SingleQuote,

    #[regex(r"return@(?:[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_][\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_\p{Nd}]*|`[^\u{000A}\u{000D}`]*`)")]
    ReturnAt,

    #[regex(r"continue@[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_][\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_\p{Nd}]*|`[^\u{000A}\u{000D}`]*`")]
    ContinueAt,

    #[regex(r"break@(?:[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_][\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_\p{Nd}]*|`[^\u{000A}\u{000D}`]*`)")]
    BreakAt,

    #[regex(r"this@(?:[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_][\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_\p{Nd}]*|`[^\u{000A}\u{000D}`]*`)")]
    ThisAt,

    #[regex(r"super@(?:[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_][\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_\p{Nd}]*|`[^\u{000A}\u{000D}`]*`)")]
    SuperAt,

    #[token("file")]
    File,

    #[token("field")]
    Field,

    #[token("property")]
    Property,

    #[token("get")]
    Get,

    #[token("set")]
    Set,

    #[token("receiver")]
    Receiver,

    #[token("param")]
    Param,

    #[token("setparam")]
    SetParam,

    #[token("delegate")]
    Delegate,

    #[token("package")]
    Package,

    #[token("import")]
    Import,

    #[token("class")]
    Class,

    #[token("interface")]
    Interface,

    #[token("fun")]
    Fun,

    #[token("object")]
    Object,

    #[token("val")]
    Val,

    #[token("var")]
    Var,

    #[token("typealias")]
    TypeAlias,

    #[token("constructor")]
    Constructor,

    #[token("by")]
    By,

    #[token("companion")]
    Companion,

    #[token("init")]
    Init,

    #[token("this")]
    This,

    #[token("super")]
    Super,

    #[token("typeof")]
    Typeof,

    #[token("where")]
    Where,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("when")]
    When,

    #[token("try")]
    Try,

    #[token("catch")]
    Catch,

    #[token("finally")]
    Finally,

    #[token("for")]
    For,

    #[token("do")]
    Do,

    #[token("while")]
    While,

    #[token("throw")]
    Throw,

    #[token("return")]
    Return,

    #[token("continue")]
    Continue,

    #[token("break")]
    Break,

    #[token("as")]
    As,

    #[token("is")]
    Is,

    #[token("in")]
    In,

    #[regex(r#"!is(?:\u{000A}|(?:\u{000D}\u{000A}?)|/\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|[\u{0020}\u{0009}\u{000C}])"#)]
    NotIs,

    #[regex(r#"!in(?:\u{000A}|(?:\u{000D}\u{000A}?)|/\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|[\u{0020}\u{0009}\u{000C}])"#)]
    NotIn,

    #[token("out")]
    Out,

    #[token("dynamic")]
    Dynamic,

    #[token("public")]
    Public,

    #[token("private")]
    Private,

    #[token("protected")]
    Protected,

    #[token("internal")]
    Internal,

    #[token("enum")]
    Enum,

    #[token("sealed")]
    Sealed,

    #[token("annotation")]
    Annotation,

    #[token("data")]
    Data,

    #[token("inner")]
    Inner,

    #[token("tailrec")]
    Tailrec,

    #[token("operator")]
    Operator,

    #[token("inline")]
    Inline,

    #[token("infix")]
    Infix,

    #[token("external")]
    External,

    #[token("suspend")]
    Suspend,

    #[token("override")]
    Override,

    #[token("abstract")]
    Abstract,

    #[token("final")]
    Final,

    #[token("open")]
    Open,

    #[token("const")]
    Const,

    #[token("lateinit")]
    Lateinit,

    #[token("vararg")]
    VarArg,

    #[token("noinline")]
    NoInline,

    #[token("crossinline")]
    CrossInline,

    #[token("reified")]
    Reified,

    #[token("expect")]
    Expect,

    #[token("actual")]
    Actual,

    //# Literals
    #[regex(r"[1-9](?:\d|_)*\d|\d")]
    IntegerLiteral,

    #[regex(r"(?:(?:\d[\d_]*\d|\d)?\.(?:\d[\d_]*\d|\d)(:?[eE][-+]?(?:\d[\d_]*\d|\d))?|(?:\d[\d_]*\d|\d)(:?[eE][-+]?(?:\d[\d_]*\d|\d))?)[Ff]")]
    RealLiteral,

    #[regex(r"0[xX](?:[0-9A-Fa-f](?:[0-9A-Fa-f]|_)*[0-9A-Fa-f]|[0-9A-Fa-f])")]
    HexLiteral,

    #[regex(r"0[bB](?:[01](?:[01]|_)*[01]|[01])")]
    BinLiteral,

    #[regex(r"(?:0[bB](?:[01](?:[01]|_)*[01]|[01])|0[xX](?:[0-9A-Fa-f](?:[0-9A-Fa-f]|_)*[0-9A-Fa-f]|[0-9A-Fa-f])|[1-9](?:\d|_)*\d|\d)L")]
    LongLiteral,

    #[regex("true|false")]
    BooleanLiteral,

    #[token("null")]
    NullLiteral,

    #[regex(r#"'(?:\\u[0-9A-Fa-f]{4}|\\[tbrn\\'"$]|[^'\\\u{000A}\u{000D}])'"#)]
    CharacterLiteral,

    /// Serves as both opening and closing quote, we do not disambiguate at this stage
    #[token("\"")]
    Quote,

    /// Serves as both opening and closing triple quote, we do not disambiguate at this stage
    #[token(r#"""""#)]
    TripleQuote,

    #[regex(r"(?:[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_][\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_\p{Nd}]*|`[^\u{000A}\u{000D}`]*`)")]
    Identifier,

    /// An identifier preceded by $. Serves as both LineStrRef and MultiLineStrRef, we do not disambiguate at this stage
    #[regex(r"\$(?:[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_][\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_\p{Nd}]*|`[^\u{000A}\u{000D}`]*`)")]
    StrRef,

    // FIXME: we should not distinguish between multi and single line in this phase
    // #[regex(r#"[^\\"$]+|\$"#, priority = 1)]
    // LineStrText,

    // #[regex(r#"[^"$]+|\$"#, priority = 0)]
    // MultiLineStrText,

    // #[regex(r#"\\u[0-9A-Fa-f]{4}|\\[tbrn\\'"$]"#)]
    // LineStrEscapedChar,
    /// Serves as both LineStrExprStart and MultiStrExprStart, we do not disambiguate at this stage
    #[token("${")]
    StrExprStart,
}

#[cfg(test)]
mod test {
    use std::error::Error;

    use super::*;

    #[test]
    fn simple() -> Result<(), Box<dyn Error>> {
        let lex = Token::lexer(
            r#"
[],--
/* comments */
//line comment
#! sh echo "hey"
hey
"#,
        )
        .spanned();
        for lex in lex {
            println!("{:?}", lex);
        }
        Ok(())
    }
}
