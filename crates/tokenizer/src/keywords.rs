//! Source: https://kotlinlang.org/spec/syntax-and-grammar.html#tokens

use logos::Logos;

// LF => \u{000A}
// CR => \u{000D}
// DelimitedComment => /\*[^\*/]*\*/
// LineComment => //[^\u{000A}\u{000D}]*
// Hidden => /\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|\u{0020}|\u{0009}|\u{000C}
// Nl => \u{000A}|(\u{000D}\u{000A}?)
// Letters => p{Lu}|p{Ll}|p{Lt}|p{Lm}|p{Lo}
// QuotedSymbol => [^LFCF`]
// UnicodeDigit => p{Nd}
// Identifier => (Letter | '_') {Letter | '_' | UnicodeDigit}| '`' QuotedSymbol {QuotedSymbol} '`'
// (p{Lu}|p{Ll}|p{Lt}|p{Lm}|p{Lo}|_)(p{Lu}|p{Ll}|p{Lt}|p{Lm}|p{Lo}|_|p{Nd})*?|`[^\u{000A}\u{000D}`]`

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

    #[regex(r"\u{000A}|(\u{000D}\u{000A}?)")]
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

    //  todo: add hidden after
    // #[token("!")]
    // ExclWs,
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

    // todo add Hidden|NL after
    // #[token("@")]
    // AtPostWs,

    // todo add Hidden|NL before
    // #[token("@")]
    // AtPreWs,

    // todo add Hidden|NL before and after
    // #[token("@")]
    // AtBothWs,

    // todo add Hidden after
    // #[token("?")]
    // QuestWs,
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

    // todo add identifier after
    #[token("return@")]
    ReturnAt,

    // todo add identifier after
    #[token("continue@")]
    ContinueAt,

    // todo add identifier after
    #[token("break@")]
    BreakAt,

    // todo add identifier after
    #[token("this@")]
    ThisAt,

    // todo add identifier after
    #[token("super@")]
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

    //todo: add hidden|NL after
    #[token("!is")]
    NotIs,

    //todo: add hidden|NL after
    #[token("!in")]
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

    #[regex(r"(?:\p{Lu}|\p{Ll}|\p{Lt}|\p{Lm}|\p{Lo}|_)(?:\p{Lu}|\p{Ll}|\p{Lt}|\p{Lm}|\p{Lo}|_|\p{Nd})*|`[^\u{000A}\u{000D}`]*`")]
    Identifier,
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
        );
        for lex in lex {
            println!("{:?}", lex);
        }
        Ok(())
    }
}
