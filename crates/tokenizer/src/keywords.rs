//! Source: https://kotlinlang.org/spec/syntax-and-grammar.html#tokens

use logos::Logos;

// DelimitedComment => /\*[^\*/]*\*/
// LineComment => //[^\u{000A}\u{000D}]*
// Hidden => /\*[^\*/]*\*/|//[^\u{000A}\u{000D}]*|\u{0020}|\u{0009}|\u{000C}
// Nl => \u{000A}|(\u{000D}\u{000A}?)

#[derive(Debug, Logos, PartialEq)]
pub enum Token {
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
"#,
        );
        for lex in lex {
            println!("{:?}", lex);
        }
        Ok(())
    }
}
