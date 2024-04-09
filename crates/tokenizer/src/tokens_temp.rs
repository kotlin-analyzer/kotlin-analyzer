//! Source: https://kotlinlang.org/spec/syntax-and-grammar.html#tokens

use logos::Logos;

/// Tokenizer generated by logos using matching string tokens and regular expressions

#[derive(Debug, Logos, PartialEq)]
pub enum Token {
    /// An identifier preceded by $. Serves as both LineStrRef and MultiLineStrRef, we do not disambiguate at this stage
    #[regex(r"\$([\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_][\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}_\p{Nd}]*|`[^\u{000A}\u{000D}`]*`)")]
    StrRef,

    // FIXME: we should not distinguish between multi and single line in this phase
    #[regex(r#"[^\\"$]+|\$"#, priority = 1)]
    LineStrText,

    #[regex(r#"[^"$]+|\$"#, priority = 0)]
    MultiLineStrText,

    #[regex(r#"\\u[0-9A-Fa-f]{4}|\\[tbrn\\'"$]"#)]
    LineStrEscapedChar,
    // Serves as both LineStrExprStart and MultiStrExprStart, we do not disambiguate at this stage
    #[token("${")]
    StrExprStart,
}
