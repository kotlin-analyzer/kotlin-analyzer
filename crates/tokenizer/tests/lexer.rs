use macros::multiline_str;
use pretty_assertions::assert_eq;
use tokenizer::Lexer;
use tokens::Token;

#[macro_export]
macro_rules! lexer_matches {
        ($source: expr, [$($token_kind: expr => $start:literal..$end:literal),+]) => {
            let source: &str = $source;
            let mut lexer =  Lexer::new(&source).spanned();
            $(assert_eq!(
                lexer.next(),
                Some(::tokenizer::TokenInfo::new($token_kind, $start..$end))
            );)+
        };

        ($source: expr, $filter_token:path, [$($start:literal..$end:literal),+]) => {
            let source: &str = $source;
            let filter_token: Token = $filter_token;

            let mut lexer =  Lexer::new(&source)
            .spanned()
            .filter(|info| *info.token() == filter_token);

            $(assert_eq!(lexer.next(), Some(::tokenizer::TokenInfo::new($filter_token, $start..$end)));)+
        };
}

#[allow(unused_macros)]
macro_rules! dbg_lexer_src {
    ($source: expr) => {
        let source: &str = $source;
        let lexer = Lexer::new(&source).spanned_with_src();
        println!("Source of 0..{}", source.len() - 1);
        for entry in lexer {
            println!("{},", entry);
        }
    };
}

#[allow(unused_macros)]
macro_rules! dbg_lexer {
    ($source: expr) => {
        let source: &str = $source;
        let lexer = Lexer::new(&source).spanned();
        println!("Source of 0..{}", source.len() - 1);
        for entry in lexer {
            println!("{},", entry);
        }
    };
}

#[test]
fn multi_line_str_test() {
    let source = multiline_str!(
        r#""""
    simple
    """
    """
    complex ${ref}
    "line string inside multi"
    """"""
    """""""""" // open with three, 4 quotes inside and 3 closing
    "#
    );

    dbg_lexer_src!(&source);

    lexer_matches!(&source, [
        Token::TripleQuoteOpen => 0..3, // "\"\"\"",
        Token::MultiLineStrText => 3..11, // "\nsimple\n",
        Token::TripleQuoteClose => 11..14, // "\"\"\"",
        Token::Nl => 14..15, // "\n",
        Token::TripleQuoteOpen => 15..18, // "\"\"\"",
        Token::MultiLineStrText => 18..27, // "\ncomplex ",
        Token::MultiStrExprStart => 27..29, // "${",
        Token::Identifier => 29..32, // "ref",
        Token::RCurl => 32..33, // "}",
        Token::MultiLineStrText => 33..34, // "\n",
        Token::QuoteOpen => 34..35, // "\"",
        Token::LineStrText => 35..59, // "line string inside multi",
        Token::QuoteClose => 59..60, // "\"",
        Token::MultiLineStrText => 60..61, // "\n",
        Token::MultiLineStringQuote => 61..64, // "\"\"\"",
        Token::TripleQuoteClose => 64..67, // "\"\"\"",
        Token::Nl => 67..68, // "\n",
        Token::TripleQuoteOpen => 68..71, // "\"\"\"",
        Token::MultiLineStringQuote => 71..75, // "\"\"\"\"",
        Token::TripleQuoteClose => 75..78, // "\"\"\"",
        Token::Ws => 78..79, // " ",
        Token::LineComment => 79..128, // "// open with three, 4 quotes inside and 3 closing",
        Token::Nl => 128..129, // "\n",
        Token::EOF => 129..129
    ]);
}

#[test]
fn nested_str_test() {
    let source = r#""hey ${echo("test")} stranger""#;
    lexer_matches!(source, [
        Token::QuoteOpen => 0..1,
        Token::LineStrText => 1..5,
        Token::LineStrExprStart => 5..7,
        Token::Identifier => 7..11,
        Token::LParen => 11..12,
        Token::QuoteOpen => 12..13,
        Token::LineStrText => 13..17,
        Token::QuoteClose => 17..18,
        Token::RParen => 18..19,
        Token::RCurl => 19..20,
        Token::LineStrText => 20..29,
        Token::QuoteClose => 29..30,
        Token::EOF => 30..30
    ]);
}

#[test]
fn nested_multi_str_test() {
    let source = multiline_str!(
        r#"
    """
    Can multiline strings
    have ${"""
        other multiline strings
    """.trimIndent()}
    I think yes
    """
    "#
    );

    lexer_matches!(&source, [
        Token::Nl => 0..1, // "\n",
        Token::TripleQuoteOpen => 1..4, // "\"\"\"",
        Token::MultiLineStrText => 4..32, // "\nCan multiline strings\nhave ",
        Token::MultiStrExprStart => 32..34, // "${",
        Token::TripleQuoteOpen => 34..37, // "\"\"\"",
        Token::MultiLineStrText => 37..62, // "\nother multiline strings\n",
        Token::TripleQuoteClose => 62..65, // "\"\"\"",
        Token::Dot => 65..66, // ".",
        Token::Identifier => 66..76, // "trimIndent",
        Token::LParen => 76..77, // "(",
        Token::RParen => 77..78, // ")",
        Token::RCurl => 78..79, // "}",
        Token::MultiLineStrText => 79..92, // "\nI think yes\n",
        Token::TripleQuoteClose => 92..95, // "\"\"\"",
        Token::Nl => 95..96, // "\n",
        Token::EOF => 96..96
    ]);
}

#[test]
fn keyword_start() {
    let source = multiline_str!(
        r#"package dev.ikeze.kotlinsyntax

        import kotlin.streams.toList
        
        fun String.length(): Int = this.chars().toList().size
        
        fun main() {
            val len = "simple".length()
            println(len)
        }"#
    );
    lexer_matches!(&source, [
        Token::Package => 0..7, // "package",
        Token::Ws => 7..8, // " ",
        Token::Identifier => 8..11, // "dev",
        Token::Dot => 11..12, // ".",
        Token::Identifier => 12..17, // "ikeze",
        Token::Dot => 17..18, // ".",
        Token::Identifier => 18..30, // "kotlinsyntax",
        Token::Nl => 30..31, // "\n",
        Token::Nl => 31..32, // "\n",
        Token::Import => 32..38, // "import",
        Token::Ws => 38..39, // " ",
        Token::Identifier => 39..45, // "kotlin",
        Token::Dot => 45..46, // ".",
        Token::Identifier => 46..53, // "streams",
        Token::Dot => 53..54, // ".",
        Token::Identifier => 54..60, // "toList",
        Token::Nl => 60..61, // "\n",
        Token::Nl => 61..62, // "\n",
        Token::Fun => 62..65, // "fun",
        Token::Ws => 65..66, // " ",
        Token::Identifier => 66..72, // "String",
        Token::Dot => 72..73, // ".",
        Token::Identifier => 73..79, // "length",
        Token::LParen => 79..80, // "(",
        Token::RParen => 80..81, // ")",
        Token::Colon => 81..82, // ":",
        Token::Ws => 82..83, // " ",
        Token::Identifier => 83..86, // "Int",
        Token::Ws => 86..87, // " ",
        Token::Assignment => 87..88, // "=",
        Token::Ws => 88..89, // " ",
        Token::This => 89..93, // "this",
        Token::Dot => 93..94, // ".",
        Token::Identifier => 94..99, // "chars",
        Token::LParen => 99..100, // "(",
        Token::RParen => 100..101, // ")",
        Token::Dot => 101..102, // ".",
        Token::Identifier => 102..108, // "toList",
        Token::LParen => 108..109, // "(",
        Token::RParen => 109..110, // ")",
        Token::Dot => 110..111, // ".",
        Token::Identifier => 111..115, // "size",
        Token::Nl => 115..116, // "\n",
        Token::Nl => 116..117, // "\n",
        Token::Fun => 117..120, // "fun",
        Token::Ws => 120..121, // " ",
        Token::Identifier => 121..125, // "main",
        Token::LParen => 125..126, // "(",
        Token::RParen => 126..127, // ")",
        Token::Ws => 127..128, // " ",
        Token::LCurl => 128..129, // "{",
        Token::Nl => 129..130, // "\n",
        Token::Val => 130..133, // "val",
        Token::Ws => 133..134, // " ",
        Token::Identifier => 134..137, // "len",
        Token::Ws => 137..138, // " ",
        Token::Assignment => 138..139, // "=",
        Token::Ws => 139..140, // " ",
        Token::QuoteOpen => 140..141, // "\"",
        Token::LineStrText => 141..147, // "simple",
        Token::QuoteClose => 147..148, // "\"",
        Token::Dot => 148..149, // ".",
        Token::Identifier => 149..155, // "length",
        Token::LParen => 155..156, // "(",
        Token::RParen => 156..157, // ")",
        Token::Nl => 157..158, // "\n",
        Token::Identifier => 158..165, // "println",
        Token::LParen => 165..166, // "(",
        Token::Identifier => 166..169, // "len",
        Token::RParen => 169..170, // ")",
        Token::Nl => 170..171, // "\n",
        Token::RCurl => 171..172, // "}",
        Token::EOF => 172..172
    ]);
}
