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
