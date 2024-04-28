use macros::multiline_str;
use tokenizer::{Lexer, TokenInfo};
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
    let lex = Lexer::new(&source).spanned();
    for lex in lex {
        println!("{}", &lex);
    }

    lexer_matches!(&source, [
        Token::TripleQuoteOpen => 0..3,
        Token::MultiLineStrText => 3..11,
        Token::TripleQuoteClose => 11..14,
        Token::Nl => 14..15,
        Token::TripleQuoteOpen => 15..18,
        Token::MultiLineStrText => 18..27,
        Token::MultiStrExprStart => 27..32,
        Token::MultiLineStrText => 32..34,
        Token::QuoteOpen => 34..35,
        Token::LineStrText => 35..59,
        Token::QuoteClose => 59..60,
        Token::MultiLineStrText => 60..61,
        Token::MultiLineStringQuote => 61..64,
        Token::TripleQuoteClose => 64..67,
        Token::Nl => 67..68,
        Token::TripleQuoteOpen => 68..71,
        Token::MultiLineStringQuote => 71..75,
        Token::TripleQuoteClose => 75..78,
        Token::Ws => 78..79,
        Token::LineComment => 79..128
    ]);
}

#[test]
fn nested_multi_str_test() {
    lexer_matches!("", [
        Token::DelimitedComment => 0..1,
        Token::Add => 0..1
    ]);
    lexer_matches!("eee", [
        Token::DelimitedComment => 0..1,
        Token::Add => 0..3
    ]);
    lexer_matches!("", Token::DelimitedComment, [0..1, 0..1]);
    lexer_matches!("", Token::Add, [0..1, 0..2]);
    lexer_matches!("", Token::Add, [0..1, 0..2]);

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
    let mut lex = Lexer::new(&source).spanned();
    let next = lex.next();
    assert_eq!(next, Some(TokenInfo::new(Token::Add, 0..2)));
    // for lex in lex {
    //     print!("{} - ", &lex);
    //     println!("{:?}", &source[lex.span]);
    // }
}
