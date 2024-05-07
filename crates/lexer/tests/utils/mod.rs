/// This asserts that a lexer's output
/// matches the given tokens at their given spans
/// ```ignore
///
///    let source = r#""hey ${echo("test")} stranger""#;
///    lexer_matches!(source, [
///        QUOTE_OPEN => 0..1,
///        LINE_STR_TEXT => 1..5,
///        LINE_STR_EXPR_START => 5..7
///    ]);
///
/// ```
#[macro_export]
macro_rules! lexer_matches {
        ($source: expr, [$($token_kind: expr => $start:literal..$end:literal),+]) => {
            let source: &str = $source;
            let mut lexer =  ::lexer::Lexer::new(&source).spanned();
            $(assert_eq!(
                lexer.next(),
                Some(::lexer::TokenInfo::new($token_kind, $start..$end))
            );)+
        };

        ($source: expr, $filter_token:path, [$($start:literal..$end:literal),+]) => {
            let source: &str = $source;
            let filter_token: ::tokens::Token = $filter_token;

            let mut lexer =  ::lexer::Lexer::new(&source)
            .spanned()
            .filter(|info| *info.token() == filter_token);

            $(assert_eq!(lexer.next(), Some(::lexer::TokenInfo::new($filter_token, $start..$end)));)+
        };
}

/// Prints out all the tokens with their respective spans and substrings
/// ```ignore
/// let source = "data class Hey()"
/// dbg_lexer_src!(&source);
/// ```
#[macro_export]
macro_rules! dbg_lexer_src {
    ($source: expr) => {
        let source: &str = $source;
        let lexer = ::lexer::Lexer::new(&source).spanned_with_src();
        println!("Source of 0..{}", source.len() - 1);
        for entry in lexer {
            println!("{},", entry);
        }
    };
}

/// Prints out all the tokens with their respective spans
/// ```ignore
/// let source = "data class Hey()"
/// dbg_lexer!(&source);
/// ```
#[macro_export]
macro_rules! dbg_lexer {
    ($source: expr) => {
        let source: &str = $source;
        let lexer = ::lexer::Lexer::new(&source).spanned();
        println!("Source of 0..{}", source.len() - 1);
        for entry in lexer {
            println!("{},", entry);
        }
    };
}

/// remove the spaces in each line of a multiline string
#[macro_export]
macro_rules! trim_idents {
    ($source: literal) => {
        $source
            .lines()
            .map(|l| l.trim_start())
            .collect::<Vec<_>>()
            .join("\n")
    };
}
