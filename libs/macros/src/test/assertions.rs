/// asserts that a tokenizer successfully to tokenize the next characters
/// NB: can only be used within lexer crate
/// ```ignore
/// assert_success!(bin_or_hex_lit, "0b101_010", 9, BIN_LITERAL);
/// assert_success!(real_lit, "45.44e+940_", 10);
/// assert_success!(int_lit, "0");
/// ```
#[macro_export]
macro_rules! assert_success {
    ($parser: expr, $source: literal) => {
        $parser(Step::new($source, None)).unwrap();
    };
    ($parser: expr, $source: literal, $pos: literal) => {
        let result = $parser(Step::new($source, None)).unwrap();
        assert_eq!(result.pos, $pos);
    };
    ($parser: expr, $source: literal, $pos: literal, $token: path) => {
        let result = $parser(Step::new($source, None)).unwrap();
        assert_eq!(result.pos, $pos);
        assert_eq!(result.res, $token);
    };
}

/// asserts that a tokenizer fails to tokenize the next characters
/// NB: can only be used within lexer crate
/// ```ignore
/// assert_failure!(line_comment, "#! shebang");
/// ```
#[macro_export]
macro_rules! assert_failure {
    ($parser: expr, $source: literal) => {
        let result = $parser(Step::new($source, None));
        assert_eq!(result, None);
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
        let lexer = Lexer::new(&source).spanned_with_src();
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
        let lexer = Lexer::new(&source).spanned();
        println!("Source of 0..{}", source.len() - 1);
        for entry in lexer {
            println!("{},", entry);
        }
    };
}
