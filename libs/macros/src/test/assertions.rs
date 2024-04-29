#[macro_export]
macro_rules! assert_success {
    ($parser: expr, $source: literal) => {
        $parser(crate::Step::new($source, None)).unwrap();
    };
    ($parser: expr, $source: literal, $pos: literal) => {
        let result = $parser(crate::Step::new($source, None)).unwrap();
        assert_eq!(result.pos, $pos);
    };
    ($parser: expr, $source: literal, $pos: literal, $token: path) => {
        let result = $parser(crate::Step::new($source, None)).unwrap();
        assert_eq!(result.pos, $pos);
        assert_eq!(result.res, $token);
    };
}

#[macro_export]
macro_rules! assert_failure {
    ($parser: expr, $source: literal) => {
        let result = $parser(crate::Step::new($source, None));
        assert_eq!(result, None);
    };
}

#[macro_export]
macro_rules! multiline_str {
    ($source: literal) => {
        $source
            .lines()
            .map(|l| l.trim_start())
            .collect::<Vec<_>>()
            .join("\n")
    };
}

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

            $(assert_eq!(lexer.next(), Some(::tokenizer::TokenInfo::new($filter_token, $start..$end)));)+
        };
}

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
