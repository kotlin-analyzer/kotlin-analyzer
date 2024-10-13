mod lexer;
pub use lexer::*;

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
