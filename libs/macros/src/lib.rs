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

#[macro_export]
macro_rules! assert_failure {
    ($parser: expr, $source: literal) => {
        let result = $parser(Step::new($source, None));
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
