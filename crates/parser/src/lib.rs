#![allow(dead_code)]
mod parts;

#[cfg(test)]
mod test_utils;

use core::fmt;

use ast::syntax::SyntaxKind::{self, *};
use lexer::SpannedWithSource;
use tokens::Token;

pub trait TokenSource<'a> {
    fn current(&mut self) -> Option<&SpannedWithSource<'a>>;
    #[cfg(debug_assertions)]
    fn current_index(&mut self) -> Option<usize>;
    fn lookahead_nth(&mut self, n: usize) -> Option<&SpannedWithSource<'a>>;
    fn is_keyword(&mut self) -> bool;

    fn bump(&mut self) -> Option<SpannedWithSource<'a>>;
}

pub trait TreeSink {
    fn token(&mut self, token: Token, substring: &str);

    fn start_node(&mut self, kind: SyntaxKind);
    fn finish_node(&mut self);

    fn error(&mut self, error: ParseError);
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
    lo: usize,
    hi: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parse error at {}..{}: {}",
            self.lo, self.hi, self.message
        )
    }
}

impl ParseError {
    pub(crate) fn new(message: String, lo: usize) -> Self {
        ParseError {
            message,
            lo,
            hi: lo,
        }
    }
    pub(crate) fn with_hi(mut self, hi: usize) -> Self {
        self.hi = hi;
        self
    }
}

pub(crate) struct Parser<'a, 'b> {
    source: &'a mut dyn TokenSource<'b>,
    sink: &'a mut dyn TreeSink,
}

impl Parser<'_, '_> {
    fn parse(mut self) {
        self.start_node(ROOT);
        parts::general::kotlin_file(&mut self);
        self.finish_node(ROOT);
    }

    fn parse_script(mut self) {
        self.start_node(ROOT);
        parts::general::script(&mut self);
        self.finish_node(ROOT);
    }

    fn skip_trivia(&mut self) {
        while matches!(
            self.current_token(),
            Some(Token::WS | Token::LINE_COMMENT | Token::DELIMITED_COMMENT)
        ) {
            self.bump()
        }
    }

    fn skip_trivia_and_newlines(&mut self) {
        while matches!(
            self.current_token(),
            Some(Token::WS | Token::LINE_COMMENT | Token::DELIMITED_COMMENT | Token::NL)
        ) {
            self.bump()
        }
    }

    fn current(&mut self) -> Option<&SpannedWithSource<'_>> {
        self.source.current()
    }
    fn current_token(&mut self) -> Option<&Token> {
        self.source.current().map(|sp| sp.token())
    }

    /// Returns the index of the current token, if available.
    /// This is used for error reporting and other features that require knowledge of the token's position in the source.
    fn current_token_index(&mut self) -> Option<usize> {
        self.source.current_index()
    }

    fn next_two(&mut self) -> Option<(SpannedWithSource<'_>, SpannedWithSource<'_>)> {
        self.source
            .lookahead_nth(0)
            .cloned()
            .zip(self.source.lookahead_nth(1).cloned())
    }

    fn next_two_tokens(&mut self) -> Option<(Token, Token)> {
        self.next_two().map(|(fs, snd)| (*fs.token(), *snd.token()))
    }

    pub(crate) fn lookahead_token(&mut self, n: usize) -> Option<Token> {
        self.source.lookahead_nth(n).map(|sp| *sp.token())
    }

    fn bump(&mut self) {
        if let Some(next) = self.source.bump() {
            self.sink.token(*next.token(), next.substring());
        }
    }

    fn error(&mut self, error: impl Into<String>) {
        if let Some(sp) = self.current() {
            let lo = sp.span().start;
            let hi = sp.span().end;
            self.sink
                .error(ParseError::new(error.into(), lo).with_hi(hi));
        }
    }

    fn recover_until(&mut self, recovery: &[Token]) {
        loop {
            match self.current_token() {
                None => break,
                Some(token) if matches!(token, Token::EOF) || recovery.contains(token) => {
                    break;
                }
                _ => self.bump(),
            }
        }
    }

    fn expect(&mut self, token: Token, error: &str) -> bool {
        if self.current_token() == Some(&token) {
            self.bump();
            true
        } else {
            self.error(error);
            false
        }
    }

    fn expect_recover(&mut self, token: Token, error: &str, recovery: &[Token]) -> bool {
        if self.expect(token, error) {
            true
        } else {
            self.recover_until(recovery);
            false
        }
    }

    fn new<'a: 'b, 'b>(
        token_source: &'a mut dyn TokenSource<'b>,
        tree_sink: &'a mut dyn TreeSink,
    ) -> Parser<'a, 'a> {
        Parser {
            source: token_source,
            sink: tree_sink,
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        // #[cfg(debug_assertions)]
        // {
        //     println!("Starting node: {:?}", kind);
        // }
        self.sink.start_node(kind);
    }
    fn finish_node(&mut self, _kind: SyntaxKind) {
        // #[cfg(debug_assertions)]
        // {
        //     println!("Finishing node: {:?}", kind);
        // }
        self.sink.finish_node();
    }
}

pub fn parse<'a>(token_source: &'a mut dyn TokenSource<'_>, tree_sink: &'a mut dyn TreeSink) {
    Parser {
        source: token_source,
        sink: tree_sink,
    }
    .parse()
}
