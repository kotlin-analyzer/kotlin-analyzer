#![allow(dead_code)]
mod parts;

#[cfg(test)]
mod test_utils;

use ast::syntax::SyntaxKind::{self, *};
use lexer::SpannedWithSource;
use tokens::Token;

pub trait TokenSource<'a> {
    fn current(&mut self) -> Option<&SpannedWithSource<'a>>;
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

pub type ParseError = String;

pub(crate) struct Parser<'a, 'b> {
    source: &'a mut dyn TokenSource<'b>,
    sink: &'a mut dyn TreeSink,
}

pub(crate) enum ParseRes {
    Ok,
    Eof,
    RParen,
}

pub(crate) enum TakeRes<'a> {
    Ok,
    Err(&'a mut dyn TreeSink),
}

impl TakeRes<'_> {
    fn expect(&mut self, error: &str) {
        match self {
            TakeRes::Ok => (),
            TakeRes::Err(sink) => sink.error(error.to_string()),
        }
    }
}

impl Parser<'_, '_> {
    fn parse(mut self) {
        self.sink.start_node(ROOT);
        loop {
            match self.kotlin_file() {
                ParseRes::Eof => break,
                ParseRes::Ok => (),
                _ => unimplemented!(),
            }
        }

        self.skip_ws();
        self.skip_newlines();
        self.sink.finish_node();
    }

    fn kotlin_file(&mut self) -> ParseRes {
        self.skip_ws();
        let t = match self.current_token() {
            None | Some(Token::EOF) => return ParseRes::Eof,
            Some(t) => t,
        };

        match t {
            Token::SHEBANG_LINE_TOKEN => self.shebang_line(),
            Token::ERR => self.bump(),
            _ => unimplemented!(),
        }

        ParseRes::Ok
    }
    fn kotlin_script(&mut self) {}

    fn shebang_line(&mut self) {
        assert_eq!(self.current_token(), Some(&Token::SHEBANG_LINE_TOKEN));

        self.sink.start_node(SHEBANG_LINE);
        self.bump();
        self.take(Token::NL);
        self.skip_newlines();
        self.sink.finish_node();
    }

    fn file_annotation(&mut self) {
        match self.next_two_tokens() {
            Some((Token::AT_NO_WS | Token::AT_PRE_WS, Token::FILE)) => {
                self.sink.start_node(FILE_ANNOTATION);
                self.bump_n(2);
                self.skip_newlines();
                self.take(Token::COLON).expect("expected a colon (:)");
                self.skip_newlines()
            }
            Some((Token::ERR, _) | (_, Token::ERR)) => self.bump(),
            _ => {}
        }
    }

    fn skip_ws(&mut self) {
        self.skip(Token::WS)
    }

    fn skip_newlines(&mut self) {
        self.skip(Token::NL)
    }

    fn skip(&mut self, token: Token) {
        while self.current_token() == Some(&token) {
            self.bump()
        }
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

    fn bump_n(&mut self, count: usize) {
        for _ in 0..count {
            self.bump();
        }
    }

    fn take(&mut self, token: Token) -> TakeRes<'_> {
        assert_eq!(self.current_token(), Some(&token));
        if Some(&token) == self.current_token() {
            self.bump();
            TakeRes::Ok
        } else {
            TakeRes::Err(self.sink)
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
        #[cfg(debug_assertions)]
        {
            println!("Starting node: {:?}", kind);
        }
        self.sink.start_node(kind);
    }
    fn finish_node(&mut self, kind: SyntaxKind) {
        #[cfg(debug_assertions)]
        {
            println!("Finishing node: {:?}", kind);
        }
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
