//! Lexing `&str` into a sequence of Kotlin tokens.
//! Note that these tokens, unlike the tokens we feed into the parser, do
//! include info about comments and whitespace.

use std::ops;

use lexer::Lexer;
use syntax::Token;

use crate::{SyntaxKind, version::KtVersion};

pub struct LexedStr<'a> {
    text: &'a str,
    kind: Vec<SyntaxKind>,
    start: Vec<u32>,
    error: Vec<LexError>,
}

struct LexError {
    msg: String,
    token: u32,
}

impl<'a> LexedStr<'a> {
    pub fn new(version: KtVersion, text: &'a str) -> LexedStr<'a> {
        let _p = tracing::info_span!("LexedStr::new").entered();
        let mut conv = Converter::new(version, text);
        let mut lexer = Lexer::new(text).spanned();
        while let Some(token_info) = lexer.next() {
            let offset = token_info.span().start;
            let token = token_info.token();
            match token {
                Token::ERR => conv.push_err(format!(
                    "Invalid token: {}",
                    conv.res.text[offset..].chars().next().unwrap_or_default()
                )),
                _ => conv.push(SyntaxKind::from(*token), offset),
            }
        }
        conv.res
    }

    pub fn single_token(version: KtVersion, text: &'a str) -> Option<(SyntaxKind, Option<String>)> {
        if text.is_empty() {
            return None;
        }

        let spanned = Lexer::new(text).spanned_with_src().next()?;
        if spanned.substring().len() as usize != text.len() {
            return None;
        }

        let mut conv = Converter::new(version, text);
        conv.push(SyntaxKind::from(*spanned.token()), spanned.span().start);

        match &*conv.res.kind {
            [kind] => Some((*kind, conv.res.error.pop().map(|it| it.msg))),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        self.text
    }

    pub fn len(&self) -> usize {
        self.kind.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn kind(&self, i: usize) -> SyntaxKind {
        assert!(i < self.len());
        self.kind[i]
    }

    pub fn text(&self, i: usize) -> &str {
        self.range_text(i..i + 1)
    }

    pub fn range_text(&self, r: ops::Range<usize>) -> &str {
        assert!(r.start < r.end && r.end <= self.len());
        let lo = self.start[r.start] as usize;
        let hi = self.start[r.end] as usize;
        &self.text[lo..hi]
    }

    // Naming is hard.
    pub fn text_range(&self, i: usize) -> ops::Range<usize> {
        assert!(i < self.len());
        let lo = self.start[i] as usize;
        let hi = self.start[i + 1] as usize;
        lo..hi
    }
    pub fn text_start(&self, i: usize) -> usize {
        assert!(i <= self.len());
        self.start[i] as usize
    }
    pub fn text_len(&self, i: usize) -> usize {
        assert!(i < self.len());
        let r = self.text_range(i);
        r.end - r.start
    }

    pub fn error(&self, i: usize) -> Option<&str> {
        assert!(i < self.len());
        let err = self
            .error
            .binary_search_by_key(&(i as u32), |i| i.token)
            .ok()?;
        Some(self.error[err].msg.as_str())
    }

    pub fn errors(&self) -> impl Iterator<Item = (usize, &str)> + '_ {
        self.error
            .iter()
            .map(|it| (it.token as usize, it.msg.as_str()))
    }

    fn push(&mut self, kind: SyntaxKind, offset: usize) {
        self.kind.push(kind);
        self.start.push(offset as u32);
    }
}

struct Converter<'a> {
    res: LexedStr<'a>,
    version: KtVersion,
}

impl<'a> Converter<'a> {
    fn new(version: KtVersion, text: &'a str) -> Self {
        Self {
            res: LexedStr {
                text,
                kind: Vec::with_capacity(text.len() / 3),
                start: Vec::with_capacity(text.len() / 3),
                error: Vec::new(),
            },
            version,
        }
    }

    fn push(&mut self, kind: SyntaxKind, offset: usize) {
        self.res.push(kind, offset);
    }

    fn push_err(&mut self, msg: String) {
        self.res.error.push(LexError {
            msg,
            token: self.res.len() as u32,
        });
    }
}
