//! Shortcuts that span lexer/parser abstraction.
//!
//! The way Rust works, parser doesn't necessary parse text, and you might
//! tokenize text without parsing it further. So, it makes sense to keep
//! abstract token parsing, and string tokenization as completely separate
//! layers.
//!
//! However, often you do parse text into syntax trees and the glue code for
//! that needs to live somewhere. Rather than putting it to lexer or parser, we
//! use a separate shortcuts module for that.

use std::mem;

use crate::version::KtVersion;
use crate::{SyntaxKind, SyntaxKind::*};

use super::{
    input::Input,
    lexed_str::LexedStr,
    output::{Output, Step},
};

#[derive(Debug)]
pub enum StrStep<'a> {
    Token { kind: SyntaxKind, text: &'a str },
    Enter { kind: SyntaxKind },
    Exit,
    Error { msg: &'a str, pos: usize },
}

impl LexedStr<'_> {
    pub fn to_input(&self, version: KtVersion) -> Input {
        let _p = tracing::info_span!("LexedStr::to_input").entered();
        let mut res = Input::with_capacity(self.len());
        let mut seen_ws = false;
        let mut seen_nl = false;
        for i in 0..self.len() {
            let kind = self.kind(i);
            if !kind.is_trivia() {
                res.push(kind, version);
                if seen_ws {
                    res.set_ws_before();
                }
                if seen_nl {
                    res.set_nl_before();
                }
                seen_ws = false;
                seen_nl = false;
            } else if matches!(kind, WHITESPACE | NEWLINE) {
                seen_ws = true;
                if matches!(kind, NEWLINE) {
                    seen_nl = true;
                }
            }
        }
        res
    }

    /// NB: only valid to call with Output from Reparser/TopLevelEntry.
    pub fn intersperse_trivia(&self, output: &Output, sink: &mut dyn FnMut(StrStep<'_>)) -> bool {
        let mut builder = Builder { lexed: self, pos: 0, state: State::PendingEnter, sink };

        for event in output.iter() {
            match event {
                Step::Token { kind, n_input_tokens: n_raw_tokens } => {
                    builder.token(kind, n_raw_tokens)
                }
                Step::Enter { kind } => builder.enter(kind),
                Step::Exit => builder.exit(),
                Step::Error { msg } => {
                    let text_pos = builder.lexed.text_start(builder.pos);
                    (builder.sink)(StrStep::Error { msg, pos: text_pos });
                }
            }
        }

        match mem::replace(&mut builder.state, State::Normal) {
            State::PendingExit => {
                builder.eat_trivias();
                (builder.sink)(StrStep::Exit);
            }
            State::PendingEnter | State::Normal => unreachable!(),
        }

        // is_eof?
        builder.pos == builder.lexed.len()
    }
}

struct Builder<'a, 'b> {
    lexed: &'a LexedStr<'a>,
    pos: usize,
    state: State,
    sink: &'b mut dyn FnMut(StrStep<'_>),
}

enum State {
    PendingEnter,
    Normal,
    PendingExit,
}

impl Builder<'_, '_> {
    fn token(&mut self, kind: SyntaxKind, n_tokens: u8) {
        match mem::replace(&mut self.state, State::Normal) {
            State::PendingEnter => unreachable!(),
            State::PendingExit => (self.sink)(StrStep::Exit),
            State::Normal => (),
        }
        self.eat_trivias();
        self.do_token(kind, n_tokens as usize);
    }

    fn enter(&mut self, kind: SyntaxKind) {
        match mem::replace(&mut self.state, State::Normal) {
            State::PendingEnter => {
                (self.sink)(StrStep::Enter { kind });
                // No need to attach trivias to previous node: there is no
                // previous node.
                return;
            }
            State::PendingExit => (self.sink)(StrStep::Exit),
            State::Normal => (),
        }

        let n_trivias =
            (self.pos..self.lexed.len()).take_while(|&it| self.lexed.kind(it).is_trivia()).count();
        let leading_trivias = self.pos..self.pos + n_trivias;
        let n_attached_trivias = n_attached_trivias(
            kind,
            leading_trivias.rev().map(|it| (self.lexed.kind(it), self.lexed.text(it))),
        );
        self.eat_n_trivias(n_trivias - n_attached_trivias);
        (self.sink)(StrStep::Enter { kind });
        self.eat_n_trivias(n_attached_trivias);
    }

    fn exit(&mut self) {
        match mem::replace(&mut self.state, State::PendingExit) {
            State::PendingEnter => unreachable!(),
            State::PendingExit => (self.sink)(StrStep::Exit),
            State::Normal => (),
        }
    }

    fn eat_trivias(&mut self) {
        while self.pos < self.lexed.len() {
            let kind = self.lexed.kind(self.pos);
            if !kind.is_trivia() {
                break;
            }
            self.do_token(kind, 1);
        }
    }

    fn eat_n_trivias(&mut self, n: usize) {
        for _ in 0..n {
            let kind = self.lexed.kind(self.pos);
            assert!(kind.is_trivia());
            self.do_token(kind, 1);
        }
    }

    fn do_token(&mut self, kind: SyntaxKind, n_tokens: usize) {
        let text = &self.lexed.range_text(self.pos..self.pos + n_tokens);
        self.pos += n_tokens;
        (self.sink)(StrStep::Token { kind, text });
    }
}

fn n_attached_trivias<'a>(
    kind: SyntaxKind,
    trivias_and_nls: impl Iterator<Item = (SyntaxKind, &'a str)>,
) -> usize {
    match kind {
        KOTLIN_FILE
        | SCRIPT
        | CLASS_DECLARATION
        | OBJECT_DECLARATION
        | FUNCTION_DECLARATION
        | PROPERTY_DECLARATION
        | PARAMETER
        | FUNCTION_VALUE_PARAMETER
        | GETTER
        | SETTER
        | SECONDARY_CONSTRUCTOR
        | PACKAGE_HEADER
        | IMPORT_HEADER
        | TYPE_ALIAS => {
            let mut res = 0;
            let trivias = trivias_and_nls.enumerate().peekable();

            for (i, (kind, text)) in trivias {
                match kind {
                    DELIMITED_COMMENT | LINE_COMMENT => {
                        if is_outer(text) {
                            res = i + 1;
                        } else {
                            break;
                        }
                    }
                    _ => (),
                }
            }
            res
        }
        _ => 0,
    }
}

fn is_outer(text: &str) -> bool {
    if text.starts_with("////") || text.starts_with("/***") {
        return false;
    }
    // Just would be nice to have Rustdoc comments in Kotlin
    text.starts_with("///") || text.starts_with("/**")
}
