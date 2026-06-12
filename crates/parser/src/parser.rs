//! See [`Parser`].

use crate::SyntaxKind::{self, EOF, ERROR, TOMBSTONE};
use crate::T;
use crate::grammar::annotations::annotation;
use crate::grammar::modifiers::modifiers;
use crate::version::KtVersion;
use ::std::collections::VecDeque;
use ::std::mem;
use drop_bomb::DropBomb;
use std::cell::Cell;
use std::num::NonZeroU32;

use super::{event::Event, input::Input, token_set::TokenSet};

/// Build a forward-parent offset. The offset is always ≥ 1 because the
/// forward-parent event is created *after* the event it forwards to, so
/// `NonZeroU32` is always valid here. Panics only on a parser bug.
#[inline]
fn fwd_parent(offset: u32) -> NonZeroU32 {
    NonZeroU32::new(offset).expect("forward-parent offset must be non-zero")
}

/// `Parser` struct provides the low-level API for
/// navigating through the stream of tokens and
/// constructing the parse tree. The actual parsing
/// happens in the [`grammar`](super::grammar) module.
///
/// However, the result of this `Parser` is not a real
/// tree, but rather a flat stream of events of the form
/// "start expression, consume number literal,
/// finish expression". See `Event` docs for more.
pub(crate) struct Parser<'t> {
    inp: &'t Input,
    pos: usize,
    events: Vec<Event>,
    errors: Vec<String>,
    steps: Cell<u32>,
    pub(crate) dangling: VecDeque<DanglingMarker>,
}

const PARSER_STEP_LIMIT: usize = if cfg!(debug_assertions) { 150_000 } else { 15_000_000 };

impl<'t> Parser<'t> {
    pub(super) fn new(inp: &'t Input) -> Parser<'t> {
        Parser {
            inp,
            pos: 0,
            events: Vec::with_capacity(2 * inp.len()),
            errors: Vec::new(),
            steps: Cell::new(0),
            dangling: VecDeque::new(),
        }
    }

    pub(crate) fn finish(self) -> (Vec<Event>, Vec<String>) {
        (self.events, self.errors)
    }

    /// Returns the kind of the current token.
    /// If parser has already reached the end of input,
    /// the special `EOF` kind is returned.
    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    /// Lookahead operation: returns the kind of the next nth
    /// token.
    pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
        assert!(n <= 3);

        let steps = self.steps.get();
        assert!(
            (steps as usize) < PARSER_STEP_LIMIT,
            "the parser seems stuck at {:?}",
            self.inp.kind(self.pos + n)
        );
        self.steps.set(steps + 1);

        self.inp.kind(self.pos + n)
    }

    /// Checks if the current token is `kind`.
    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    pub(crate) fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.inp.kind(self.pos + n) == kind
    }

    pub(crate) fn nth_ats(&self, n: usize, kinds: TokenSet) -> bool {
        kinds.contains(self.inp.kind(self.pos + n))
    }

    /// Consume the next token if `kind` matches.
    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        self.do_bump(kind, 1);
        true
    }

    /// Checks if the current token is in `kinds`.
    pub(crate) fn at_ts(&self, kinds: TokenSet) -> bool {
        kinds.contains(self.current())
    }

    /// Checks if the current token is a `(` preceded by whitespace.
    pub(crate) fn at_lparen_after_ws(&self) -> bool {
        self.at(T!['(']) && self.has_ws_before()
    }

    /// Checks if the current token has a whitespace before it.
    pub(crate) fn has_ws_before(&self) -> bool {
        self.inp.has_ws_before(self.pos)
    }

    /// Checks if the current token has a newline before it.
    pub(crate) fn has_nl_before(&self) -> bool {
        self.inp.has_nl_before(self.pos)
    }
    /// Starts a new node in the syntax tree. All nodes and tokens
    /// consumed between the `start` and the corresponding `Marker::complete`
    /// belong to the same node.
    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len() as u32;
        self.push_event(Event::tombstone());
        Marker::new(pos)
    }

    pub(crate) fn start_with_modifiers(&mut self) -> DanglingMarker {
        let mut m =
            match self.dangling.pop_front_if(|e| matches!(e, DanglingMarker::Modifiers { .. })) {
                Some(m @ DanglingMarker::Modifiers { has_modifiers: true, .. }) => m,
                Some(m @ DanglingMarker::Modifiers { has_modifiers: false, .. }) => {
                    m.forget(self);
                    DanglingMarker::Modifiers { marker: self.start(), has_modifiers: false }
                }
                _ => DanglingMarker::Modifiers { marker: self.start(), has_modifiers: false },
            };

        let seen = modifiers(self);
        if let DanglingMarker::Modifiers { has_modifiers, .. } = &mut m {
            *has_modifiers |= seen.is_some();
        }
        m
    }

    pub(crate) fn start_with_fresh_modifiers(&mut self) -> DanglingMarker {
        let mut m =
            match self.dangling.pop_front_if(|e| matches!(e, DanglingMarker::Modifiers { .. })) {
                Some(m @ DanglingMarker::Modifiers { .. }) => {
                    m.forget(self);
                    DanglingMarker::Modifiers { marker: self.start(), has_modifiers: false }
                }
                _ => DanglingMarker::Modifiers { marker: self.start(), has_modifiers: false },
            };

        let seen = modifiers(self);
        if let DanglingMarker::Modifiers { has_modifiers, .. } = &mut m {
            *has_modifiers |= seen.is_some();
        }
        m
    }

    pub(crate) fn start_with_annotation(&mut self) -> DanglingMarker {
        // TODO: Match only annotations
        let mut m =
            match self.dangling.pop_front_if(|e| matches!(e, DanglingMarker::Modifiers { .. })) {
                Some(m @ DanglingMarker::Modifiers { .. }) => m,
                _ => DanglingMarker::Modifiers { marker: self.start(), has_modifiers: false },
            };
        // TODO: revise, related to the above TODO
        let seen = annotation(self);
        if let DanglingMarker::Modifiers { has_modifiers, .. } = &mut m {
            *has_modifiers |= seen.is_some();
        }
        m
    }

    pub(crate) fn start_with_func(&mut self) -> DanglingMarker {
        match self.dangling.pop_front_if(|e| matches!(e, DanglingMarker::Func { .. })) {
            Some(m @ DanglingMarker::Func { valid: true, .. }) => m,
            Some(m @ DanglingMarker::Func { valid: false, .. }) => {
                m.forget(self);
                DanglingMarker::Func { marker: self.start(), valid: false }
            }
            _ => {
                let m = self.start();
                DanglingMarker::Func { marker: m, valid: false }
            }
        }
    }

    pub(crate) fn start_with_paren(&mut self) -> DanglingMarker {
        match self.dangling.pop_front_if(|e| matches!(e, DanglingMarker::Paren(_))) {
            Some(m @ DanglingMarker::Paren(_)) => m,
            _ => {
                let m = self.start();
                self.bump(T!['(']);
                DanglingMarker::Paren(m)
            }
        }
    }

    pub(crate) fn has_dangling_parens(&self) -> bool {
        self.dangling.iter().any(|e| matches!(e, DanglingMarker::Paren(_)))
    }

    /// Consume the next token. Panics if the parser isn't currently at `kind`.
    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind));
    }

    /// Advances the parser by one token
    pub(crate) fn bump_any(&mut self) {
        let kind = self.nth(0);
        if kind == EOF {
            return;
        }
        self.do_bump(kind, 1);
    }

    /// Advances the parser by one token, remapping its kind.
    /// This is useful to create contextual keywords from
    /// identifiers. For example, the lexer creates a `union`
    /// *identifier* token, but the parser remaps it to the
    /// `union` keyword, and keyword is what ends up in the
    /// final tree.
    pub(crate) fn bump_remap(&mut self, kind: SyntaxKind) {
        if self.nth(0) == EOF {
            // FIXME: panic!?
            return;
        }
        self.do_bump(kind, 1);
    }

    /// Emit error with the `message`
    /// FIXME: this should be much more fancy and support
    /// structured errors with spans and notes, like rustc
    /// does.
    pub(crate) fn error<T: Into<String>>(&mut self, message: T) {
        let err = self.errors.len() as u32;
        self.errors.push(message.into());
        self.push_event(Event::Error { err });
    }

    /// Consume the next token if it is `kind` or emit an error
    /// otherwise.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error(format!("expected {kind:?}"));
        false
    }

    /// Create an error node and consume the next token.
    pub(crate) fn err_and_bump(&mut self, message: &str) {
        let m = self.start();
        self.error(message);
        self.bump_any();
        m.complete(self, ERROR);
    }

    /// Create an error node and consume the next token unless it is in the recovery set.
    ///
    /// Returns true if recovery kicked in.
    pub(crate) fn err_recover(&mut self, message: &str, recovery: TokenSet) -> bool {
        if matches!(self.current(), T!['{'] | T!['}']) {
            self.error(message);
            return true;
        }

        if self.at_ts(recovery) {
            self.error(message);
            return true;
        }

        let m = self.start();
        self.error(message);
        self.bump_any();
        m.complete(self, ERROR);
        false
    }

    fn do_bump(&mut self, kind: SyntaxKind, n_raw_tokens: u8) {
        self.pos += n_raw_tokens as usize;
        self.steps.set(0);
        self.push_event(Event::Token { kind, n_raw_tokens });
    }

    fn push_event(&mut self, event: Event) {
        self.events.push(event);
    }

    pub(crate) fn current_version(&self) -> KtVersion {
        self.inp.version(self.pos)
    }
}

/// See [`Parser::start`].
#[must_use = "Marker must be either completed or abandoned"]
pub(crate) struct Marker {
    pos: u32,
    bomb: DropBomb,
}

impl Marker {
    fn new(pos: u32) -> Marker {
        Marker { pos, bomb: DropBomb::new("Marker must be either completed or abandoned") }
    }

    /// Finishes the syntax tree node and assigns `kind` to it,
    /// and mark the create a `CompletedMarker` for possible future
    /// operation like `.precede()` to deal with forward_parent.
    pub(crate) fn complete(mut self, p: &mut Parser<'_>, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();
        let idx = self.pos as usize;
        match &mut p.events[idx] {
            Event::Start { kind: slot, .. } => {
                *slot = kind;
            }
            _ => unreachable!(),
        }
        p.push_event(Event::Finish);
        let end_pos = p.events.len() as u32;
        CompletedMarker::new(self.pos, end_pos, kind)
    }

    /// Abandons the syntax tree node. All its children
    /// are attached to its parent instead.
    pub(crate) fn abandon(mut self, parser: &mut Parser<'_>) {
        self.bomb.defuse();
        let idx = self.pos as usize;
        if idx == parser.events.len() - 1 {
            assert!(matches!(
                parser.events.pop(),
                Some(Event::Start { kind: TOMBSTONE, forward_parent: None })
            ));
        }
    }
}

pub(crate) struct CompletedMarker {
    start_pos: u32,
    end_pos: u32,
    kind: SyntaxKind,
}

impl CompletedMarker {
    fn new(start_pos: u32, end_pos: u32, kind: SyntaxKind) -> Self {
        CompletedMarker { start_pos, end_pos, kind }
    }

    /// This method allows to create a new node which starts
    /// *before* the current one. That is, parser could start
    /// node `A`, then complete it, and then after parsing the
    /// whole `A`, decide that it should have started some node
    /// `B` before starting `A`. `precede` allows to do exactly
    /// that. See also docs about
    /// [`Event::Start::forward_parent`](crate::event::Event::Start::forward_parent).
    ///
    /// Given completed events `[START, FINISH]` and its corresponding
    /// `CompletedMarker(pos: 0, _)`.
    /// Append a new `START` events as `[START, FINISH, NEWSTART]`,
    /// then mark `NEWSTART` as `START`'s parent with saving its relative
    /// distance to `NEWSTART` into forward_parent(=2 in this case);
    pub(crate) fn precede(self, parser: &mut Parser<'_>) -> Marker {
        let new_pos = parser.start();
        let idx = self.start_pos as usize;
        match &mut parser.events[idx] {
            Event::Start { forward_parent, .. } => {
                *forward_parent = Some(fwd_parent(new_pos.pos - self.start_pos));
            }
            _ => unreachable!(),
        }
        new_pos
    }

    /// Extends this completed marker *to the left* up to `m`.
    pub(crate) fn extend_to(self, parser: &mut Parser<'_>, mut m: Marker) -> CompletedMarker {
        m.bomb.defuse();
        let idx = m.pos as usize;
        match &mut parser.events[idx] {
            Event::Start { forward_parent, .. } => {
                *forward_parent = Some(fwd_parent(self.start_pos - m.pos));
            }
            _ => unreachable!(),
        }
        self
    }

    /// Extends this completed marker *to the right* up to end.
    pub(crate) fn extend_right(self, parser: &mut Parser<'_>) -> CompletedMarker {
        let idx = (self.end_pos - 1) as usize;
        match mem::replace(&mut parser.events[idx], Event::tombstone()) {
            Event::Finish => {}
            _ => {
                unreachable!()
            }
        };
        parser.push_event(Event::Finish);
        let end_pos = parser.events.len() as u32;
        CompletedMarker::new(self.start_pos, end_pos, self.kind)
    }

    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub(crate) fn last_token(&self, parser: &Parser<'_>) -> Option<SyntaxKind> {
        let end_pos = self.end_pos as usize;
        debug_assert_eq!(parser.events[end_pos - 1], Event::Finish);
        parser.events[..end_pos].iter().rev().find_map(|event| match event {
            Event::Token { kind, .. } => Some(*kind),
            _ => None,
        })
    }
}

pub(crate) enum DanglingMarker {
    /// A marker that can work for both fn declaration and anonymous function, which can be determined only after parsing the function name.
    Func {
        marker: Marker,
        valid: bool,
    },
    Paren(Marker),
    Modifiers {
        marker: Marker,
        has_modifiers: bool,
    },
}

impl DanglingMarker {
    pub(crate) fn abandon(self, parser: &mut Parser<'_>) {
        parser.dangling.push_back(self);
    }

    pub(crate) fn forget(self, parser: &mut Parser<'_>) {
        self.marker().abandon(parser);
    }

    pub(crate) fn complete(self, parser: &mut Parser<'_>, kind: SyntaxKind) -> CompletedMarker {
        match self {
            DanglingMarker::Func { marker: m, .. }
            | DanglingMarker::Paren(m)
            | DanglingMarker::Modifiers { marker: m, .. } => m.complete(parser, kind),
        }
    }
    pub(crate) fn has_modifiers(&self) -> bool {
        matches!(self, DanglingMarker::Modifiers { has_modifiers: true, .. })
    }

    pub(crate) fn has_fn(&self) -> bool {
        matches!(self, DanglingMarker::Func { valid: true, .. })
    }

    pub(crate) fn map(self, f: impl FnOnce(Marker) -> Self) -> Self {
        f(self.marker())
    }

    pub(crate) fn marker(self) -> Marker {
        match self {
            DanglingMarker::Func { marker: m, .. }
            | DanglingMarker::Paren(m)
            | DanglingMarker::Modifiers { marker: m, .. } => m,
        }
    }
}
