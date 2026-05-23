//! See [`Input`].

use crate::version::KtVersion;
use syntax::SyntaxKind;

#[allow(non_camel_case_types)]
type bits = u64;

/// Input for the parser -- a sequence of tokens.
///
/// As of now, parser doesn't have access to the *text* of the tokens, and makes
/// decisions based solely on their classification. Unlike `LexerToken`, the
/// `Tokens` doesn't include whitespace and comments. Main input to the parser.
///
/// Struct of arrays internally, but this shouldn't really matter.
pub struct Input {
    kind: Vec<SyntaxKind>,
    contextual_kind: Vec<SyntaxKind>,
    ws_before: Vec<bits>,
    version: Vec<KtVersion>,
}

/// `pub` impl used by callers to create `Tokens`.
impl Input {
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            kind: Vec::with_capacity(capacity),
            contextual_kind: Vec::with_capacity(capacity),
            ws_before: Vec::with_capacity(capacity / size_of::<bits>()),
            version: Vec::with_capacity(capacity),
        }
    }
    #[inline]
    pub fn push(&mut self, kind: SyntaxKind, version: KtVersion) {
        self.push_impl(kind, SyntaxKind::EOF, version)
    }

    #[inline]
    pub fn set_ws_before(&mut self) {
        let n = self.len() - 1;
        let (idx, b_idx) = self.bit_index(n);
        self.ws_before[idx] |= 1 << b_idx;
    }

    #[inline]
    fn push_impl(&mut self, kind: SyntaxKind, contextual_kind: SyntaxKind, version: KtVersion) {
        let idx = self.len();
        if idx.is_multiple_of(bits::BITS as usize) {
            self.ws_before.push(0);
        }
        self.kind.push(kind);
        self.contextual_kind.push(contextual_kind);
        self.version.push(version);
    }
}

/// pub(crate) impl used by the parser to consume `Tokens`.
impl Input {
    pub(crate) fn kind(&self, idx: usize) -> SyntaxKind {
        self.kind.get(idx).copied().unwrap_or(SyntaxKind::EOF)
    }
    pub(crate) fn contextual_kind(&self, idx: usize) -> SyntaxKind {
        self.contextual_kind.get(idx).copied().unwrap_or(SyntaxKind::EOF)
    }
    pub(crate) fn version(&self, idx: usize) -> KtVersion { self.version[idx] }
    pub(crate) fn has_ws_before(&self, n: usize) -> bool {
        let (idx, b_idx) = self.bit_index(n);
        self.ws_before[idx] & (1 << b_idx) != 0
    }
}

impl Input {
    fn bit_index(&self, n: usize) -> (usize, usize) {
        let idx = n / (bits::BITS as usize);
        let b_idx = n % (bits::BITS as usize);
        (idx, b_idx)
    }
    pub fn len(&self) -> usize { self.kind.len() }
}
