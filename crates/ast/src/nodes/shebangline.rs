use crate::Span;
use nonempty::NonEmpty;

use super::Nl;

pub struct ShebangLine {
    pub shebang_span: Span,
    pub end: NonEmpty<Nl>,
}
