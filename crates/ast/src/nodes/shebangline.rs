use crate::Span;
use nonempty::NonEmpty;

use super::Nl;

pub struct ShebangLine {
    shebang_span: Span,
    end: NonEmpty<Nl>,
}
