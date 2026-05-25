mod entries;
mod t;

pub use entries::SyntaxKind;
pub use entries::SyntaxNode;

impl SyntaxKind {
    pub fn is_soft_keyword(&self) -> bool {
        todo!("is_soft_keyword is not implemented yet");
    }
}
