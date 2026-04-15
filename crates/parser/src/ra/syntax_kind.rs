// impl From<u16> for SyntaxKind {
//     #[inline]
//     fn from(d: u16) -> SyntaxKind {
//         assert!(d <= (SyntaxKind::__LAST as u16));
//         unsafe { std::mem::transmute::<u16, SyntaxKind>(d) }
//     }
// }

// impl From<SyntaxKind> for u16 {
//     #[inline]
//     fn from(k: SyntaxKind) -> u16 {
//         k as u16
//     }
// }

// use syntax::SyntaxKind;

// impl SyntaxKind {
// #[inline]
// pub fn is_trivia(self) -> bool {
//     matches!(
//         self,
//         SyntaxKind::WS | SyntaxKind::DELIMITED_COMMENT | SyntaxKind::LINE_COMMENT
//     )
// }

//// Returns true if this is an identifier or a keyword.
// #[inline]
// pub fn is_any_identifier(self) -> bool {
//     // Assuming no edition removed keywords...
//     self == SyntaxKind::IDENT || self.is_keyword(Edition::LATEST)
// }
// }
