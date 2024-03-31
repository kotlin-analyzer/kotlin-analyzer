use logos::{Logos, Source, SpannedIter};

use crate::tokens::Token;

pub fn lexer<'a, S>(source: &'a <Token as Logos>::Source) -> SpannedIter<'a, Token>
where
    S: Source,
{
    Token::lexer(source).spanned()
}
