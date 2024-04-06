use std::{
    fmt::Display,
    ops::{Range, RangeInclusive},
};

use logos::Source;
use token_maps::{KEYWORDS, OPERATORS};
use tokens::Token;

pub type Span = Range<usize>;

#[derive(Debug, Clone)]
pub struct TokenInfo {
    token: Result<Token, ()>,
    span: Span,
}

impl Display for TokenInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token = self
            .token
            .map(|t| format!("{t:?}"))
            .unwrap_or("Error".into());
        write!(f, "{token} @ {:?}", self.span)
    }
}

impl TokenInfo {
    fn success(token: Token, span: Span) -> Self {
        Self {
            token: Ok(token),
            span,
        }
    }

    fn error(span: Span) -> Self {
        Self {
            token: Err(()),
            span,
        }
    }
}
enum LexGrammarMode {
    Normal,
    String,
    MultilineString,
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    last_token: Option<TokenKind>,
    mode: LexGrammarMode,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Operator(TokenInfo),
    Keyword(TokenInfo),
    Identifier(TokenInfo),
    Comment(TokenInfo),
    Err(TokenInfo),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.token_info().fmt(f)
    }
}

impl TokenKind {
    fn span(&self) -> Span {
        self.token_info().span.clone()
    }

    fn token_info(&self) -> &TokenInfo {
        match self {
            TokenKind::Operator(info) => info,
            TokenKind::Keyword(info) => info,
            TokenKind::Identifier(info) => info,
            TokenKind::Comment(info) => info,
            TokenKind::Err(info) => info,
        }
    }

    fn is_operator(&self) -> bool {
        matches!(self, Self::Operator(_))
    }
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            last_token: None,
            mode: LexGrammarMode::Normal,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = TokenKind;

    fn next(&mut self) -> Option<Self::Item> {
        // comments
        // keywords
        // operators
        // strings
        // handle EOF
        let all_parsers = [parse_operator, parse_keyword, err_parser];
        let result = apply_parsers(&all_parsers, &self.input, self.pos, &self.last_token);
        if let Some(res) = result.clone() {
            self.pos = res.span().end;
            self.last_token = result;
            Some(res)
        } else {
            None
        }
    }
}

const OPERATORS_KEY_LENGTH_RANGE: RangeInclusive<u8> = 1..=3;
const KEYWORDS_KEY_LENGTH_RANGE: RangeInclusive<u8> = 2..=11;

fn apply_parsers(
    ps: &[impl Fn(&str, usize, &Option<TokenKind>) -> Option<TokenKind>],
    input: &str,
    offset: usize,
    previous: &Option<TokenKind>,
) -> Option<TokenKind> {
    for p in ps {
        if let Some(token_info) = (p)(input, offset, previous) {
            return Some(token_info);
        }
    }
    None
}

fn err_parser(input: &str, offset: usize, prev: &Option<TokenKind>) -> Option<TokenKind> {
    if input.len() == offset {
        return None;
    };

    let all_parsers = [parse_operator, parse_keyword];
    let mut next_offset = offset;

    let mut res = apply_parsers(&all_parsers, input, next_offset, prev);
    loop {
        if res.is_none() {
            next_offset += 1;
            res = apply_parsers(&all_parsers, input, next_offset, &res)
        } else {
            break;
        }
    }

    if offset == next_offset {
        None
    } else {
        Some(TokenKind::Err(TokenInfo::error(offset..next_offset)))
    }
}

fn parse_operator(input: &str, offset: usize, _: &Option<TokenKind>) -> Option<TokenKind> {
    if input.len() == offset {
        return None;
    };
    // we prioritize bigger lengths
    for size in OPERATORS_KEY_LENGTH_RANGE.rev() {
        let span = offset..offset + size as usize;
        let slice = input.slice(span.clone());
        if let Some(key) = slice {
            let matched = OPERATORS.get(key);
            if let Some(token) = matched {
                return Some(TokenKind::Operator(TokenInfo::success(*token, span)));
            }
        }
    }
    None
}

fn parse_keyword(input: &str, offset: usize, prev: &Option<TokenKind>) -> Option<TokenKind> {
    if input.len() == offset {
        return None;
    };
    // we prioritize bigger lengths
    for size in KEYWORDS_KEY_LENGTH_RANGE.rev() {
        let span = offset..offset + size as usize;
        let slice = input.slice(span.clone());
        if let Some(key) = slice {
            let matched = KEYWORDS.get(key);
            let test = prev.as_ref().map(|p| p.is_operator()).unwrap_or_default();
            if let Some((token, _)) = test
                .then(|| matched.zip(parse_operator(input, span.end, prev)))
                .flatten()
            {
                return Some(TokenKind::Keyword(TokenInfo::success(*token, span)));
            }
        }
    }
    None
}

#[cfg(test)]
mod test {
    use std::error::Error;

    use super::*;

    #[test]
    fn simple() -> Result<(), Box<dyn Error>> {
        let lex = Lexer::new(
            r#"
0444.10_99e+4f
[],--
/* comments */
//line comment
#! sh echo "hey"
hey
fun hello() = "Hello"
var funvar = 3
"#,
        );
        for lex in lex {
            println!("{}", lex);
        }
        Ok(())
    }
}
