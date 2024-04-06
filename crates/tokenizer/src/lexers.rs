use std::{
    fmt::Display,
    ops::{Range, RangeInclusive},
};

use logos::Source;
use token_maps::{KEYWORDS, OPERATORS};
use tokens::Token;

pub type Span = Range<usize>;

#[derive(Debug)]
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
    last_token: Option<Result<Token, ()>>,
    mode: LexGrammarMode,
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
    type Item = TokenInfo;

    fn next(&mut self) -> Option<Self::Item> {
        // comments
        // keywords
        // operators
        // strings
        // handle EOF
        let all_parsers = [parse_operator, parse_keyword, err_parser];
        if let Some(res) = apply_parsers(&all_parsers, &self.input, self.pos) {
            self.pos = res.span.end;
            Some(res)
        } else {
            None
        }
    }
}

const OPERATORS_KEY_LENGTH_RANGE: RangeInclusive<u8> = 1..=3;
const KEYWORDS_KEY_LENGTH_RANGE: RangeInclusive<u8> = 2..=11;

fn apply_parsers(
    ps: &[impl Fn(&str, usize) -> Option<TokenInfo>],
    input: &str,
    offset: usize,
) -> Option<TokenInfo> {
    for p in ps {
        if let Some(token_info) = (p)(input, offset) {
            return Some(token_info);
        }
    }
    None
}

fn err_parser(input: &str, offset: usize) -> Option<TokenInfo> {
    if input.len() == offset {
        return None;
    };

    let all_parsers = [parse_operator, parse_keyword];
    let mut next_offset = offset;
    while let None = apply_parsers(&all_parsers, input, next_offset) {
        next_offset += 1;
    }
    if offset == next_offset {
        None
    } else {
        Some(TokenInfo::error(offset..next_offset))
    }
}

fn parse_operator(input: &str, offset: usize) -> Option<TokenInfo> {
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
                return Some(TokenInfo::success(*token, span));
            }
        }
    }
    None
}

fn parse_keyword(input: &str, offset: usize) -> Option<TokenInfo> {
    if input.len() == offset {
        return None;
    };
    // we prioritize bigger lengths
    for size in KEYWORDS_KEY_LENGTH_RANGE.rev() {
        let span = offset..offset + size as usize;
        let slice = input.slice(span.clone());
        if let Some(key) = slice {
            let matched = KEYWORDS.get(key);
            if let Some((token, next_char)) = matched.zip(input.get(span.end..span.end + 1)) {
                // TODO: use identifier parser here when it is implemented
                if !next_char
                    .chars()
                    .nth(0)
                    .map(|c| c.is_alphanumeric())
                    .unwrap_or_default()
                {
                    return Some(TokenInfo::success(*token, span));
                }
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
"#,
        );
        for lex in lex {
            println!("{}", lex);
        }
        Ok(())
    }
}
