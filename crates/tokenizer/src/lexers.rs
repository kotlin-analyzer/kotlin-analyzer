use std::{
    fmt::Display,
    ops::{Range, RangeInclusive},
};

use logos::Source;
use token_maps::{PrefixForComment, COMMENTS, KEYWORDS, OPERATORS};
use tokens::Token;

pub type Span = Range<usize>;

#[derive(Debug, Clone)]
pub struct TokenInfo {
    kind: TokenKind,
    span: Span,
}

impl Display for TokenInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {:?}", self.kind, self.span)
    }
}

#[derive(Debug, Clone)]
enum LexGrammarMode {
    Normal,
    String,
    MultilineString,
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    mode: LexGrammarMode,
    token: TokenKind,
}

#[derive(Debug, Clone)]
pub struct Step<'a> {
    pos: usize,
    res: TokenKind,
    mode: LexGrammarMode,
    input: &'a str,
}

impl<'a> Step<'a> {
    fn advance(&self, increment: usize) -> Step<'a> {
        let mut step = self.clone();
        step.pos += increment;
        step
    }

    fn advance_with(&self, increment: usize, kind: TokenKind) -> Step<'a> {
        let mut step = self.advance(increment);
        step.res = kind;
        step
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Operator(Token),
    Keyword(Token),
    Identifier(Token),
    Comment(Token),
    Err,
    Begin,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token = self
            .token()
            .as_ref()
            .map(|t| format!("{t:?}"))
            .unwrap_or("Error".into());
        write!(f, "{token}")
    }
}

impl TokenKind {
    fn token(&self) -> Option<&Token> {
        match self {
            TokenKind::Operator(token) => Some(token),
            TokenKind::Keyword(token) => Some(token),
            TokenKind::Identifier(token) => Some(token),
            TokenKind::Comment(token) => Some(token),
            TokenKind::Err => None,
            TokenKind::Begin => None,
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
            mode: LexGrammarMode::Normal,
            token: TokenKind::Begin,
        }
    }

    fn to_step(&self) -> Step<'a> {
        Step {
            pos: self.pos,
            res: self.token.clone(),
            mode: self.mode.clone(),
            input: self.input,
        }
    }

    fn apply_step(&mut self, step: &Step<'a>) {
        self.pos = step.pos;
        self.token = step.res.clone();
        self.mode = step.mode.clone();
    }

    pub fn spanned(self) -> impl Iterator<Item = TokenInfo> + 'a {
        self.scan(0, |start, step| {
            let next = TokenInfo {
                kind: step.res.clone(),
                span: (*start)..step.pos,
            };
            *start = step.pos;
            Some(next)
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Step<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        // comments
        // strings
        if self.input.len() == self.pos + 1 && matches!(self.token, TokenKind::Operator(Token::EOF))
        {
            return None;
        };

        if self.input.len() == self.pos + 1 {
            let eof_step = self
                .to_step()
                .advance_with(0, TokenKind::Operator(Token::EOF));
            self.apply_step(&eof_step);
            return Some(eof_step);
        };

        let all_parsers = [parse_comment, parse_operator, parse_keyword, err_parser];

        let old_step = self.to_step();

        let step = apply_parsers(&all_parsers, old_step);

        if let Some(step) = step {
            self.apply_step(&step);
            Some(step)
        } else {
            None
        }
    }
}

const OPERATORS_KEY_LENGTH_RANGE: RangeInclusive<u8> = 1..=3;
const KEYWORDS_KEY_LENGTH_RANGE: RangeInclusive<u8> = 2..=11;

fn apply_parsers<'a, 'b>(
    ps: &'b [impl Fn(Step<'a>) -> Option<Step<'a>>],
    step: Step<'a>,
) -> Option<Step<'a>> {
    for p in ps {
        let new_step = (p)(step.clone());
        if new_step.is_some() {
            return new_step;
        }
    }
    None
}

fn err_parser<'a>(step: Step<'a>) -> Option<Step<'a>> {
    let all_parsers = [parse_comment, parse_operator, parse_keyword];

    let mut found = false;
    let mut next_step = step;

    while let None = apply_parsers(&all_parsers, next_step.clone()) {
        found = true;
        next_step = next_step.advance_with(1, TokenKind::Err);
    }

    found.then_some(next_step)
}

fn parse_comment<'a>(step: Step<'a>) -> Option<Step<'a>> {
    // comment prefix can always be identified with the first two chars
    if let Some(key) = &step.input.slice(step.pos..step.pos + 2) {
        if let Some(prefix) = COMMENTS.get(key) {
            match prefix {
                PrefixForComment::ShebangLine => {
                    let new_line_index =
                        step.input[step.pos..].find(|ch| ch == '\u{000A}' || ch == '\u{000D}');
                    if let Some(index) = new_line_index {
                        return Some(
                            step.advance_with(index, TokenKind::Comment(Token::ShebangLine)),
                        );
                    }
                }
                PrefixForComment::DelimitedComment => {
                    let comment_end_idx = step.input[step.pos..].find("*/");
                    if let Some(index) = comment_end_idx {
                        return Some(
                            step.advance_with(
                                index + 2,
                                TokenKind::Comment(Token::DelimitedComment),
                            ),
                        );
                    }
                }
                PrefixForComment::LineComment => {
                    let new_line_index =
                        step.input[step.pos..].find(|ch| ch == '\u{000A}' || ch == '\u{000D}');
                    if let Some(index) = new_line_index {
                        return Some(
                            step.advance_with(index, TokenKind::Comment(Token::LineComment)),
                        );
                    }
                }
            }
        }
    }
    // parse remaining

    None
}

fn parse_operator<'a>(step: Step<'a>) -> Option<Step<'a>> {
    // we prioritize bigger lengths
    for size in OPERATORS_KEY_LENGTH_RANGE.rev() {
        let slice = step.input.slice(step.pos..step.pos + size as usize);

        if let Some(key) = slice {
            let matched = OPERATORS.get(key);
            if let Some(token) = matched {
                return Some(step.advance_with(size.into(), TokenKind::Operator(*token)));
            }
        }
    }
    None
}

fn parse_keyword<'a>(step: Step<'a>) -> Option<Step<'a>> {
    if !step.res.is_operator() {
        return None;
    }

    // we prioritize bigger lengths
    for size in KEYWORDS_KEY_LENGTH_RANGE.rev() {
        if let Some(key) = step.input.slice(step.pos..step.pos + size as usize) {
            let matched = KEYWORDS.get(key);
            let next = matched.zip((parse_operator(step.advance(size.into()))).map(|op| op.res));

            if let Some((token, TokenKind::Operator(_))) = next {
                return Some(step.advance_with(size.into(), TokenKind::Keyword(*token)));
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
        let source = r#"
0444.10_99e+4f
[],--
/* comments */
//line comment
#! sh echo "hey"
hey
fun hello() = "Hello"
var funvar = 3
"#;
        let lex = Lexer::new(source).spanned();
        for lex in lex {
            print!("{} - ", &lex);
            println!("{:?}", &source[lex.span]);
        }
        Ok(())
    }
}
