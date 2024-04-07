use std::{
    fmt::Display,
    ops::{Range, RangeInclusive},
};

use logos::Source;
use token_maps::{PrefixForComment, COMMENTS, KEYWORDS, OPERATORS};
use tokens::Token;

#[macro_export]
macro_rules! assert_success {
    ($parser: expr, $source: expr) => {
        $parser(Step::new($source, None)).unwrap();
    };
    ($parser: expr, $source: expr, $pos: expr) => {
        let result = $parser(Step::new($source, None)).unwrap();
        assert_eq!(result.pos, $pos);
    };
    ($parser: expr, $source: expr, $pos: expr, $token: expr) => {
        let result = $parser(Step::new($source, None)).unwrap();
        assert_eq!(result.pos, $pos);
        assert_eq!(result.res.token(), Some(&$token));
    };
}

#[macro_export]
macro_rules! assert_failure {
    ($parser: expr, $source: expr) => {
        let result = $parser(Step::new($source, None));
        assert_eq!(result, None);
    };
}

trait ParseFn<'a>: Fn(Step<'a>) -> Option<Step<'a>> {
    fn with(&self, kind: TokenKind) -> impl ParseFn<'a> {
        move |step: Step<'a>| self(step).map(|s| s.advance_with(0, kind.clone()))
    }
}

impl<'a, F> ParseFn<'a> for F where F: Fn(Step<'a>) -> Option<Step<'a>> {}

pub type Span = Range<usize>;

#[derive(Debug, Clone)]
pub struct TokenInfo {
    kind: TokenKind,
    span: Span,
}

impl Display for TokenInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.kind,
            if self.kind.is_eof() {
                "".into()
            } else {
                format!(" @ {:?}", self.span)
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Step<'a> {
    pos: usize,
    res: TokenKind,
    mode: LexGrammarMode,
    input: &'a str,
}

impl<'a> Step<'a> {
    fn new(input: &'a str, kind: Option<TokenKind>) -> Self {
        match kind {
            Some(token_kind) => Self {
                pos: 0,
                res: token_kind,
                mode: LexGrammarMode::Normal,
                input,
            },
            None => Self {
                pos: 0,
                res: TokenKind::Begin,
                mode: LexGrammarMode::Normal,
                input,
            },
        }
    }
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

    fn find(&self, incr: usize, pat: impl Fn(char) -> bool) -> usize {
        self.input[self.pos + incr..]
            .find(pat)
            .map(|pos| pos + incr)
            .unwrap_or(self.input.len() - self.pos)
    }

    fn find_str_index(&self, incr: usize, pat: &str) -> usize {
        self.input[self.pos + incr..]
            .find(pat)
            .map(|pos| pos + incr)
            .unwrap_or(self.input.len() - self.pos)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Operator(Token),
    Keyword(Token),
    Identifier(Token),
    Comment(Token),
    Literal(Token),
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
            TokenKind::Literal(token) => Some(token),
            TokenKind::Err => None,
            TokenKind::Begin => None,
        }
    }

    fn is_operator(&self) -> bool {
        matches!(self, Self::Operator(_))
    }

    fn is_eof(&self) -> bool {
        matches!(self, Self::Operator(Token::EOF))
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
        // strings
        if self.input.len() <= self.pos + 1 && self.token.is_eof() {
            return None;
        };

        if self.input.len() <= self.pos + 1 {
            let eof_step = self
                .to_step()
                .advance_with(0, TokenKind::Operator(Token::EOF));
            self.apply_step(&eof_step);
            return Some(eof_step);
        };

        let all_parsers = [
            parse_comment,
            parse_operator,
            parse_keyword,
            parse_literals,
            err_parser,
        ];

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

fn apply_parsers<'a, 'b>(ps: &'b [impl ParseFn<'a>], step: Step<'a>) -> Option<Step<'a>> {
    for p in ps {
        let new_step = (p)(step.clone());
        if new_step.is_some() {
            return new_step;
        }
    }
    None
}

fn err_parser(step: Step<'_>) -> Option<Step<'_>> {
    let all_parsers = [parse_comment, parse_operator, parse_keyword, parse_literals];

    let mut found = false;
    let mut next_step = step;

    while apply_parsers(&all_parsers, next_step.clone()).is_none() {
        found = true;
        next_step = next_step.advance_with(1, TokenKind::Err);
    }

    found.then_some(next_step)
}

fn parse_comment(step: Step<'_>) -> Option<Step<'_>> {
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

    None
}

fn parse_operator(step: Step<'_>) -> Option<Step<'_>> {
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

fn parse_keyword(step: Step<'_>) -> Option<Step<'_>> {
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

fn parse_or<'a>(p1: impl ParseFn<'a>, p2: impl ParseFn<'a>) -> impl ParseFn<'a> {
    move |step| {
        if let Some(step) = p1(step.clone()) {
            Some(step)
        } else {
            p2(step)
        }
    }
}

fn bin_or_hex_lit(step: Step<'_>) -> Option<Step<'_>> {
    match step.input.slice(step.pos..step.pos + 2) {
        Some("0b" | "0B") => {
            let index = step.find(2, |ch| ch != '0' && ch != '1' && ch != '_');
            let entry = &step.input[step.pos + 2..step.pos + index];
            if entry.starts_with('_') {
                return None;
            }
            if entry.ends_with('_') {
                return Some(step.advance_with(index - 1, TokenKind::Literal(Token::BinLiteral)));
            }
            Some(step.advance_with(index, TokenKind::Literal(Token::BinLiteral)))
        }
        Some("0x" | "0X") => {
            let index = step.find(
                2,
                |ch| !matches!(ch, '0'..='9' | 'a'..='f' | 'A'..='F' | '_'),
            );

            let entry = &step.input[step.pos + 2..step.pos + index];
            if entry.starts_with('_') {
                return None;
            }
            if entry.ends_with('_') {
                return Some(step.advance_with(index - 1, TokenKind::Literal(Token::HexLiteral)));
            }
            Some(step.advance_with(index, TokenKind::Literal(Token::HexLiteral)))
        }
        _ => None,
    }
}

fn int_lit(step: Step<'_>) -> Option<Step<'_>> {
    let first_char = step
        .input
        .slice(step.pos..step.pos + 1)
        .and_then(|s| s.chars().next());

    match first_char {
        Some(fc @ '0'..='9') => {
            let index = step.find(0, |ch| !ch.is_ascii_digit() && ch != '_');

            let entry = &step.input[step.pos..step.pos + index];

            if entry.ends_with('_') {
                return Some(
                    step.advance_with(index - 1, TokenKind::Literal(Token::IntegerLiteral)),
                );
            }

            if entry.contains('_') && !('1'..'9').contains(&fc) {
                return None;
            }

            if fc == '0' && entry.len() > 1 {
                return None;
            }

            Some(step.advance_with(index, TokenKind::Literal(Token::IntegerLiteral)))
        }
        _ => None,
    }
}

// int_lit but without check for 0 as first char
// TODO: refactor
fn dec_digits(step: Step<'_>) -> Option<Step<'_>> {
    let first_char = step
        .input
        .slice(step.pos..step.pos + 1)
        .and_then(|s| s.chars().next());

    match first_char {
        Some(fc @ '0'..='9') => {
            let index = step.find(0, |ch| !ch.is_ascii_digit() && ch != '_');

            let entry = &step.input[step.pos..step.pos + index];

            if entry.ends_with('_') {
                return Some(
                    step.advance_with(index - 1, TokenKind::Literal(Token::IntegerLiteral)),
                );
            }

            if entry.contains('_') && !('1'..'9').contains(&fc) {
                return None;
            }

            Some(step.advance_with(index, TokenKind::Literal(Token::IntegerLiteral)))
        }
        _ => None,
    }
}

fn tag<'a>(pattern: &'static str) -> impl ParseFn<'a> {
    move |step| {
        if step
            .input
            .slice(step.pos..step.pos + pattern.len())
            .map(|t| t == pattern)
            .unwrap_or_default()
        {
            Some(step.advance_with(pattern.len(), TokenKind::Identifier(Token::Identifier)))
        } else {
            None
        }
    }
}

fn long_lit(step: Step<'_>) -> Option<Step<'_>> {
    parse_and(parse_or(bin_or_hex_lit, int_lit), tag("L"))
        .with(TokenKind::Literal(Token::LongLiteral))(step)
}

fn exponent_lit(step: Step<'_>) -> Option<Step<'_>> {
    parse_and(
        parse_and(
            parse_or(tag("e"), tag("E")),
            parse_opt(parse_or(tag("+"), tag("-"))),
        ),
        dec_digits,
    )(step)
}

fn double_lit(step: Step<'_>) -> Option<Step<'_>> {
    parse_or(
        parse_and(
            parse_and(parse_opt(dec_digits), parse_and(tag("."), dec_digits)),
            parse_opt(exponent_lit),
        ),
        parse_and(dec_digits, exponent_lit),
    )(step)
}

fn real_lit(step: Step<'_>) -> Option<Step<'_>> {
    parse_or(
        parse_and(
            parse_or(double_lit, dec_digits),
            parse_or(tag("f"), tag("F")),
        ),
        double_lit,
    )
    .with(TokenKind::Literal(Token::RealLiteral))(step)
}

fn parse_literals(step: Step<'_>) -> Option<Step<'_>> {
    parse_or(
        real_lit,
        parse_or(long_lit, parse_or(bin_or_hex_lit, int_lit)),
    )(step)
}

fn parse_opt<'a, F>(p: F) -> impl ParseFn<'a>
where
    F: ParseFn<'a>,
{
    move |step| {
        if let Some(step) = p(step.clone()) {
            Some(step)
        } else {
            Some(step)
        }
    }
}

fn parse_and<'a, F, F2>(p1: F, p2: F2) -> impl ParseFn<'a>
where
    F: ParseFn<'a>,
    F2: ParseFn<'a>,
{
    move |step| {
        if let Some(step1) = p1(step) {
            p2(step1)
        } else {
            None
        }
    }
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
0b101_010
0xff_ff
true
false
null
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

    #[test]
    fn combinators() {
        assert_success!(tag("Mine"), "Mineral", 4);
        assert_success!(tag("."), ".Mineral", 1);
        assert_success!(parse_or(tag("Mine"), tag("ral")), "Mineral", 4);

        let and_test = parse_and(tag("Mine"), tag("ral"))(Step::new("Mineral", None)).unwrap();

        assert_eq!(and_test.pos, 7);

        assert_failure!(parse_and(tag("Mine"), tag("gal")), "Mineral");
    }

    #[test]
    fn opt() {
        assert_success!(parse_opt(tag("Mine")), "Mineral", 4);
        assert_success!(parse_and(tag("Mine"), parse_opt(tag("ral"))), "Mineral", 7);
        assert_success!(
            parse_and(parse_opt(tag("Mine")), parse_opt(tag("ral"))),
            "Mineral",
            7
        );
        assert_success!(parse_opt(tag("Nine")), "Mineral", 0);
        assert_success!(parse_and(tag("Mine"), parse_opt(tag("gal"))), "Mineral", 4);
        assert_success!(
            parse_and(parse_opt(tag("Hi")), parse_opt(tag("Mine"))),
            "Mineral",
            4
        );
        assert_success!(parse_and(parse_opt(tag("Hi")), tag("Mine")), "Mineral", 4);

        assert_success!(parse_or(tag("Mine"), parse_opt(tag("ral"))), "Mineral", 4);
        assert_success!(
            parse_or(parse_opt(tag("Mine")), parse_opt(tag("ral"))),
            "Mineral",
            4
        );
        assert_success!(parse_opt(tag("Nine")), "Mineral", 0);
        assert_success!(parse_or(tag("Mine"), parse_opt(tag("gal"))), "Mineral", 4);
        assert_success!(
            parse_or(parse_opt(tag("Hi")), parse_opt(tag("Mine"))),
            "Mineral",
            0
        );
        assert_success!(parse_and(parse_opt(tag("Hi")), tag("Mine")), "Mineral", 4);
    }

    #[test]
    fn int_literals() {
        assert_success!(int_lit, "23419", 5, Token::IntegerLiteral);
        assert_success!(int_lit, "2_341_567", 9, Token::IntegerLiteral);
        assert_success!(int_lit, "2_341_567r", 9, Token::IntegerLiteral);
        assert_success!(int_lit, "0");
        assert_success!(int_lit, "2_341_567_");

        assert_failure!(int_lit, "_2_341_567");
        assert_failure!(int_lit, "0_341_567");
        assert_failure!(int_lit, "u2341");
        assert_failure!(int_lit, "0444");
    }

    #[test]
    fn hex_bin_literals() {
        assert_success!(bin_or_hex_lit, "0b101_010", 9, Token::BinLiteral);
        assert_success!(bin_or_hex_lit, "0xff_ff_bb", 10, Token::HexLiteral);

        assert_success!(bin_or_hex_lit, "0b101_010r", 9, Token::BinLiteral);
        assert_success!(bin_or_hex_lit, "0xff_ff_bbr", 10, Token::HexLiteral);

        assert_success!(bin_or_hex_lit, "0b101_010\n", 9, Token::BinLiteral);
        assert_success!(bin_or_hex_lit, "0xff_ff_bbL\n", 10, Token::HexLiteral);

        assert_success!(bin_or_hex_lit, "0b101_010_");
        assert_success!(bin_or_hex_lit, "0xff_ff_bb_");

        assert_failure!(bin_or_hex_lit, "_0b101_010");
    }

    #[test]
    fn long_literals() {
        assert_success!(long_lit, "23419L", 6, Token::LongLiteral);
        assert_success!(long_lit, "2_341_567L", 10, Token::LongLiteral);

        assert_failure!(long_lit, "23419");
        assert_failure!(long_lit, "2_341_567");

        assert_success!(long_lit, "0b101_010L", 10, Token::LongLiteral);
        assert_success!(long_lit, "0xff_ff_bbL", 11, Token::LongLiteral);

        assert_failure!(long_lit, "0b101_010\n");
        assert_failure!(long_lit, "0xff_ff_bb\n");

        assert_failure!(long_lit, "0b101_010");
        assert_failure!(long_lit, "0xff_ff_bb");
        assert_failure!(long_lit, "0xff_ff");
    }

    #[test]
    fn double_literal() {
        assert_success!(double_lit, "0444.10_99e+4f", 13);
        assert_success!(double_lit, "3333.4e+3f", 9);
        assert_success!(double_lit, "3e+4f", 4);
        assert_success!(double_lit, ".444f", 4);
        assert_success!(double_lit, "366_39338e+4f", 12);
        assert_success!(double_lit, "38.38_390f", 9);
        assert_success!(double_lit, ".445_444f", 8);
        assert_success!(double_lit, "45.44e-940", 10);
        assert_success!(double_lit, "45.44e+940_", 10);
        assert_success!(double_lit, "01e+10", 6);

        assert_failure!(double_lit, "666.");
        assert_failure!(double_lit, "23419");
        assert_failure!(double_lit, "2_341_567");
    }
    #[test]
    fn real_literal() {
        assert_success!(real_lit, "0444.10_99e+4f", 14);
        assert_success!(real_lit, "3333.4e+3f", 10);
        assert_success!(real_lit, "3e+4f", 5);
        assert_success!(real_lit, ".444f", 5);
        assert_success!(real_lit, "366_39338e+4f", 13);
        assert_success!(real_lit, "38.38_390f", 10);
        assert_success!(real_lit, ".445_444f", 9);
        assert_success!(real_lit, "45.44e-940", 10);
        assert_success!(real_lit, "45.44e+940_", 10);

        assert_failure!(real_lit, "666.");
        assert_failure!(real_lit, "23419");
        assert_failure!(real_lit, "2_341_567");
    }

    #[test]
    fn literals() {
        assert_success!(parse_literals, "23419L", 6, Token::LongLiteral);
        assert_success!(parse_literals, "2_341_567L", 10, Token::LongLiteral);

        assert_success!(parse_literals, "23419", 5, Token::IntegerLiteral);
        assert_success!(parse_literals, "2_341_567", 9, Token::IntegerLiteral);

        assert_success!(parse_literals, "0b101_010L", 10, Token::LongLiteral);
        assert_success!(parse_literals, "0xff_ff_bbL", 11, Token::LongLiteral);

        assert_success!(parse_literals, "0b101_010\n", 9, Token::BinLiteral);
        assert_success!(parse_literals, "0xff_ff_bb\n", 10, Token::HexLiteral);

        assert_success!(parse_literals, "0b101_010", 9, Token::BinLiteral);
        assert_success!(parse_literals, "0xff_ff_bb", 10, Token::HexLiteral);

        assert_success!(parse_literals, "38.38_390f", 10, Token::RealLiteral);
        assert_success!(parse_literals, ".445_444f", 9, Token::RealLiteral);
        assert_success!(parse_literals, "45.44e-940", 10, Token::RealLiteral);
    }
}
