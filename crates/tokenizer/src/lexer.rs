#![deny(clippy::index_refutable_slice)]
#![deny(clippy::indexing_slicing)]

use std::{
    fmt::Display,
    ops::{Range, RangeInclusive},
};

use logos::Source;
use token_maps::{KEYWORDS, OPERATORS};
use tokens::Token;
use unicode_categories::UnicodeCategories;

trait ParseFn<'a>: Fn(Step<'a>) -> Option<Step<'a>> {
    #[inline]
    fn with(&self, token: Token) -> impl ParseFn<'a> {
        move |step: Step<'a>| self(step).map(|s| s.advance_with(0, token))
    }

    #[inline]
    fn and(&self, p: impl ParseFn<'a>) -> impl ParseFn<'a> {
        and(self, p)
    }

    #[inline]
    fn or(&self, p: impl ParseFn<'a>) -> impl ParseFn<'a> {
        or(self, p)
    }
}

trait CharExt {
    fn is_kotlin_letter(&self) -> bool;
    fn can_start_ident(&self) -> bool;
}

impl CharExt for char {
    fn is_kotlin_letter(&self) -> bool {
        self.is_letter_lowercase()
            || self.is_letter_uppercase()
            || self.is_letter_titlecase()
            || self.is_letter_modifier()
    }

    fn can_start_ident(&self) -> bool {
        self.is_kotlin_letter() || self.is_number_decimal_digit()
    }
}

impl<'a, F> ParseFn<'a> for F where F: Fn(Step<'a>) -> Option<Step<'a>> {}

/// This type represents the span of a token.
pub type Span = Range<usize>;

/// This type contains information about a token including its kind and span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenInfo {
    token: Token,
    span: Span,
}

impl TokenInfo {
    pub fn new(token: Token, span: Span) -> Self {
        Self { token, span }
    }
    pub fn token(&self) -> &Token {
        &self.token
    }
}

impl Display for TokenInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token::{:?}{}",
            self.token,
            if matches!(self.token, Token::EOF) {
                "".into()
            } else {
                format!(" => {:?}", self.span)
            }
        )
    }
}

/// This represents the grammar mode of the lexer. The three states are:
/// - Normal: The default mode for all non string tokens in the syntax
/// - String: The mode for single line string literals
/// - MultilineString: The mode for multiline string literals
#[derive(Debug, Clone, PartialEq)]
pub enum LexGrammarMode {
    Normal,
    String,
    MultilineString,
}

/// This is the main lexer that contains the input string and the current position.
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    prev_mode: LexGrammarMode,
    mode: LexGrammarMode,
    token: Token,
}

/// This is a single step in the lexer that contains the current position,
/// the result of the tokenization, the current mode and the previous mode.
#[derive(Debug, Clone, PartialEq)]
pub struct Step<'a> {
    pos: usize,
    res: Token,
    mode: LexGrammarMode,
    prev_mode: LexGrammarMode,
    input: &'a str,
}

impl<'a> Step<'a> {
    #[allow(dead_code)]
    fn new(input: &'a str, token: Option<Token>) -> Self {
        match token {
            Some(token) => Self {
                pos: 0,
                res: token,
                prev_mode: LexGrammarMode::Normal,
                mode: LexGrammarMode::Normal,
                input,
            },
            None => Self {
                pos: 0,
                // fine to start with Err here, since we won't return it
                res: Token::Err,
                mode: LexGrammarMode::Normal,
                prev_mode: LexGrammarMode::Normal,
                input,
            },
        }
    }
    fn advance(&self, increment: usize) -> Step<'a> {
        let mut step = self.clone();
        step.pos += increment;
        step
    }

    fn advance_with(&self, increment: usize, kind: Token) -> Step<'a> {
        let mut step = self.advance(increment);
        step.res = kind;
        step
    }

    fn set_mode(&mut self, mode: LexGrammarMode) {
        self.prev_mode = self.mode.clone();
        self.mode = mode;
    }

    fn find(&self, incr: usize, pat: impl Fn(char) -> bool) -> usize {
        if let Some(pos) = self.input.get(self.pos + incr..).and_then(|s| s.find(pat)) {
            pos + incr
        } else {
            self.input.len() - self.pos
        }
    }

    fn next_char(&self) -> Option<char> {
        self.input
            .get(self.pos..self.pos + 1)
            .and_then(|s| s.chars().next())
    }

    fn prev_char(&self) -> Option<char> {
        self.input
            .get(self.pos - 1..self.pos)
            .and_then(|s| s.chars().next())
    }
}

impl<'a> Lexer<'a> {
    #[allow(dead_code)]
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            mode: LexGrammarMode::Normal,
            prev_mode: LexGrammarMode::Normal,
            // fine to start with Err here, it is replaced and never used
            token: Token::Err,
        }
    }

    fn to_step(&self) -> Step<'a> {
        Step {
            pos: self.pos,
            res: self.token.clone(),
            prev_mode: self.prev_mode.clone(),
            mode: self.mode.clone(),
            input: self.input,
        }
    }

    fn apply_step(&mut self, step: &Step<'a>) {
        self.pos = step.pos;
        self.token = step.res.clone();
        self.prev_mode = step.prev_mode.clone();
        self.mode = step.mode.clone();
    }

    /// This method returns an iterator over the tokens in the input string.
    /// alongside their spans
    pub fn spanned(self) -> impl Iterator<Item = TokenInfo> + 'a {
        self.scan(0, |start, step| {
            let next = TokenInfo {
                token: step.res,
                span: (*start)..step.pos,
            };
            *start = step.pos;
            Some(next)
        })
    }

    fn tokenize_next(&self) -> Option<Step<'a>> {
        let step = self.to_step();
        match self.mode {
            LexGrammarMode::Normal => wrap_with_err(
                parse_comment
                    .or(parse_operator)
                    .or(parse_keyword)
                    .or(parse_literals)
                    .or(parse_identifier),
            )(step),
            LexGrammarMode::String => wrap_with_err(
                str_ref.or(str_expr_start
                    .or(line_str_escaped_char)
                    .or(line_str_text)
                    .or(quote_close)),
            )(step),
            LexGrammarMode::MultilineString => wrap_with_err(
                str_ref
                    .or(str_expr_start)
                    .or(multi_line_str_text)
                    .or(multi_line_string_quote)
                    .or(triple_quote_close)
                    .or(quote_open),
            )(step),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Step<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        // strings
        if self.input.len() <= self.pos + 1 && matches!(self.token, Token::EOF) {
            return None;
        };

        if self.input.len() <= self.pos + 1 {
            let eof_step = self.to_step().advance_with(0, Token::EOF);
            self.apply_step(&eof_step);
            return Some(eof_step);
        };

        if let Some(step) = self.tokenize_next() {
            self.apply_step(&step);
            Some(step)
        } else {
            None
        }
    }
}

const OPERATORS_KEY_LENGTH_RANGE: RangeInclusive<u8> = 1..=3;
const KEYWORDS_KEY_LENGTH_RANGE: RangeInclusive<u8> = 2..=11;

fn apply_parsers_seq<'a, 'b>(ps: &'b [impl ParseFn<'a>], step: Step<'a>) -> Option<Step<'a>> {
    let mut step = step;
    for p in ps {
        if let Some(new_step) = (p)(step) {
            step = new_step;
        } else {
            return None;
        }
    }
    Some(step)
}

fn wrap_with_err<'a>(parser: impl ParseFn<'a>) -> impl ParseFn<'a> {
    move |step| {
        let mut found = false;
        let mut next_step = step;
        loop {
            if !(next_step.pos < next_step.input.len()) {
                break;
            }
            match parser(next_step.clone()) {
                Some(step) => {
                    if !found {
                        next_step = step;
                    }
                    break;
                }
                None => {
                    found = true;
                    next_step = next_step.advance_with(1, Token::Err);
                }
            }
        }

        Some(next_step)
    }
}

fn parse_comment(step: Step<'_>) -> Option<Step<'_>> {
    if let Some(step) = shebang.or(line_comment).or(delimited_comment)(step) {
        match step.res {
            Token::DelimitedComment | Token::LineComment => {
                opt(opt(hidden.or(nl)).and(tag("@")).with(Token::AtPreWs))(step)
            }
            _ => Some(step),
        }
    } else {
        None
    }
}

fn parse_operator(step: Step<'_>) -> Option<Step<'_>> {
    // we prioritize bigger lengths
    for size in OPERATORS_KEY_LENGTH_RANGE.rev() {
        let slice = step.input.slice(step.pos..step.pos + size as usize);

        if let Some(key) = slice {
            let matched = OPERATORS.get(key);
            if let Some(token) = matched {
                return handle_operator(step.advance_with(size.into(), *token), token);
            }
        }
    }
    None
}

fn unicode_char_lit(step: Step<'_>) -> Option<Step<'_>> {
    tag("\\u").and(repeat::<4>(
        char_range('0'..='9')
            .or(char_range('a'..='f'))
            .or(char_range('A'..='F')),
    ))(step)
}

fn escaped_identifier(step: Step<'_>) -> Option<Step<'_>> {
    tag("\\").and(
        tag("n")
            .or(tag("t"))
            .or(tag("\\"))
            .or(tag("'"))
            .or(tag("\""))
            .or(tag("$"))
            .or(tag("r"))
            .or(tag("b")),
    )(step)
}

fn handle_operator<'a>(mut step: Step<'a>, token: &Token) -> Option<Step<'a>> {
    match token {
        Token::SingleQuote => unicode_char_lit
            .or(escaped_identifier)
            .or(not(
                tag("'").or(tag("\u{000A}").or(tag("\u{000D}")).or(tag("\\")))
            ))
            .and(tag("'"))
            .with(Token::CharacterLiteral)(step),
        Token::QuoteOpen => {
            step.set_mode(LexGrammarMode::String);
            Some(step)
        }
        Token::TripleQuoteOpen => {
            step.mode = LexGrammarMode::MultilineString;
            Some(step)
        }
        Token::ExclNoWs => opt(hidden.with(Token::ExclWs))(step),
        Token::AtNoWs => opt(hidden.or(nl).with(Token::AtPostWs))(step),
        Token::QuestNoWs => opt(hidden.or(nl).with(Token::QuestWs))(step),
        Token::Nl | Token::Ws => {
            // TODO: handle multiple hidden | nl before
            opt(tag("@")
                .with(Token::AtPreWs)
                .and(opt(hidden.with(Token::AtBothWs))))(step)
        }
        _ => Some(step),
    }
}

fn parse_keyword(step: Step<'_>) -> Option<Step<'_>> {
    if step
        .prev_char()
        .map(|ch| ch.can_start_ident())
        .unwrap_or_default()
    {
        return None;
    }

    // we prioritize bigger lengths
    for size in KEYWORDS_KEY_LENGTH_RANGE.rev() {
        if let Some(key) = step.input.slice(step.pos..step.pos + size as usize) {
            let next_is_ident = step
                .advance(size.into())
                .next_char()
                .map(|ch| ch.can_start_ident())
                .unwrap_or_default();

            if next_is_ident {
                return None;
            }

            if let Some(token) = KEYWORDS.get(key) {
                return handle_keyword(step.advance_with(size.into(), *token), token);
            }
        }
    }

    None
}

fn handle_keyword<'a>(step: Step<'a>, token: &Token) -> Option<Step<'a>> {
    match token {
        Token::This => opt(tag("@").and(parse_identifier).with(Token::ThisAt))(step),
        Token::Super => opt(tag("@").and(parse_identifier).with(Token::SuperAt))(step),
        Token::Return => opt(tag("@").and(parse_identifier).with(Token::ReturnAt))(step),
        Token::Break => opt(tag("@").and(parse_identifier).with(Token::BreakAt))(step),
        Token::Continue => opt(tag("@").and(parse_identifier).with(Token::ContinueAt))(step),
        Token::NotIn => opt(hidden)(step),
        Token::NotIs => opt(hidden)(step),
        _ => Some(step),
    }
}

fn ws(step: Step<'_>) -> Option<Step<'_>> {
    tag("\u{0020}")
        .or(tag("\u{0009}"))
        .or(tag("\u{000C}"))
        .with(Token::Ws)(step)
}

fn nl(step: Step<'_>) -> Option<Step<'_>> {
    tag("\u{000A}")
        .or(tag("\u{000D}").and(opt(tag("\u{000A}"))))
        .with(Token::Ws)(step)
}

fn hidden(step: Step<'_>) -> Option<Step<'_>> {
    many(ws.or(line_comment).or(delimited_comment))(step)
}

fn shebang(step: Step<'_>) -> Option<Step<'_>> {
    tag("#!")
        .and(many0(not(tag("\u{000A}").or(tag("\u{000D}")))))
        .with(Token::ShebangLine)(step)
}

fn line_comment(step: Step<'_>) -> Option<Step<'_>> {
    tag("//")
        .and(many0(not(tag("\u{000A}").or(tag("\u{000D}")))))
        .with(Token::LineComment)(step)
}

fn delimited_comment(step: Step<'_>) -> Option<Step<'_>> {
    tag("/*")
        .and(many0(delimited_comment.or(not(tag("*/")))))
        .and(tag("*/"))
        .with(Token::DelimitedComment)(step)
}

#[inline]
fn or<'a>(p1: impl ParseFn<'a>, p2: impl ParseFn<'a>) -> impl ParseFn<'a> {
    move |step| {
        if let Some(step) = p1(step.clone()) {
            Some(step)
        } else {
            p2(step)
        }
    }
}

#[inline]
fn not<'a>(p: impl ParseFn<'a>) -> impl ParseFn<'a> {
    move |step| {
        if !(step.pos < step.input.len()) {
            return None;
        }
        if p(step.clone()).is_some() {
            None
        } else {
            Some(step.advance(1))
        }
    }
}

fn bin_or_hex_lit(step: Step<'_>) -> Option<Step<'_>> {
    match step.input.slice(step.pos..step.pos + 2) {
        Some("0b" | "0B") => {
            let index = step.find(2, |ch| ch != '0' && ch != '1' && ch != '_');
            let entry = &step.input.get(step.pos + 2..step.pos + index)?;

            if entry.starts_with('_') {
                return None;
            }
            if entry.ends_with('_') {
                return Some(step.advance_with(index - 1, Token::BinLiteral));
            }
            Some(step.advance_with(index, Token::BinLiteral))
        }
        Some("0x" | "0X") => {
            let index = step.find(
                2,
                |ch| !matches!(ch, '0'..='9' | 'a'..='f' | 'A'..='F' | '_'),
            );

            let entry = &step.input.get(step.pos + 2..step.pos + index)?;

            if entry.starts_with('_') {
                return None;
            }
            if entry.ends_with('_') {
                return Some(step.advance_with(index - 1, Token::HexLiteral));
            }
            Some(step.advance_with(index, Token::HexLiteral))
        }
        _ => None,
    }
}

fn int_lit(step: Step<'_>) -> Option<Step<'_>> {
    match step.next_char() {
        Some(fc @ '0'..='9') => {
            let index = step.find(0, |ch| !ch.is_ascii_digit() && ch != '_');

            let entry = &step.input.get(step.pos..step.pos + index)?;

            if entry.ends_with('_') {
                return Some(step.advance_with(index - 1, Token::IntegerLiteral));
            }

            if entry.contains('_') && !('1'..'9').contains(&fc) {
                return None;
            }

            if fc == '0' && entry.len() > 1 {
                return None;
            }

            Some(step.advance_with(index, Token::IntegerLiteral))
        }
        _ => None,
    }
}

// int_lit but without check for 0 as first char
// TODO: refactor
fn dec_digits(step: Step<'_>) -> Option<Step<'_>> {
    match step.next_char() {
        Some('0'..='9') => {
            let index = step.find(0, |ch| !ch.is_ascii_digit() && ch != '_');

            let entry = &step.input.get(step.pos..step.pos + index)?;

            if entry.ends_with('_') {
                return Some(step.advance_with(index - 1, Token::IntegerLiteral));
            }

            Some(step.advance_with(index, Token::IntegerLiteral))
        }
        _ => None,
    }
}

#[inline]
fn tag<'a>(pattern: &'static str) -> impl ParseFn<'a> {
    move |step| {
        if step
            .input
            .slice(step.pos..step.pos + pattern.len())
            .map(|t| t == pattern)
            .unwrap_or_default()
        {
            Some(step.advance_with(pattern.len(), Token::Identifier))
        } else {
            None
        }
    }
}

#[inline]
fn when<'a>(condition: impl Fn(char) -> bool) -> impl ParseFn<'a> {
    move |step| {
        if step.next_char().map(&condition).unwrap_or_default() {
            Some(step.advance(1))
        } else {
            None
        }
    }
}

/// This matches multiple entities of the same type one or more times.
/// For zero or more times, use `many0`
#[inline]
fn many<'a>(p: impl ParseFn<'a>) -> impl ParseFn<'a> {
    move |step| {
        let start = step.pos;
        let mut next_step = step;
        while let Some(step) = p(next_step.clone()) {
            next_step = step;
            if !(next_step.pos < next_step.input.len()) {
                break;
            }
        }
        if start == next_step.pos {
            None
        } else {
            Some(next_step)
        }
    }
}

/// This matches multiple entities of the same type zero or more times.
/// For one or more times, use `many`
#[inline]
fn many0<'a>(p: impl ParseFn<'a>) -> impl ParseFn<'a> {
    opt(many(p))
}

fn char_range<'a>(range: RangeInclusive<char>) -> impl ParseFn<'a> {
    move |step| {
        if step
            .input
            .get(step.pos..step.pos + 1)
            .and_then(|s| Some(range.contains(&s.chars().next()?)))
            .unwrap_or_default()
        {
            Some(step.advance(1))
        } else {
            None
        }
    }
}

#[inline]
fn repeat<'a, const N: usize>(p1: impl ParseFn<'a>) -> impl ParseFn<'a> {
    move |step| apply_parsers_seq(&[&p1; N], step)
}

fn long_lit(step: Step<'_>) -> Option<Step<'_>> {
    and(or(bin_or_hex_lit, int_lit), tag("L")).with(Token::LongLiteral)(step)
}

fn exponent_lit(step: Step<'_>) -> Option<Step<'_>> {
    and(
        and(tag("e").or(tag("E")), opt(or(tag("+"), tag("-")))),
        dec_digits,
    )(step)
}

fn letter(step: Step<'_>) -> Option<Step<'_>> {
    tag("_")
        .or(when(|ch| ch.is_letter_lowercase()))
        .or(when(|ch| ch.is_letter_uppercase()))
        .or(when(|ch| ch.is_letter_titlecase()))
        .or(when(|ch| ch.is_letter_modifier()))(step)
}

fn quoted_symbol<'a>(step: Step<'a>) -> Option<Step<'a>> {
    tag("`")
        .and(many0(not(tag("\u{000A}").or(tag("\u{000D}").or(tag("`"))))))
        .and(tag("`"))(step)
}

fn parse_identifier<'a>(step: Step<'a>) -> Option<Step<'a>> {
    quoted_symbol
        .or(letter.and(many0(letter.or(when(|ch| ch.is_number_decimal_digit())))))
        .with(Token::Identifier)(step)
}

fn double_lit(step: Step<'_>) -> Option<Step<'_>> {
    or(
        and(
            and(opt(dec_digits), and(tag("."), dec_digits)),
            opt(exponent_lit),
        ),
        and(dec_digits, exponent_lit),
    )(step)
}

fn real_lit(step: Step<'_>) -> Option<Step<'_>> {
    or(
        and(or(double_lit, dec_digits), or(tag("f"), tag("F"))),
        double_lit,
    )
    .with(Token::RealLiteral)(step)
}

fn parse_literals(step: Step<'_>) -> Option<Step<'_>> {
    or(real_lit, or(long_lit, or(bin_or_hex_lit, int_lit)))(step)
}

#[inline]
fn opt<'a, F>(p: F) -> impl ParseFn<'a>
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

#[inline]
fn and<'a, F, F2>(p1: F, p2: F2) -> impl ParseFn<'a>
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

fn str_ref(step: Step<'_>) -> Option<Step<'_>> {
    tag("$").and(parse_identifier).with(match step.mode {
        LexGrammarMode::String => Token::LineStrRef,
        LexGrammarMode::MultilineString => Token::MultiLineStrRef,
        LexGrammarMode::Normal => unreachable!("only called in string mode"),
    })(step)
}

fn str_expr_start(step: Step<'_>) -> Option<Step<'_>> {
    tag("${").and(parse_identifier).with(match step.mode {
        LexGrammarMode::String => Token::LineStrExprStart,
        LexGrammarMode::MultilineString => Token::MultiStrExprStart,
        LexGrammarMode::Normal => unreachable!("only called in string mode"),
    })(step)
}

fn line_str_text(step: Step<'_>) -> Option<Step<'_>> {
    many(not(tag("\\").or(tag("\"")).or(tag("$"))))
        .or(tag("$"))
        .with(Token::LineStrText)(step)
}

fn multi_line_str_text(step: Step<'_>) -> Option<Step<'_>> {
    many(not(tag("\"").or(tag("$"))))
        .or(tag("$"))
        .with(Token::MultiLineStrText)(step)
}

fn quote_close(step: Step<'_>) -> Option<Step<'_>> {
    if let Some(mut step) = tag("\"").with(Token::QuoteClose)(step) {
        step.mode = step.prev_mode.clone();
        return Some(step);
    }
    None
}

fn quote_open(step: Step<'_>) -> Option<Step<'_>> {
    if let Some(mut step) = tag("\"").with(Token::QuoteOpen)(step) {
        step.set_mode(LexGrammarMode::String);
        return Some(step);
    }
    None
}

fn multi_line_string_quote(step: Step<'_>) -> Option<Step<'_>> {
    let origin = step.clone();
    if let Some(step) = tag(r#"""""#).and(many(tag("\"")))(step) {
        let det = step.pos - origin.pos;
        if det >= 6 {
            return Some(origin.advance_with(det - 3, Token::MultiLineStringQuote));
        }
    }
    None
}

fn triple_quote_close(step: Step<'_>) -> Option<Step<'_>> {
    if let Some(mut step) = opt(multi_line_string_quote).and(tag(r#"""""#))(step) {
        step.mode = LexGrammarMode::Normal;
        step.res = Token::TripleQuoteClose;
        return Some(step);
    }
    None
}

fn line_str_escaped_char(step: Step<'_>) -> Option<Step<'_>> {
    escaped_identifier.or(unicode_char_lit)(step)
}

#[cfg(test)]
mod playground {
    use std::error::Error;

    use macros::multiline_str;

    use super::*;

    #[test]
    #[ignore]
    fn simple() -> Result<(), Box<dyn Error>> {
        let source = multiline_str!(
            r#"
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
            'A'
            '\uffac'
            '\n'
            this
            this@me
            super
            !in//comment
            /* pre */@man
            name@ // post
              @  // both
            !is/* comment */
            var name: String?/*ddjjd*/ = null;
            var name2: String? /*ddjjd*/ // = null;
            super@me
            continue
            continue@where
            return
            return@here
            break
            break@now
            `backticks baby`
            fun hello() = "Hello"
            var funvar = 3
            """
            simple multi line
            """
            """
            complex "multi line"
            """"""
            "#
        );

        let lex = Lexer::new(&source).spanned();
        for lex in lex {
            print!("{} - ", &lex);
            println!("{:?}", &source[lex.span]);
        }
        Ok(())
    }
}
#[cfg(test)]
mod test {
    use macros::{assert_failure, assert_success, multiline_str};

    use super::*;

    #[test]
    fn nested_str_test() {
        let source = r#""hey ${echo("test")} stranger""#;
        let lex = Lexer::new(&source).spanned();
        for lex in lex {
            print!("{} - ", &lex);
            println!("{:?}", &source[lex.span]);
        }
    }

    #[test]
    fn delimited_comment_test() {
        assert_success!(delimited_comment, "/**/", 4, Token::DelimitedComment);
        assert_success!(delimited_comment, "/**\n*/", 6, Token::DelimitedComment);
        assert_success!(
            delimited_comment,
            "/* comment */",
            13,
            Token::DelimitedComment
        );
        assert_success!(
            delimited_comment,
            "/* comment /* inner comment */ */",
            33,
            Token::DelimitedComment
        );
        assert_success!(
            delimited_comment,
            "/* comment /* inner /* deep */ */ */",
            36,
            Token::DelimitedComment
        );

        assert_failure!(delimited_comment, "/* comment /* inner /* deep */ */");
        assert_failure!(delimited_comment, "/* comment");
        assert_failure!(delimited_comment, "/* comment /* inner comment */");
    }

    #[test]
    fn shebang_test() {
        assert_success!(shebang, "#!", 2, Token::ShebangLine);
        assert_success!(shebang, "#!\n", 2, Token::ShebangLine);
        assert_success!(shebang, "#! sh echo", 10, Token::ShebangLine);
        assert_success!(shebang, "#! comment // nested", 20, Token::ShebangLine);
        assert_success!(
            shebang,
            "#! comment // nested #! deep /* more */",
            39,
            Token::ShebangLine
        );

        assert_failure!(shebang, "// comment");
        assert_failure!(shebang, "/* comment */");
    }

    #[test]
    fn line_comment_test() {
        assert_success!(line_comment, "//", 2, Token::LineComment);
        assert_success!(line_comment, "//\n", 2, Token::LineComment);
        assert_success!(line_comment, "// line comment", 15, Token::LineComment);
        assert_success!(line_comment, "// comment // nested", 20, Token::LineComment);
        assert_success!(
            line_comment,
            "// comment // nested /* delimited */",
            36,
            Token::LineComment
        );

        assert_failure!(line_comment, "/* comment");
        assert_failure!(line_comment, "#! shebang");
    }

    #[test]
    fn quoted_symbol_test() {
        assert_success!(quoted_symbol, "`backticks baby`", 16);
    }
    #[test]
    fn identifier_test() {
        assert_success!(parse_identifier, "`backticks baby`", 16);
        assert_success!(parse_identifier, "hello", 5);
    }

    #[test]
    fn char_literal() {
        assert_success!(parse_operator, "'A'", 3);
    }
    #[test]
    fn many_test() {
        assert_success!(many(tag("A")), "A", 1);
        assert_success!(many(tag("A")), "AAA", 3);
        assert_success!(many(tag("A")), "AAAB", 3);

        assert_failure!(many(tag("BA")), "A");
    }

    #[test]
    fn many0_test() {
        assert_success!(many0(tag("A")), "A", 1);
        assert_success!(many0(tag("A")), "AAA", 3);
        assert_success!(many0(tag("A")), "AAAB", 3);

        assert_success!(many0(tag("BA")), "A", 0);
    }

    #[test]
    fn combinators() {
        assert_success!(tag("Mine"), "Mineral", 4);
        assert_success!(tag("."), ".Mineral", 1);
        assert_success!(or(tag("Mine"), tag("ral")), "Mineral", 4);

        let and_test = and(tag("Mine"), tag("ral"))(Step::new("Mineral", None)).unwrap();

        assert_eq!(and_test.pos, 7);

        assert_failure!(and(tag("Mine"), tag("gal")), "Mineral");

        assert_success!(not(tag("Mince")), "Mineral");
        assert_failure!(not(tag("Mince")), "Mincer of words");
    }

    #[test]
    fn opt_test() {
        assert_success!(opt(tag("Mine")), "Mineral", 4);
        assert_success!(and(tag("Mine"), opt(tag("ral"))), "Mineral", 7);
        assert_success!(and(tag("Mine"), opt(tag("ral"))), "Minefield", 4);
        assert_success!(and(opt(tag("Mine")), opt(tag("ral"))), "Mineral", 7);
        assert_success!(opt(tag("Nine")), "Mineral", 0);
        assert_success!(and(tag("Mine"), opt(tag("gal"))), "Mineral", 4);
        assert_success!(and(opt(tag("Hi")), opt(tag("Mine"))), "Mineral", 4);
        assert_success!(and(opt(tag("Hi")), tag("Mine")), "Mineral", 4);

        assert_success!(or(tag("Mine"), opt(tag("ral"))), "Mineral", 4);
        assert_success!(or(opt(tag("Mine")), opt(tag("ral"))), "Mineral", 4);
        assert_success!(opt(tag("Nine")), "Mineral", 0);
        assert_success!(or(tag("Mine"), opt(tag("gal"))), "Mineral", 4);
        assert_success!(or(opt(tag("Hi")), opt(tag("Mine"))), "Mineral", 0);
        assert_success!(and(opt(tag("Hi")), tag("Mine")), "Mineral", 4);
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
        assert_failure!(int_lit, "023");
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
        assert_success!(double_lit, "03_33.4e+3f", 10);
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
