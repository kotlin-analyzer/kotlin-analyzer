#![deny(clippy::index_refutable_slice)]
#![deny(clippy::arithmetic_side_effects)]
#![deny(clippy::indexing_slicing)]

use std::{
    collections::VecDeque,
    fmt::Display,
    ops::{Range, RangeInclusive},
};

use tokens::Token::{self, *};
use tokens::{get_keyword, OPERATORS};
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
    fn can_be_in_ident(&self) -> bool;
}

impl CharExt for char {
    fn is_kotlin_letter(&self) -> bool {
        self.is_letter_lowercase()
            || self.is_letter_uppercase()
            || self.is_letter_titlecase()
            || self.is_letter_modifier()
    }

    fn can_be_in_ident(&self) -> bool {
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
        write!(f, "{:?} => {:?}", self.token, self.span)
    }
}

/// This represents the grammar mode of the lexer. The three states are:
/// - Normal: The default mode for all non string tokens in the syntax
/// - String: The mode for single line string literals
/// - MultilineString: The mode for multiline string literals
#[derive(Debug, Clone, PartialEq, Default)]
enum LexGrammarMode {
    #[default]
    Normal,
    String,
    MultilineString,
}

/// This is the main lexer that contains the input string and the current position.
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    step: Step<'a>,
}

#[derive(Debug, Clone, PartialEq, Default)]
struct Mode(VecDeque<LexGrammarMode>);

impl Mode {
    fn peek(&self) -> &LexGrammarMode {
        self.0.back().unwrap_or(&LexGrammarMode::Normal)
    }

    fn pop(&mut self) {
        self.0.pop_back();
    }

    fn set(&mut self, mode: LexGrammarMode) {
        self.0.push_back(mode)
    }
}

/// This is a single step in the lexer that contains the current position,
/// the result of the tokenization, the current mode and the previous mode.
#[derive(Debug, Clone, PartialEq)]
struct Step<'a> {
    pos: usize,
    res: Token,
    mode: Mode,
    input: &'a str,
}

impl<'a> Step<'a> {
    #[cfg(test)]
    fn new(input: &'a str, token: Option<Token>) -> Self {
        match token {
            Some(token) => Self {
                pos: 0,
                res: token,
                mode: Mode::default(),
                input,
            },
            None => Self {
                pos: 0,
                // fine to start with Err here, since we won't use it
                res: ERR,
                mode: Mode::default(),
                input,
            },
        }
    }
    fn advance(&self, increment: usize) -> Step<'a> {
        let mut step = self.clone();
        if let Some(pos) = step.pos.checked_add(increment) {
            step.pos = pos;
        }
        step
    }

    fn advance_with(&self, increment: usize, kind: Token) -> Step<'a> {
        let mut step = self.advance(increment);
        step.res = kind;
        step
    }

    fn get_till_end(&self, incr: usize) -> Option<&'a str> {
        self.pos
            .checked_add(incr)
            .and_then(|start| self.input.get(start..))
    }

    fn get_until(&self, incr: usize) -> Option<&'a str> {
        self.pos
            .checked_add(incr)
            .and_then(|end| self.input.get(self.pos..end))
    }

    fn find(&self, incr: usize, pat: impl Fn(char) -> bool) -> usize {
        if let Some(pos) = self.get_till_end(incr).and_then(|s| s.find(pat)) {
            pos.checked_add(incr).unwrap_or_default()
        } else {
            self.input.len().checked_sub(self.pos).unwrap_or_default()
        }
    }

    fn next_char(&self) -> Option<char> {
        self.pos
            .checked_add(1)
            .and_then(|end| self.input.get(self.pos..end).and_then(|s| s.chars().next()))
    }

    fn prev_char(&self) -> Option<char> {
        self.pos.checked_sub(1).and_then(|start| {
            self.input
                .get(start..self.pos)
                .and_then(|s| s.chars().next())
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpannedWithSource<'a> {
    token: Token,
    span: Span,
    substring: &'a str,
}

impl SpannedWithSource<'_> {
    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn substring(&self) -> &str {
        self.substring
    }

    pub fn is_keyword(&self) -> bool {
        tokens::is_keyword(self.substring())
    }

    pub fn is_soft_keyword(&self) -> bool {
        tokens::is_soft_keyword(self.substring())
    }

    pub fn is_operator(&self) -> bool {
        tokens::is_operator(self.substring())
    }
}

impl Display for SpannedWithSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"{:?} => {:?}, // {:?}"#,
            self.token(),
            self.span(),
            self.substring()
        )
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            step: Step {
                input,
                pos: 0,
                mode: Mode::default(),
                // fine to start with Err here, it is replaced and never used
                res: ERR,
            },
        }
    }

    fn apply_step(&mut self, step: Step<'a>) {
        self.step = step;
    }

    /// This method returns an iterator over the tokens in the input string.
    /// alongside their spans
    pub fn spanned(self) -> impl Iterator<Item = TokenInfo> + 'a {
        self.scan(0, |start, step| {
            let next = TokenInfo {
                token: step.0,
                span: (*start)..step.1,
            };
            *start = step.1;
            Some(next)
        })
    }

    pub fn spanned_with_src(self) -> impl Iterator<Item = SpannedWithSource<'a>> + 'a {
        let input = self.step.input;
        self.spanned()
            .map(|TokenInfo { token, span }| SpannedWithSource {
                token,
                span: span.clone(),
                substring: input.get(span).unwrap_or_default(),
            })
    }

    fn tokenize_next(&self) -> Option<Step<'a>> {
        let step = self.step.clone();
        match self.step.mode.peek() {
            LexGrammarMode::Normal => wrap_with_err(
                parse_comment
                    .or(parse_operator)
                    .or(parse_keyword)
                    .or(parse_literals)
                    .or(parse_identifier),
            )(step),
            LexGrammarMode::String => wrap_with_err(
                str_ref
                    .or(str_expr_start)
                    .or(line_str_escaped_char)
                    .or(line_str_text)
                    .or(quote_close),
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

impl Iterator for Lexer<'_> {
    type Item = (Token, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.step.input.len() == self.step.pos && matches!(self.step.res, EOF) {
            return None;
        };

        if self.step.input.len() == self.step.pos {
            let eof_step = self.step.advance_with(0, EOF);
            self.apply_step(eof_step);
            return Some((self.step.res, self.step.pos));
        };

        if let Some(step) = self.tokenize_next() {
            self.apply_step(step);
            Some((self.step.res, self.step.pos))
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
            if next_step.pos >= next_step.input.len() {
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
                    next_step = next_step.advance_with(1, ERR);
                }
            }
        }

        Some(next_step)
    }
}

fn parse_comment(step: Step<'_>) -> Option<Step<'_>> {
    if let Some(step) = shebang.or(line_comment).or(delimited_comment)(step) {
        match step.res {
            DELIMITED_COMMENT | LINE_COMMENT => {
                opt(opt(hidden.or(nl)).and(tag("@")).with(AT_PRE_WS))(step)
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
        let slice = step.get_until(size.into());

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
        SINGLE_QUOTE => unicode_char_lit
            .or(escaped_identifier)
            .or(not(
                tag("'").or(tag("\u{000A}").or(tag("\u{000D}")).or(tag("\\")))
            ))
            .and(tag("'"))
            .with(CHARACTER_LITERAL)(step),
        QUOTE_OPEN => {
            step.mode.set(LexGrammarMode::String);
            Some(step)
        }
        TRIPLE_QUOTE_OPEN => {
            step.mode.set(LexGrammarMode::MultilineString);
            Some(step)
        }
        EXCL_NO_WS => opt(hidden.with(EXCL_WS))(step),
        AT_NO_WS => opt(hidden.or(nl).with(AT_POST_WS))(step),
        QUEST_NO_WS => opt(hidden.or(nl).with(QUEST_WS))(step),
        NL | WS => {
            // TODO: handle multiple hidden | nl before
            opt(tag("@").with(AT_PRE_WS).and(opt(hidden.with(AT_BOTH_WS))))(step)
        }
        R_CURL => {
            // special case for string interpolation:
            // whenever we see a }, we pop lexer mode queue
            step.mode.pop();
            Some(step)
        }
        _ => Some(step),
    }
}

fn parse_keyword(step: Step<'_>) -> Option<Step<'_>> {
    if step
        .prev_char()
        .map(|ch| ch.can_be_in_ident())
        .unwrap_or_default()
    {
        return None;
    }

    // we prioritize bigger lengths
    for size in KEYWORDS_KEY_LENGTH_RANGE.rev() {
        if let Some(key) = step.get_until(size.into()) {
            if step
                .advance(size.into())
                .next_char()
                .map(|ch| ch.can_be_in_ident())
                .unwrap_or_default()
            {
                continue;
            }

            if let Some(token) = get_keyword(key) {
                return handle_keyword(step.advance_with(size.into(), *token), token);
            }
        }
    }

    None
}

fn handle_keyword<'a>(step: Step<'a>, token: &Token) -> Option<Step<'a>> {
    match token {
        THIS => opt(tag("@").and(parse_identifier).with(THIS_AT))(step),
        SUPER => opt(tag("@").and(parse_identifier).with(SUPER_AT))(step),
        RETURN => opt(tag("@").and(parse_identifier).with(RETURN_AT))(step),
        BREAK => opt(tag("@").and(parse_identifier).with(BREAK_AT))(step),
        CONTINUE => opt(tag("@").and(parse_identifier).with(CONTINUE_AT))(step),
        NOT_IN => opt(hidden)(step),
        NOT_IS => opt(hidden)(step),
        _ => Some(step),
    }
}

fn ws(step: Step<'_>) -> Option<Step<'_>> {
    tag("\u{0020}")
        .or(tag("\u{0009}"))
        .or(tag("\u{000C}"))
        .with(WS)(step)
}

fn nl(step: Step<'_>) -> Option<Step<'_>> {
    tag("\u{000A}")
        .or(tag("\u{000D}").and(opt(tag("\u{000A}"))))
        .with(WS)(step)
}

fn hidden(step: Step<'_>) -> Option<Step<'_>> {
    many(ws.or(line_comment).or(delimited_comment))(step)
}

fn shebang(step: Step<'_>) -> Option<Step<'_>> {
    tag("#!")
        .and(many0(not(tag("\u{000A}").or(tag("\u{000D}")))))
        .with(SHEBANG_LINE_TOKEN)(step)
}

fn line_comment(step: Step<'_>) -> Option<Step<'_>> {
    tag("//")
        .and(many0(not(tag("\u{000A}").or(tag("\u{000D}")))))
        .with(LINE_COMMENT)(step)
}

fn delimited_comment(step: Step<'_>) -> Option<Step<'_>> {
    tag("/*")
        .and(many0(delimited_comment.or(not(tag("*/")))))
        .and(tag("*/"))
        .with(DELIMITED_COMMENT)(step)
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
        if step.pos >= step.input.len() {
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
    match step.get_until(2) {
        Some("0b" | "0B") => {
            let index = step.find(2, |ch| ch != '0' && ch != '1' && ch != '_');
            let entry = step
                .pos
                .checked_add(2)
                .zip(step.pos.checked_add(index))
                .and_then(|(start, end)| step.input.get(start..end))?;

            if entry.starts_with('_') {
                return None;
            }
            if entry.ends_with('_') {
                return index
                    .checked_sub(1)
                    .map(|incr| step.advance_with(incr, BIN_LITERAL));
            }
            Some(step.advance_with(index, BIN_LITERAL))
        }
        Some("0x" | "0X") => {
            let index = step.find(
                2,
                |ch| !matches!(ch, '0'..='9' | 'a'..='f' | 'A'..='F' | '_'),
            );

            let entry = step
                .pos
                .checked_add(2)
                .zip(step.pos.checked_add(index))
                .and_then(|(start, end)| step.input.get(start..end))?;

            if entry.starts_with('_') {
                return None;
            }
            if entry.ends_with('_') {
                return index
                    .checked_sub(1)
                    .map(|incr| step.advance_with(incr, HEX_LITERAL));
            }
            Some(step.advance_with(index, HEX_LITERAL))
        }
        _ => None,
    }
}

fn int_lit(step: Step<'_>) -> Option<Step<'_>> {
    match step.next_char() {
        Some(fc @ '0'..='9') => {
            let index = step.find(0, |ch| !ch.is_ascii_digit() && ch != '_');

            let entry = &step.get_until(index)?;

            if entry.ends_with('_') {
                return index
                    .checked_sub(1)
                    .map(|incr| step.advance_with(incr, INTEGER_LITERAL));
            }

            if entry.contains('_') && !('1'..'9').contains(&fc) {
                return None;
            }

            if fc == '0' && entry.len() > 1 {
                return None;
            }

            Some(step.advance_with(index, INTEGER_LITERAL))
        }
        _ => None,
    }
}

fn dec_digits(step: Step<'_>) -> Option<Step<'_>> {
    match step.next_char() {
        Some('0'..='9') => {
            let index = step.find(0, |ch| !ch.is_ascii_digit() && ch != '_');

            let entry = &step.get_until(index)?;

            if entry.ends_with('_') {
                return index
                    .checked_sub(1)
                    .map(|incr| step.advance_with(incr, INTEGER_LITERAL));
            }

            Some(step.advance_with(index, INTEGER_LITERAL))
        }
        _ => None,
    }
}

#[inline]
fn tag<'a>(pattern: &'static str) -> impl ParseFn<'a> {
    move |step| {
        if step
            .get_until(pattern.len())
            .map(|t| t == pattern)
            .unwrap_or_default()
        {
            Some(step.advance_with(pattern.len(), IDENTIFIER))
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
            if next_step.pos >= next_step.input.len() {
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
            .get_until(1)
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
    and(or(bin_or_hex_lit, int_lit), tag("L")).with(LONG_LITERAL)(step)
}

fn exponent_lit(step: Step<'_>) -> Option<Step<'_>> {
    and(
        and(tag("e").or(tag("E")), opt(or(tag("+"), tag("-")))),
        dec_digits,
    )(step)
}

fn letter(step: Step<'_>) -> Option<Step<'_>> {
    tag("_").or(when(|ch| ch.is_kotlin_letter()))(step)
}

fn quoted_symbol(step: Step<'_>) -> Option<Step<'_>> {
    tag("`")
        .and(many0(not(tag("\u{000A}").or(tag("\u{000D}").or(tag("`"))))))
        .and(tag("`"))(step)
}

fn parse_identifier(step: Step<'_>) -> Option<Step<'_>> {
    quoted_symbol
        .or(letter.and(many0(when(|ch| ch.can_be_in_ident()))))
        .with(IDENTIFIER)(step)
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
    .with(REAL_LITERAL)(step)
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
    tag("$").and(parse_identifier).with(match step.mode.peek() {
        LexGrammarMode::String => LINE_STR_REF,
        LexGrammarMode::MultilineString => MULTI_LINE_STR_REF,
        LexGrammarMode::Normal => unreachable!("only called in string mode"),
    })(step)
}

fn str_expr_start(mut step: Step<'_>) -> Option<Step<'_>> {
    tag(r"${").with(match step.mode.peek() {
        LexGrammarMode::String => {
            step.mode.set(LexGrammarMode::Normal);
            LINE_STR_EXPR_START
        }
        LexGrammarMode::MultilineString => {
            step.mode.set(LexGrammarMode::Normal);
            MULTI_STR_EXPR_START
        }
        LexGrammarMode::Normal => unreachable!("only called in string mode"),
    })(step)
}

fn line_str_text(step: Step<'_>) -> Option<Step<'_>> {
    many(not(tag("\\").or(tag("\"")).or(tag("$"))))
        .or(tag("$"))
        .with(LINE_STR_TEXT)(step)
}

fn multi_line_str_text(step: Step<'_>) -> Option<Step<'_>> {
    many(not(tag("\"").or(tag("$"))))
        .or(tag("$"))
        .with(MULTI_LINE_STR_TEXT)(step)
}

fn quote_close(step: Step<'_>) -> Option<Step<'_>> {
    if let Some(mut step) = tag("\"").with(QUOTE_CLOSE)(step) {
        step.mode.pop();
        return Some(step);
    }
    None
}

fn quote_open(step: Step<'_>) -> Option<Step<'_>> {
    if let Some(mut step) = tag("\"").with(QUOTE_OPEN)(step) {
        step.mode.set(LexGrammarMode::String);
        return Some(step);
    }
    None
}

fn multi_line_string_quote(step: Step<'_>) -> Option<Step<'_>> {
    let origin = step.clone();
    if let Some(step) = tag(r#"""""#).and(many(tag("\"")))(step) {
        let (det, incr) = step
            .pos
            .checked_sub(origin.pos)
            .and_then(|det| det.checked_sub(3).map(|incr| (det, incr)))?;
        if det >= 6 {
            return Some(origin.advance_with(incr, MULTI_LINE_STRING_QUOTE));
        }
    }
    None
}

fn triple_quote_close(step: Step<'_>) -> Option<Step<'_>> {
    if let Some(mut step) = opt(multi_line_string_quote).and(tag(r#"""""#))(step) {
        step.mode.set(LexGrammarMode::Normal);
        step.res = TRIPLE_QUOTE_CLOSE;
        return Some(step);
    }
    None
}

fn line_str_escaped_char(step: Step<'_>) -> Option<Step<'_>> {
    escaped_identifier.or(unicode_char_lit)(step)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{assert_failure, assert_success};

    #[test]
    fn delimited_comment_test() {
        assert_success!(delimited_comment, "/**/", 4, DELIMITED_COMMENT);
        assert_success!(delimited_comment, "/**\n*/", 6, DELIMITED_COMMENT);
        assert_success!(delimited_comment, "/* comment */", 13, DELIMITED_COMMENT);
        assert_success!(
            delimited_comment,
            "/* comment /* inner comment */ */",
            33,
            DELIMITED_COMMENT
        );
        assert_success!(
            delimited_comment,
            "/* comment /* inner /* deep */ */ */",
            36,
            DELIMITED_COMMENT
        );

        assert_failure!(delimited_comment, "/* comment /* inner /* deep */ */");
        assert_failure!(delimited_comment, "/* comment");
        assert_failure!(delimited_comment, "/* comment /* inner comment */");
    }

    #[test]
    fn shebang_test() {
        assert_success!(shebang, "#!", 2, SHEBANG_LINE_TOKEN);
        assert_success!(shebang, "#!\n", 2, SHEBANG_LINE_TOKEN);
        assert_success!(shebang, "#! sh echo", 10, SHEBANG_LINE_TOKEN);
        assert_success!(shebang, "#! comment // nested", 20, SHEBANG_LINE_TOKEN);
        assert_success!(
            shebang,
            "#! comment // nested #! deep /* more */",
            39,
            SHEBANG_LINE_TOKEN
        );

        assert_failure!(shebang, "// comment");
        assert_failure!(shebang, "/* comment */");
    }

    #[test]
    fn line_comment_test() {
        assert_success!(line_comment, "//", 2, LINE_COMMENT);
        assert_success!(line_comment, "//\n", 2, LINE_COMMENT);
        assert_success!(line_comment, "// line comment", 15, LINE_COMMENT);
        assert_success!(line_comment, "// comment // nested", 20, LINE_COMMENT);
        assert_success!(
            line_comment,
            "// comment // nested /* delimited */",
            36,
            LINE_COMMENT
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
        assert_success!(int_lit, "23419", 5, INTEGER_LITERAL);
        assert_success!(int_lit, "2_341_567", 9, INTEGER_LITERAL);
        assert_success!(int_lit, "2_341_567r", 9, INTEGER_LITERAL);
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
        assert_success!(bin_or_hex_lit, "0b101_010", 9, BIN_LITERAL);
        assert_success!(bin_or_hex_lit, "0xff_ff_bb", 10, HEX_LITERAL);

        assert_success!(bin_or_hex_lit, "0b101_010r", 9, BIN_LITERAL);
        assert_success!(bin_or_hex_lit, "0xff_ff_bbr", 10, HEX_LITERAL);

        assert_success!(bin_or_hex_lit, "0b101_010\n", 9, BIN_LITERAL);
        assert_success!(bin_or_hex_lit, "0xff_ff_bbL\n", 10, HEX_LITERAL);

        assert_success!(bin_or_hex_lit, "0b101_010_");
        assert_success!(bin_or_hex_lit, "0xff_ff_bb_");

        assert_failure!(bin_or_hex_lit, "_0b101_010");
    }

    #[test]
    fn long_literals() {
        assert_success!(long_lit, "23419L", 6, LONG_LITERAL);
        assert_success!(long_lit, "2_341_567L", 10, LONG_LITERAL);

        assert_failure!(long_lit, "23419");
        assert_failure!(long_lit, "2_341_567");

        assert_success!(long_lit, "0b101_010L", 10, LONG_LITERAL);
        assert_success!(long_lit, "0xff_ff_bbL", 11, LONG_LITERAL);

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
        assert_success!(parse_literals, "23419L", 6, LONG_LITERAL);
        assert_success!(parse_literals, "2_341_567L", 10, LONG_LITERAL);

        assert_success!(parse_literals, "23419", 5, INTEGER_LITERAL);
        assert_success!(parse_literals, "2_341_567", 9, INTEGER_LITERAL);

        assert_success!(parse_literals, "0b101_010L", 10, LONG_LITERAL);
        assert_success!(parse_literals, "0xff_ff_bbL", 11, LONG_LITERAL);

        assert_success!(parse_literals, "0b101_010\n", 9, BIN_LITERAL);
        assert_success!(parse_literals, "0xff_ff_bb\n", 10, HEX_LITERAL);

        assert_success!(parse_literals, "0b101_010", 9, BIN_LITERAL);
        assert_success!(parse_literals, "0xff_ff_bb", 10, HEX_LITERAL);

        assert_success!(parse_literals, "38.38_390f", 10, REAL_LITERAL);
        assert_success!(parse_literals, ".445_444f", 9, REAL_LITERAL);
        assert_success!(parse_literals, "45.44e-940", 10, REAL_LITERAL);
    }
}
