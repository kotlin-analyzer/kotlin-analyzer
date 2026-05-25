//! Lexing `&str` into a sequence of Kotlin tokens.
//! Note that these tokens, unlike the tokens we feed into the parser, do
//! include info about comments and whitespace.

use std::ops;

use lexer::{Lexer, Token};

use crate::T;
use crate::version::KtVersion;
use crate::{SyntaxKind, SyntaxKind::*};

pub struct LexedStr<'a> {
    text: &'a str,
    kind: Vec<SyntaxKind>,
    start: Vec<u32>,
    error: Vec<LexError>,
}

struct LexError {
    msg: String,
    token: u32,
}

impl<'a> LexedStr<'a> {
    pub fn new(version: KtVersion, text: &'a str) -> LexedStr<'a> {
        let _p = tracing::info_span!("LexedStr::new").entered();
        let mut conv = Converter::new(version, text);
        let lexer = Lexer::new(text).spanned();
        for token_info in lexer {
            let offset = token_info.span().start;
            let token = token_info.token();
            match token {
                Token::ERR => conv.push_err(format!(
                    "Invalid token: {}",
                    conv.res.text[offset..].chars().next().unwrap_or_default()
                )),
                _ => conv.extend_token(token, offset),
            }
        }
        conv.res
    }

    pub fn single_token(version: KtVersion, text: &'a str) -> Option<(SyntaxKind, Option<String>)> {
        if text.is_empty() {
            return None;
        }

        let spanned = Lexer::new(text).spanned_with_src().next()?;
        if spanned.substring().len() != text.len() {
            return None;
        }

        let mut conv = Converter::new(version, text);
        conv.extend_token(spanned.token(), spanned.span().start);

        match &*conv.res.kind {
            [kind] => Some((*kind, conv.res.error.pop().map(|it| it.msg))),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        self.text
    }

    pub fn len(&self) -> usize {
        self.kind.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn kind(&self, i: usize) -> SyntaxKind {
        assert!(i < self.len());
        self.kind[i]
    }

    pub fn text(&self, i: usize) -> &str {
        self.range_text(i..i + 1)
    }

    pub fn range_text(&self, r: ops::Range<usize>) -> &str {
        assert!(r.start < r.end && r.end <= self.len());
        let lo = self.start[r.start] as usize;
        let hi = self.start[r.end] as usize;
        &self.text[lo..hi]
    }

    // Naming is hard.
    pub fn text_range(&self, i: usize) -> ops::Range<usize> {
        assert!(i < self.len());
        let lo = self.start[i] as usize;
        let hi = self.start[i + 1] as usize;
        lo..hi
    }
    pub fn text_start(&self, i: usize) -> usize {
        assert!(i <= self.len());
        self.start[i] as usize
    }
    pub fn text_len(&self, i: usize) -> usize {
        assert!(i < self.len());
        let r = self.text_range(i);
        r.end - r.start
    }

    pub fn error(&self, i: usize) -> Option<&str> {
        assert!(i < self.len());
        let err = self.error.binary_search_by_key(&(i as u32), |i| i.token).ok()?;
        Some(self.error[err].msg.as_str())
    }

    pub fn errors(&self) -> impl Iterator<Item = (usize, &str)> + '_ {
        self.error.iter().map(|it| (it.token as usize, it.msg.as_str()))
    }

    fn push(&mut self, kind: SyntaxKind, offset: usize) {
        self.kind.push(kind);
        self.start.push(offset as u32);
    }
}

struct Converter<'a> {
    res: LexedStr<'a>,
    version: KtVersion,
}

impl<'a> Converter<'a> {
    fn new(version: KtVersion, text: &'a str) -> Self {
        Self {
            res: LexedStr {
                text,
                kind: Vec::with_capacity(text.len() / 3),
                start: Vec::with_capacity(text.len() / 3),
                error: Vec::new(),
            },
            version,
        }
    }

    fn push(&mut self, kind: SyntaxKind, offset: usize) {
        self.res.push(kind, offset);
    }

    fn push_err(&mut self, msg: String) {
        self.res.error.push(LexError { msg, token: self.res.len() as u32 });
    }

    fn extend_token(&mut self, token: &Token, offset: usize) {
        let syntax_kind = {
            match token {
                Token::DELIMITED_COMMENT => DELIMITED_COMMENT,
                Token::LINE_COMMENT => LINE_COMMENT,
                Token::SHEBANG_LINE => SHEBANG,
                Token::WHITESPACE => WHITESPACE,
                Token::NEWLINE => NEWLINE,
                Token::RESERVED => {
                    self.push_err(
                        "invalid range expression `...` (note: use either `..` or `..<`)"
                            .to_string(),
                    );
                    T![..<]
                }
                Token::DOT => T![.],
                Token::COMMA => T![,],
                Token::L_PAREN => T!['('],
                Token::R_PAREN => T![')'],
                Token::L_SQUARE => T!['['],
                Token::R_SQUARE => T![']'],
                Token::L_CURL => T!['{'],
                Token::R_CURL => T!['}'],
                Token::MULT => T![*],
                Token::MOD => T![%],
                Token::DIV => T![/],
                Token::ADD => T![+],
                Token::SUB => T![-],
                Token::INCR => T![++],
                Token::DECR => T![--],
                Token::CONJ => T![&&],
                Token::DISJ => T![||],
                Token::EXCL_WS => T![!],
                Token::EXCL_NO_WS => T![!],
                Token::COLON => T![:],
                Token::SEMICOLON => T![;],
                Token::ASSIGNMENT => T![=],
                Token::ADD_ASSIGNMENT => T![+=],
                Token::SUB_ASSIGNMENT => T![-=],
                Token::MULT_ASSIGNMENT => T![*=],
                Token::DIV_ASSIGNMENT => T![/=],
                Token::MOD_ASSIGNMENT => T![%=],
                Token::ARROW => T![->],
                Token::RANGE => T![..],
                Token::RANGE_UNTIL => T![..<],
                Token::COLON_COLON => T![::],
                Token::AT_NO_WS => T![@],
                Token::AT_POST_WS => T![@],
                Token::AT_PRE_WS => T![@],
                Token::AT_BOTH_WS => T![@],
                Token::QUEST_WS => T![?],
                Token::QUEST_NO_WS => T![?],
                Token::L_ANGLE => T![<],
                Token::R_ANGLE => T![>],
                Token::LE => T![<=],
                Token::GE => T![>=],
                Token::EXCL_EQ => T![!=],
                Token::EXCL_EQ_EQ => T![!==],
                Token::AS_SAFE => T![as?],
                Token::EQ_EQ => T![==],
                Token::EQ_EQ_EQ => T![===],
                Token::SINGLE_QUOTE => todo!(),
                Token::AMP => T![&],
                Token::RETURN_AT => T![return@],
                Token::CONTINUE_AT => T![continue@],
                Token::BREAK_AT => T![break@],
                Token::THIS_AT => T![this@],
                Token::SUPER_AT => T![super@],
                Token::FILE => T![file],
                Token::FIELD => T![field],
                Token::PROPERTY => T![property],
                Token::GET => T![get],
                Token::SET => T![set],
                Token::RECEIVER => T![receiver],
                Token::PARAM => T![param],
                Token::SETPARAM => T![setparam],
                Token::DELEGATE => T![delegate],
                Token::PACKAGE => T![package],
                Token::IMPORT => T![import],
                Token::CLASS => T![class],
                Token::INTERFACE => T![interface],
                Token::FUN => T![fun],
                Token::OBJECT => T![object],
                Token::VAL => T![val],
                Token::VAR => T![var],
                Token::TYPE_ALIAS => T![typealias],
                Token::CONSTRUCTOR => T![constructor],
                Token::BY => T![by],
                Token::COMPANION => T![companion],
                Token::INIT => T![init],
                Token::THIS => T![this],
                Token::SUPER => T![super],
                Token::TYPEOF => T![typeof],
                Token::WHERE => T![where],
                Token::IF => T![if],
                Token::ELSE => T![else],
                Token::WHEN => T![when],
                Token::TRY => T![try],
                Token::CATCH => T![catch],
                Token::FINALLY => T![finally],
                Token::FOR => T![for],
                Token::DO => T![do],
                Token::WHILE => T![while],
                Token::THROW => T![throw],
                Token::RETURN => T![return],
                Token::CONTINUE => T![continue],
                Token::BREAK => T![break],
                Token::AS => T![as],
                Token::IS => T![is],
                Token::IN => T![in],
                Token::NOT_IS => T![!is],
                Token::NOT_IN => T![!in],
                Token::OUT => T![out],
                Token::DYNAMIC => T![dynamic],
                Token::PUBLIC => T![public],
                Token::PRIVATE => T![private],
                Token::PROTECTED => T![protected],
                Token::INTERNAL => T![internal],
                Token::ENUM => T![enum],
                Token::SEALED => T![sealed],
                Token::ANNOTATION => T![annotation],
                Token::DATA => T![data],
                Token::INNER => T![inner],
                Token::TAILREC => T![tailrec],
                Token::OPERATOR => T![operator],
                Token::INLINE => T![inline],
                Token::INFIX => T![infix],
                Token::EXTERNAL => T![external],
                Token::SUSPEND => T![suspend],
                Token::OVERRIDE => T![override],
                Token::ABSTRACT => T![abstract],
                Token::FINAL => T![final],
                Token::OPEN => T![open],
                Token::CONST => T![const],
                Token::LATEINIT => T![lateinit],
                Token::VAR_ARG => T![vararg],
                Token::NO_INLINE => T![noinline],
                Token::CROSS_INLINE => T![crossinline],
                Token::REIFIED => T![reified],
                Token::EXPECT => T![expect],
                Token::ACTUAL => T![actual],
                Token::VALUE => T![value],
                Token::CONTEXT => T![context],
                Token::INTEGER_LITERAL => INT,
                Token::REAL_LITERAL => REAL,
                Token::HEX_LITERAL => HEX,
                Token::BIN_LITERAL => BIN,
                Token::LONG_LITERAL => LONG,
                Token::UNSIGNED_LITERAL => UNSIGNED,
                Token::BOOLEAN_LITERAL => BOOL,
                Token::NULL_LITERAL => NULL_KW,
                Token::CHARACTER_LITERAL => CHAR,
                Token::IDENTIFIER_TOKEN => IDENT,
                Token::QUOTE_OPEN => QUOTE,
                Token::QUOTE_CLOSE => QUOTE,
                Token::TRIPLE_QUOTE_OPEN => TRIPLE_QUOTE,
                Token::TRIPLE_QUOTE_CLOSE => TRIPLE_QUOTE,
                Token::LINE_STR_REF => STR_REF,
                Token::MULTI_LINE_STR_REF => STR_REF,
                Token::MULTI_LINE_STRING_QUOTE => MULTI_LINE_STRING_QUOTE,
                Token::LINE_STR_TEXT => TEXT,
                Token::MULTI_LINE_STR_TEXT => TEXT,
                Token::LINE_STR_ESCAPED_CHAR => ESCAPED_CHAR,
                Token::LINE_STR_EXPR_START => STR_EXPR_START,
                Token::MULTI_STR_EXPR_START => STR_EXPR_START,
                Token::ERR => ERROR,
                Token::EOF => EOF,
            }
        };
        self.push(syntax_kind, offset);
    }
}
