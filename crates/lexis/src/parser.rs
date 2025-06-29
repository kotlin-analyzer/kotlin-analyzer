// mod flexible;
// mod indent;
// pub mod types;

use lady_deirdre::{
    lexis::{SiteRef, Token, TokenCount, TokenCursor, TokenRule},
    syntax::SyntaxSession,
};

use crate::{syntax::KotlinNode, tokens::KotlinToken};

#[allow(dead_code)]
pub(super) trait CustomParser<'session> {
    fn session(&mut self) -> &mut impl SyntaxSession<'session, Node = KotlinNode>;
    fn step_start_ref(&mut self) -> &mut SiteRef;

    fn skip_trivia(&mut self) {
        *(self.step_start_ref()) = self.session().site_ref(0);
        loop {
            match self.session().token(0) {
                KotlinToken::Whitespace => {
                    self.session().advance();
                }
                KotlinToken::LineCommentStart => {
                    self.session().descend(KotlinNode::LINE_COMMENT);
                }
                KotlinToken::DelimitedCommentStart => {
                    self.session().descend(KotlinNode::DELIMITED_COMMENT);
                }
                _ => break,
            }
        }
    }

    fn match_all<const N: usize>(&mut self, rules: [TokenRule; N]) -> [bool; N] {
        let mut result = [false; N];
        for (rule, entry) in rules.iter().zip(result.iter_mut()) {
            self.skip_trivia();
            *entry = self.session().token(0).rule() == *rule;
        }
        result
    }

    fn tokens_no_advance<const N: usize>(
        &mut self,
        skip_newlines: bool,
    ) -> [(KotlinToken, usize); N] {
        let mut result = [(KotlinToken::EOI, 0); N];
        let mut start_index = 0;

        for entry in result.iter_mut() {
            let (idx, token) = self.token_skipping_trivia(start_index, skip_newlines);
            *entry = (token, idx);
            start_index = idx;
        }
        result
    }

    fn match_any(&mut self, rules: &[TokenRule]) -> bool {
        self.skip_trivia();
        rules
            .iter()
            .all(|rule| self.session().token(0).rule() == *rule)
    }

    fn skip_all(&mut self, rule: TokenRule) {
        self.skip_trivia();
        while let token = self.session().token(0)
            && token.rule() == rule
            && token != KotlinToken::EOI
        {
            self.skip_trivia();
            self.session().advance();
        }
    }

    fn token_skipping(
        &mut self,
        skipping: KotlinToken,
        start_index: TokenCount,
    ) -> (usize, KotlinToken) {
        let mut index = start_index;
        while let token = self.session().token(index)
            && token == skipping
            && token != KotlinToken::EOI
        {
            index += 1;
        }
        (index + 1, self.session().token(index))
    }

    /// Gets the index of a token from a start index without advancing the session.
    /// The index return is from the current session cursor.
    fn find_token_from(
        &mut self,
        token: KotlinToken,
        start_index: TokenCount,
    ) -> (usize, KotlinToken) {
        let mut index = start_index;
        while let t = self.session().token(index)
            && t != KotlinToken::EOI
            && t != token
        {
            index += 1;
        }
        (index + 1, self.session().token(index))
    }

    /// Gets the index of a token, while counting the occurence of another.
    /// Returns the count and the index of target token. Returns 0 if token is not found.
    /// Note that this function does not advance the session.
    fn count_token_until(
        &mut self,
        counted: KotlinToken,
        until: KotlinToken,
        start_index: TokenCount,
    ) -> (usize, TokenCount) {
        let mut end = start_index;
        let mut count: usize = 0;
        while let token = self.session().token(end)
            && token != KotlinToken::EOI
            && token != until
        {
            end += 1;
            if token == counted {
                count += 1;
            }
        }
        (count, end)
    }

    /// Gets the next token, skipping trivia without advancing the session.
    /// Returns the index of the token and the token.
    fn token_skipping_trivia(
        &mut self,
        start_index: TokenCount,
        skip_newlines: bool,
    ) -> (usize, KotlinToken) {
        let mut index = start_index;
        // Had to reimplement trivia skipping here, but one that doesn't advance the session.
        loop {
            match self.session().token(index) {
                KotlinToken::Whitespace => {
                    index += 1;
                }
                KotlinToken::LineCommentStart => {
                    index = self.find_token_from(KotlinToken::NL, index).0;
                }
                KotlinToken::DelimitedCommentStart => {
                    index += 1;
                    let mut remaining = 1;
                    while remaining > 0 {
                        remaining -= 1;
                        let (rem, idx) = self.count_token_until(
                            KotlinToken::DelimitedCommentStart,
                            KotlinToken::DelimitedCommentEnd,
                            index,
                        );
                        remaining += rem;
                        index = idx + 1; // skip */
                    }
                }
                KotlinToken::NL if skip_newlines => {
                    index += 1;
                }
                token => return (index, token),
            }
        }
    }
}
