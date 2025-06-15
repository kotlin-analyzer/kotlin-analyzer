mod indent;

use lady_deirdre::{
    lexis::TokenSet,
    syntax::{NodeRef, Recovery, RecoveryResult, SyntaxError, SyntaxSession, EMPTY_NODE_SET},
};

use crate::{syntax::KotlinNode, tokens::KotlinToken};

pub(super) fn skip_trivia<'a>(session: &mut impl SyntaxSession<'a, Node = KotlinNode>) {
    loop {
        let token = session.token(0);

        if token != KotlinToken::Whitespace {
            break;
        }

        session.advance();
    }
}

pub(super) static NEWLINE_TOKEN_SET: TokenSet = TokenSet::inclusive(&[KotlinToken::Newline as u8]);
pub(super) static NEWLINE_RECOVERY: Recovery =
    Recovery::unlimited().unexpected_set(NEWLINE_TOKEN_SET);

pub(super) static SHEBANG_TOKEN_SET: TokenSet = TokenSet::inclusive(&[KotlinToken::Hash as u8]);
pub(super) static COMMENT_TOKEN_SET: TokenSet = TokenSet::inclusive(&[KotlinToken::Div as u8]);
pub(super) static CLOSING_COMMENT_TOKEN_SET: TokenSet =
    TokenSet::inclusive(&[KotlinToken::Mult as u8]);
// static ALL_TOKEN_SET: TokenSet = TokenSet::all();

pub fn parse_shebang_line<'a>(
    session: &mut impl SyntaxSession<'a, Node = KotlinNode>,
) -> KotlinNode {
    let parent = session.parent_ref();
    skip_trivia(session);

    loop {
        match (session.token(0), session.token(1)) {
            (KotlinToken::Hash, KotlinToken::ExclNoWs) => {
                session.skip(2);
                skip_trivia(session);

                // skip until the end of the line
                while session.token(0) != KotlinToken::Newline
                    && session.token(0) != KotlinToken::EOI
                {
                    session.advance();
                }

                // Expect a newline after the shebang line
                if session.token(0) == KotlinToken::EOI {
                    // report failure if we reach the end of input without a newline
                    let start_ref = session.site_ref(0);
                    let end_ref = session.end_site_ref();

                    session.failure(SyntaxError {
                        span: start_ref..end_ref,
                        context: KotlinNode::SHEBANG_LINE,
                        recovery: RecoveryResult::UnexpectedEOI,
                        expected_tokens: &NEWLINE_TOKEN_SET,
                        expected_nodes: &EMPTY_NODE_SET,
                    });

                    return KotlinNode::ShebangLine {
                        node: NodeRef::nil(),
                        parent,
                    };
                }

                // skip all the newlines after the shebang line
                while session.token(0) == KotlinToken::Newline {
                    session.advance();
                }

                let node = session.node_ref();

                return KotlinNode::ShebangLine { node, parent };
            }
            _ => {
                let start_site = session.site_ref(0);
                let result = NEWLINE_RECOVERY.recover(session, &SHEBANG_TOKEN_SET);
                let end_site_ref = session.site_ref(0);

                session.failure(SyntaxError {
                    span: start_site..end_site_ref,
                    context: KotlinNode::SHEBANG_LINE,
                    recovery: result,
                    expected_tokens: &SHEBANG_TOKEN_SET,
                    expected_nodes: &EMPTY_NODE_SET,
                });

                if !result.recovered() {
                    return KotlinNode::ShebangLine {
                        node: NodeRef::nil(),
                        parent,
                    };
                }
            }
        }
    }
}

pub fn parse_line_comment<'a>(
    session: &mut impl SyntaxSession<'a, Node = KotlinNode>,
) -> KotlinNode {
    skip_trivia(session);

    if session.token(0) == KotlinToken::LineCommentStart {
        session.advance();
        while session.token(0) != KotlinToken::Newline && session.token(0) != KotlinToken::EOI {
            session.advance();
        }

        let node = session.node_ref();
        let parent = session.parent_ref();

        return KotlinNode::LineComment { node, parent };
    }

    let start_site = session.site_ref(0);
    let result = NEWLINE_RECOVERY.recover(session, &CLOSING_COMMENT_TOKEN_SET);
    let end_site = session.site_ref(0);

    session.failure(SyntaxError {
        span: start_site..end_site,
        context: KotlinNode::LINE_COMMENT,
        recovery: result,
        expected_tokens: &COMMENT_TOKEN_SET,
        expected_nodes: &EMPTY_NODE_SET,
    });

    let node = session.node_ref();
    let parent = session.parent_ref();

    KotlinNode::LineComment { node, parent }
}

pub fn parse_delimited_comment<'a>(
    session: &mut impl SyntaxSession<'a, Node = KotlinNode>,
) -> KotlinNode {
    let parent = session.parent_ref();
    skip_trivia(session);
    let mut matched = 0;

    loop {
        if session.token(0) == KotlinToken::DelimitedCommentStart {
            matched += 1;
            session.advance();

            while !(session.token(0) == KotlinToken::Mult && session.token(1) == KotlinToken::Div)
                && session.token(0) != KotlinToken::DelimitedCommentStart
                && session.token(0) != KotlinToken::EOI
            {
                session.advance();
            }

            if session.token(0) == KotlinToken::DelimitedCommentStart {
                continue;
            }

            if matched > 0 && session.token(0) != KotlinToken::EOI {
                matched -= 1;
                session.skip(2); // Skip the closing `*/`

                if matched == 0 {
                    let node = session.node_ref();
                    return KotlinNode::DelimitedComment { node, parent };
                }

                continue;
            }
        }
        break;
    }

    let start_site = session.site_ref(0);
    let result = NEWLINE_RECOVERY.recover(session, &COMMENT_TOKEN_SET);

    let end_site_ref = session.site_ref(0);

    session.failure(SyntaxError {
        span: start_site..end_site_ref,
        context: KotlinNode::DELIMITED_COMMENT,
        recovery: result,
        expected_tokens: &COMMENT_TOKEN_SET,
        expected_nodes: &EMPTY_NODE_SET,
    });

    let node = session.node_ref();
    KotlinNode::DelimitedComment { node, parent }
}
