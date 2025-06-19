mod indent;
pub mod types;

use lady_deirdre::{
    lexis::TokenSet,
    syntax::{NodeRef, Recovery, SyntaxError, SyntaxSession, EMPTY_NODE_SET},
};

use crate::{syntax::KotlinNode, tokens::KotlinToken};
use finl_unicode::categories::CharacterCategories;

// pub(super) fn skip_trivia<'a>(session: &mut impl SyntaxSession<'a, Node = KotlinNode>) {
//     loop {
//         let token = session.token(0);

//         if token != KotlinToken::Whitespace {
//             break;
//         }

//         session.advance();
//     }
// }

pub(super) static IDENT_STOP_TOKEN_SET: TokenSet =
    TokenSet::inclusive(&[KotlinToken::NL as u8, KotlinToken::Whitespace as u8]);
pub(super) static SURROUND_TOKEN_SET: TokenSet = TokenSet::inclusive(&[
    KotlinToken::LCurl as u8,
    KotlinToken::RCurl as u8,
    KotlinToken::LSquare as u8,
    KotlinToken::RSquare as u8,
    KotlinToken::LParen as u8,
    KotlinToken::RParen as u8,
    KotlinToken::LAngle as u8,
    KotlinToken::RAngle as u8,
]);

pub(super) static SURROUNDED_RECOVERY: Recovery =
    Recovery::unlimited().unexpected_set(SURROUND_TOKEN_SET);

pub fn parse_non_ascii_identifier<'a>(
    session: &mut impl SyntaxSession<'a, Node = KotlinNode>,
) -> KotlinNode {
    static TICK_TOKEN_SET: TokenSet = TokenSet::inclusive(&[KotlinToken::AsciiIdentifier as u8]);

    if let KotlinToken::MisMatch = session.token(0)
        && let Some(mut chars) = session.string(0).map(str::chars)
        && chars
            .next()
            .map(|c| c == '_' || c.is_letter())
            .unwrap_or_default()
        && chars.all(|c| c == '_' || c.is_letter() || c.is_number_decimal())
    {
        session.advance(); // skip first

        while let Some(mut chars) = session.string(0).map(str::chars)
            && chars.all(|c| c == '_' || c.is_letter() || c.is_number_decimal())
        {
            session.advance();
        }

        let node = session.node_ref();
        let parent = session.parent_ref();
        return KotlinNode::NonAsciiIdentifier { node, parent };
    }

    let start_site = session.site_ref(0);
    let result = SURROUNDED_RECOVERY.recover(session, &IDENT_STOP_TOKEN_SET);
    let end_site = session.site_ref(0);

    session.failure(SyntaxError {
        span: start_site..end_site,
        context: KotlinNode::NON_ASCII_IDENTIFIER,
        recovery: result,
        expected_tokens: &TICK_TOKEN_SET,
        expected_nodes: &EMPTY_NODE_SET,
    });

    let parent = session.parent_ref();
    return KotlinNode::NonAsciiIdentifier {
        node: NodeRef::nil(),
        parent,
    };
}
