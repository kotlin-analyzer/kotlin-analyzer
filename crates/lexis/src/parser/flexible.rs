use lady_deirdre::{
    arena::Identifiable,
    lexis::{Length, Site, SiteRef, TokenCount, TokenCursor, TokenRef},
    syntax::{ErrorRef, NodeRef, NodeRule, SyntaxError, SyntaxSession},
};

use crate::{syntax::KotlinNode, tokens::KotlinToken};

pub(super) struct FlexibleSessionSyntax<'session, S> {
    session: &'session mut S,
    faking: bool,
    fake_index: TokenCount,
}

impl<'session, S> FlexibleSessionSyntax<'session, S> {
    pub(super) fn new(session: &'session mut S) -> Self {
        Self {
            session,
            faking: false,
            fake_index: 0,
        }
    }
}

impl<'session, S> Identifiable for FlexibleSessionSyntax<'session, S>
where
    for<'code> S: SyntaxSession<'code, Node = KotlinNode>,
{
    fn id(&self) -> lady_deirdre::arena::Id {
        self.session.id()
    }
}

impl<'session, S> TokenCursor<'session> for FlexibleSessionSyntax<'session, S>
where
    for<'code> S: SyntaxSession<'code, Node = KotlinNode>,
{
    type Token = KotlinToken;

    fn advance(&mut self) -> bool {
        let is_not_eoi = self.token(0) == KotlinToken::EOI;
        self.skip(1);
        is_not_eoi
    }

    fn skip(&mut self, distance: TokenCount) {
        if self.faking && self.token(distance) != KotlinToken::EOI {
            self.fake_index += distance;
        } else {
            self.session.skip(distance);
        }
    }

    fn token(&mut self, distance: TokenCount) -> Self::Token {
        if self.faking {
            self.session.token(self.fake_index + distance)
        } else {
            self.session.token(distance)
        }
    }

    fn site(&mut self, distance: TokenCount) -> Option<Site> {
        if self.faking {
            self.session.site(self.fake_index + distance)
        } else {
            self.session.site(distance)
        }
    }

    fn length(&mut self, distance: TokenCount) -> Option<Length> {
        if self.faking {
            self.session.length(self.fake_index + distance)
        } else {
            self.session.length(distance)
        }
    }

    fn string(&mut self, distance: TokenCount) -> Option<&'session str> {
        if self.faking {
            self.session.string(self.fake_index + distance)
        } else {
            self.session.string(distance)
        }
    }

    fn token_ref(&mut self, distance: TokenCount) -> TokenRef {
        if self.faking {
            self.session.token_ref(self.fake_index + distance)
        } else {
            self.session.token_ref(distance)
        }
    }

    fn site_ref(&mut self, distance: TokenCount) -> SiteRef {
        if self.faking {
            self.session.site_ref(self.fake_index + distance)
        } else {
            self.session.site_ref(distance)
        }
    }

    fn end_site_ref(&mut self) -> SiteRef {
        if self.faking {
            //TODO: is this fine?
            self.session.end_site_ref()
        } else {
            self.session.end_site_ref()
        }
    }
}

impl<'session, S> SyntaxSession<'session> for FlexibleSessionSyntax<'session, S>
where
    for<'code> S: SyntaxSession<'code, Node = KotlinNode>,
{
    type Node = KotlinNode;

    fn descend(&mut self, rule: NodeRule) -> NodeRef {
        self.session.descend(rule)
    }

    fn enter(&mut self, rule: NodeRule) -> NodeRef {
        self.session.enter(rule)
    }

    fn leave(&mut self, node: Self::Node) -> NodeRef {
        self.session.leave(node)
    }

    fn lift(&mut self, node_ref: &NodeRef) {
        self.session.lift(node_ref)
    }

    fn node_ref(&self) -> NodeRef {
        self.session.node_ref()
    }

    fn parent_ref(&self) -> NodeRef {
        self.session.parent_ref()
    }

    fn failure(&mut self, error: SyntaxError) -> ErrorRef {
        self.session.failure(error)
    }
}
