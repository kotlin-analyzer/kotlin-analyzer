use std::marker::PhantomData;

use lady_deirdre::{
    lexis::SiteRef,
    syntax::{Node, NodeRef, PolyRef, SyntaxSession},
};

use crate::{parser::CustomParser, syntax::KotlinNode, tokens::KotlinToken};

pub struct TypeParser<'session, 'code, S> {
    session: &'session mut S,
    step_start_ref: SiteRef,
    _code: PhantomData<&'code ()>,
}

impl<'session, 'code, S> TypeParser<'session, 'code, S>
where
    S: SyntaxSession<'code, Node = KotlinNode>,
{
    #[inline(always)]
    pub fn parse(session: &'session mut S) -> KotlinNode {
        let _node = session.node_ref();
        let _parent = session.parent_ref();
        let step_start_ref = session.site_ref(0);

        let mut _parser = Self {
            session,
            step_start_ref,
            _code: PhantomData,
        };

        todo!()
    }

    fn parse_type(&mut self) -> KotlinNode {
        use KotlinToken::*;
        self.skip_trivia();
        let type_modifier = match self.session.token(0) {
            AtNoWs | Suspend => self.session.descend(KotlinNode::TYPE_MODIFIER),
            _ => NodeRef::nil(),
        };
        let mut state = 1;

        loop {
            self.skip_trivia();
            match state {
                0 => {
                    match self.session.token(0) {
                        LParen => {
                            // main types: parenthesizedType | functionType | nullableType | definitelyNonNullableType
                            // parenthesizedUserType | parenthesizedType | functionTypeParameters
                            state += 1;
                        }
                        Identifier | Dynamic => {
                            // main types: typeReference
                            // Not: parenthesizedType
                            // Could be functionType | nullableType | definitelyNonNullableType
                            // Identifier
                            state += 2;
                        }
                        _ => {}
                    }
                }
                _ => {
                    break;
                }
            }
        }

        let node = self.session.node_ref();
        let parent = self.session.parent_ref();
        KotlinNode::Type {
            node,
            parent,
            modifier: type_modifier,
        }
    }

    fn parse_parameter_or_type(&mut self) -> NodeRef {
        use KotlinToken::*;

        match self.tokens_no_advance(true) {
            [(Identifier, _), (Colon, _)] => {
                // TODO: handle when identifier is a soft keyword
                self.session.descend(KotlinNode::PARAMETER);
            }
            [(Identifier, _), _] => {
                let node = self.session.descend(KotlinNode::TYPE_REFERENCE);
            }
            _ => {}
        }
        todo!()
    }
}

impl<'session, 'code, S> CustomParser<'code> for TypeParser<'session, 'code, S>
where
    S: SyntaxSession<'code, Node = KotlinNode>,
{
    fn session(&mut self) -> &mut impl SyntaxSession<'code, Node = KotlinNode> {
        self.session
    }

    fn step_start_ref(&mut self) -> &mut SiteRef {
        &mut self.step_start_ref
    }
}
