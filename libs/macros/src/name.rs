use inflector::Inflector;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Error, Ident, Result};
use tokens::Token;

use crate::parse::{BasicParseEntry, ParseEntry};

#[derive(Debug, Clone)]
pub enum SimpleName {
    /// The form of symbols like `==`, `+=`, etc.
    Token { token: Token, span: Span },
    /// The form of an identitifier like name, functionName
    Ident(Ident),
    /// Explicit token name like AT_NO_WS, EQUAL
    TokenIdent(Ident),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RetType {
    Single,
    Many,
}

#[derive(Debug, Clone)]
pub struct CompositeName {
    parent: Ident,
    position: usize,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum Name {
    Simple(SimpleName, RetType),
    Composite(CompositeName, RetType),
}

impl Name {
    fn simple(name: SimpleName) -> Self {
        Self::Simple(name, RetType::Single)
    }

    fn composite(name: CompositeName) -> Self {
        Self::Composite(name, RetType::Single)
    }
}

impl SimpleName {
    fn name(&self) -> String {
        match self {
            SimpleName::Token { token, .. } => format!("{token:?}"),
            SimpleName::Ident(id) | SimpleName::TokenIdent(id) => id.to_string(),
        }
    }

    fn span(&self) -> Span {
        match self {
            SimpleName::Token { span, .. } => span.clone(),
            SimpleName::Ident(id) | SimpleName::TokenIdent(id) => id.span(),
        }
    }

    fn type_name(&self) -> Ident {
        Ident::new(&self.name().to_pascal_case(), self.span())
    }

    fn cast_closure(&self) -> TokenStream {
        let variant = format_ident!("{}", self.type_name());
        quote!(#variant::cast)
    }

    fn method_name_inner(&self) -> String {
        match self {
            SimpleName::Token { .. } | SimpleName::TokenIdent(_) => {
                format!("get_{}", self.name().to_snake_case())
            }
            SimpleName::Ident(_) => {
                format!("{}", self.name().to_snake_case())
            }
        }
    }

    fn iter_fn(&self, ret_type: &RetType) -> Ident {
        match ret_type {
            RetType::Single => format_ident!("find_map"),
            RetType::Many => format_ident!("filter_map"),
        }
    }

    fn method_name(&self, ret_type: &RetType) -> Ident {
        let name = self.method_name_inner();
        match ret_type {
            RetType::Single => format_ident!("{}", name),
            RetType::Many => format_ident!("{}", name.to_plural()),
        }
    }

    fn return_type(&self, ret_type: &RetType) -> TokenStream {
        let type_name = format_ident!("{}", self.type_name());
        match ret_type {
            RetType::Single => quote!(Option<#type_name>),
            RetType::Many => quote!(impl Iterator<Item = #type_name> + '_),
        }
    }
}

impl CompositeName {
    pub fn new(parent: &Ident, position: usize, span: Span) -> Self {
        Self {
            parent: parent.clone(),
            position,
            span,
        }
    }
    pub fn to_simple(&self) -> SimpleName {
        let Self {
            parent,
            position,
            span,
        } = self;
        SimpleName::Ident(Ident::new(
            &format!("{}Segment{position}", parent.to_string()),
            span.clone(),
        ))
    }
}

impl Name {
    pub fn type_name(&self) -> Ident {
        match self {
            Self::Simple(simple, _) => simple.type_name(),
            Self::Composite(comp, _) => comp.to_simple().type_name(),
        }
    }

    pub fn cast_closure(&self) -> TokenStream {
        match self {
            Self::Simple(simple, _) => simple.cast_closure(),
            Self::Composite(..) => {
                let variant = format_ident!("{}", self.type_name());
                // the variant real type would be defined in the same module and not in ::syntax
                quote!(#variant::cast)
            }
        }
    }

    pub fn method_name(&self) -> Ident {
        match self {
            Self::Simple(simple, ret_type) => simple.method_name(ret_type),
            Self::Composite(comp, ret_type) => comp.to_simple().method_name(ret_type),
        }
    }

    pub fn iter_fn(&self) -> Ident {
        match self {
            Self::Simple(simple, ret_type) => simple.iter_fn(ret_type),
            Self::Composite(comp, ret_type) => comp.to_simple().iter_fn(ret_type),
        }
    }

    pub fn return_type(&self) -> TokenStream {
        match self {
            Self::Simple(simple, ret_type) => simple.return_type(ret_type),
            Self::Composite(comp, ret_type) => comp.to_simple().return_type(ret_type),
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub struct NameCtx<'a> {
    pub parent: &'a Ident,
    pub position: usize,
}

impl NameCtx<'_> {
    pub fn new<'a>(parent: &'a Ident, position: usize) -> NameCtx<'a> {
        NameCtx {
            parent: parent,
            position,
        }
    }
}

fn resolve_token(name: &str, span: Span) -> Result<Token> {
    tokens::resolve_token(name).ok_or_else(|| Error::new(span, "No matching Token variant"))
}

pub trait IntoName {
    fn into_name(&self, ctx: NameCtx) -> Result<Name>;
}

impl IntoName for BasicParseEntry {
    fn into_name(&self, ctx: NameCtx) -> Result<Name> {
        if let Some(name) = self.config().name.clone() {
            return Ok(Name::simple(SimpleName::Ident(name)));
        }

        match self {
            BasicParseEntry::CharLit(ch, ..) => resolve_token(&ch.value().to_string(), ch.span())
                .map(|token| {
                    Name::simple(SimpleName::Token {
                        token,
                        span: ch.span(),
                    })
                }),
            BasicParseEntry::StrLit(st, ..) => resolve_token(&st.value().to_string(), st.span())
                .map(|token| {
                    Name::simple(SimpleName::Token {
                        token,
                        span: st.span(),
                    })
                }),
            BasicParseEntry::Ident(id, ..) => {
                if id.to_string().starts_with(char::is_lowercase) {
                    Ok(Name::simple(SimpleName::Ident(id.clone())))
                } else {
                    Ok(Name::simple(SimpleName::TokenIdent(id.clone())))
                }
            }
            BasicParseEntry::Repeated { .. } => Ok(Name::Composite(
                CompositeName::new(&ctx.parent, ctx.position, self.span()),
                RetType::Many,
            )),
            BasicParseEntry::Optional { .. } | BasicParseEntry::Group { .. } => Ok(
                Name::composite(CompositeName::new(&ctx.parent, ctx.position, self.span())),
            ),
        }
    }
}

impl IntoName for ParseEntry {
    fn into_name(&self, ctx: NameCtx) -> Result<Name> {
        if let Some(name) = self.config().name.clone() {
            return Ok(Name::simple(SimpleName::Ident(name)));
        }
        match self {
            ParseEntry::Basic(basic_parse_entry) => basic_parse_entry.into_name(ctx),
            ParseEntry::Choice { .. } => {
                let name =
                    Name::composite(CompositeName::new(&ctx.parent, ctx.position, self.span()));
                Ok(name)
            }
        }
    }
}

impl BasicParseEntry {
    pub fn get_children_names(&self, parent: &Ident) -> Result<Vec<Name>> {
        match self {
            BasicParseEntry::CharLit(..)
            | BasicParseEntry::StrLit(..)
            | BasicParseEntry::Ident(..) => Ok(Vec::with_capacity(0)),
            BasicParseEntry::Optional { entries, .. }
            | BasicParseEntry::Repeated { entries, .. }
            | BasicParseEntry::Group { entries, .. } => entries
                .iter()
                .enumerate()
                .map(|(pos, s)| s.into_name(NameCtx::new(parent, pos)))
                .collect(),
        }
    }
}

impl ParseEntry {
    pub fn get_children_names(&self, parent: &Ident) -> Result<Vec<Name>> {
        match self {
            ParseEntry::Basic(basic_parse_entry) => basic_parse_entry.get_children_names(parent),
            ParseEntry::Choice { entries, .. } => entries
                .iter()
                .enumerate()
                .map(|(pos, s)| s.into_name(NameCtx::new(parent, pos)))
                .collect(),
        }
    }
}
