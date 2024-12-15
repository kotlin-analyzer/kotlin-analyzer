use inflector::Inflector;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Error, Result};
use tokens::Token;

use crate::parse::{BasicParseEntry, ParseEntry, ParseEntryExt};

#[derive(Debug, Clone)]
pub enum SimpleName {
    /// The form of symbols like `==`, `+=`, etc.
    Token { token: Token, span: Span },
    /// The form of an identitifier like name, functionName
    Ident(Ident),
    /// Explicit token name like AT_NO_WS, EQUAL
    TokenIdent(Ident),
}

#[derive(Debug, Clone)]
pub struct CompositeName {
    parent: Ident,
    position: usize,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum Name {
    Simple(SimpleName),
    Composite(CompositeName),
}

pub trait Named {
    fn name(&self) -> String;
    fn span(&self) -> Span;
    fn type_name(&self) -> String;
    fn variant_name(&self) -> String;
    fn cast_closure(&self) -> TokenStream;
    fn method_name(&self) -> String;
}

impl Named for SimpleName {
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

    fn type_name(&self) -> String {
        match self {
            SimpleName::Token { .. } | SimpleName::TokenIdent(_) => "SyntaxNode".into(),
            SimpleName::Ident(id) => id.to_string().to_pascal_case(),
        }
    }

    fn variant_name(&self) -> String {
        match self {
            SimpleName::Token { .. } | SimpleName::TokenIdent(_) => {
                self.name().to_screaming_snake_case()
            }
            SimpleName::Ident(..) => self.type_name(),
        }
    }

    fn cast_closure(&self) -> TokenStream {
        let variant = format_ident!("{}", self.variant_name());
        match self {
            SimpleName::Token { .. } | SimpleName::TokenIdent(_) => {
                quote!(::syntax::cast_syntax_kind(::syntax::SyntaxKind::#variant))
            }
            SimpleName::Ident(_) => quote!(#variant::cast),
        }
    }

    fn method_name(&self) -> String {
        match self {
            SimpleName::Token { .. } | SimpleName::TokenIdent(_) => {
                format!("get_{}", self.name().to_snake_case())
            }
            SimpleName::Ident(_) => {
                format!("{}", self.name().to_snake_case())
            }
        }
    }
}

impl CompositeName {
    pub fn new(parent: Ident, position: usize, span: Span) -> Self {
        Self {
            parent,
            position,
            span,
        }
    }
    pub fn to_simple(&self) -> SimpleName {
        let Self {
            parent, position, ..
        } = self;
        SimpleName::Ident(format_ident!("{parent}Segment{position}"))
    }
}

impl Named for Name {
    fn name(&self) -> String {
        match self {
            Self::Simple(simple) => simple.name(),
            Self::Composite(comp) => comp.to_simple().name(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Self::Simple(simple) => simple.span(),
            Self::Composite(comp) => comp.to_simple().span(),
        }
    }

    fn type_name(&self) -> String {
        match self {
            Self::Simple(simple) => simple.type_name(),
            Self::Composite(comp) => comp.to_simple().type_name(),
        }
    }

    fn variant_name(&self) -> String {
        match self {
            Self::Simple(simple) => simple.variant_name(),
            Self::Composite(comp) => comp.to_simple().variant_name(),
        }
    }

    fn cast_closure(&self) -> TokenStream {
        match self {
            Self::Simple(simple) => simple.cast_closure(),
            Self::Composite(comp) => comp.to_simple().cast_closure(),
        }
    }

    fn method_name(&self) -> String {
        match self {
            Self::Simple(simple) => simple.method_name(),
            Self::Composite(comp) => comp.to_simple().method_name(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct NameCtx {
    parent: Ident,
    position: usize,
    config_name: Result<Name>,
}

impl NameCtx {
    fn new(parent: Ident, position: usize, config_name: Result<Name>) -> Self {
        Self {
            parent,
            position,
            config_name,
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
        ctx.config_name.or_else(|_| match self {
            BasicParseEntry::CharLit(ch) => {
                resolve_token(&ch.value().to_string(), ch.span()).map(|token| {
                    Name::Simple(SimpleName::Token {
                        token,
                        span: ch.span(),
                    })
                })
            }
            BasicParseEntry::StrLit(st) => {
                resolve_token(&st.value().to_string(), st.span()).map(|token| {
                    Name::Simple(SimpleName::Token {
                        token,
                        span: st.span(),
                    })
                })
            }
            BasicParseEntry::Ident(id) => {
                if id.to_string().starts_with(char::is_lowercase) {
                    Ok(Name::Simple(SimpleName::Ident(id.clone())))
                } else {
                    Ok(Name::Simple(SimpleName::TokenIdent(id.clone())))
                }
            }
            BasicParseEntry::Optional { .. }
            | BasicParseEntry::Repeated { .. }
            | BasicParseEntry::Group { .. } => Ok(Name::Composite(CompositeName::new(
                ctx.parent,
                ctx.position,
                self.span(),
            ))),
        })
    }
}

impl IntoName for ParseEntry {
    fn into_name(&self, ctx: NameCtx) -> Result<Name> {
        ctx.config_name.clone().or_else(|_| match self {
            ParseEntry::Basic(basic_parse_entry) => basic_parse_entry.into_name(ctx),
            ParseEntry::Choice { .. } => {
                let name =
                    Name::Composite(CompositeName::new(ctx.parent, ctx.position, self.span()));
                Ok(name)
            }
        })
    }
}

impl IntoName for ParseEntryExt {
    fn into_name(
        &self,
        NameCtx {
            parent, position, ..
        }: NameCtx,
    ) -> Result<Name> {
        let config_name = self.get_config_name();

        self.entry.into_name(NameCtx {
            parent,
            position,
            config_name,
        })
    }
}

impl BasicParseEntry {
    fn get_children_names(&self, parent: Ident, config_name: Result<Name>) -> Result<Vec<Name>> {
        match self {
            BasicParseEntry::CharLit(..)
            | BasicParseEntry::StrLit(..)
            | BasicParseEntry::Ident(..) => Ok(Vec::with_capacity(0)),
            BasicParseEntry::Optional { entries, .. }
            | BasicParseEntry::Repeated { entries, .. }
            | BasicParseEntry::Group { entries, .. } => entries
                .iter()
                .enumerate()
                .map(|(pos, s)| s.into_name(NameCtx::new(parent.clone(), pos, config_name.clone())))
                .collect(),
        }
    }
}

impl ParseEntry {
    fn get_children_names(&self, parent: Ident, config_name: Result<Name>) -> Result<Vec<Name>> {
        match self {
            ParseEntry::Basic(basic_parse_entry) => {
                basic_parse_entry.get_children_names(parent.clone(), config_name.clone())
            }
            ParseEntry::Choice { entries } => entries
                .iter()
                .enumerate()
                .map(|(pos, s)| s.into_name(NameCtx::new(parent.clone(), pos, config_name.clone())))
                .collect(),
        }
    }
}

impl ParseEntryExt {
    fn get_config_name(&self) -> Result<Name> {
        self.config
            .name
            .clone()
            .map(|name| Name::Simple(SimpleName::Ident(name)))
            .ok_or_else(|| {
                Error::new(
                    self.entry.span(),
                    "Missing name for entries. Try adding @Name at the end",
                )
            })
    }

    pub fn get_children_names(&self, parent: Ident) -> Result<Vec<Name>> {
        let config_name = self.get_config_name();

        self.entry.get_children_names(parent, config_name)
    }
}
