use inflector::Inflector;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{parse_quote, ExprPath, Ident, Result};
use tokens::Token;

use crate::parse::{BasicParseEntry, ParseEntry};

#[derive(Debug, Clone)]
enum CompositeKind {
    Segment,
    Variant,
}

impl CompositeKind {
    fn new(parent: &Name) -> Self {
        if parent.is_choice() {
            Self::Variant
        } else {
            Self::Segment
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompositeName<'a> {
    parent: &'a Name<'a>,
    position: usize,
    span: Span,
    kind: CompositeKind,
}

#[derive(Debug, Clone)]
pub enum NameForm<'a> {
    /// The form of symbols like `==`, `+=`, etc.
    Token { token: Token, span: Span },
    /// The form of an identitifier like name, functionName
    Ident(&'a Ident),
    /// Explicit token name like AT_NO_WS, EQUAL
    TokenIdent(&'a Ident),
    /// Explicitly named nodes with @ as in @Name
    FromConfig(&'a Ident),
    /// Unamed complex tokens, like {...} or [...] or (...)
    Composite(CompositeName<'a>),
}

#[derive(Debug, Clone)]
pub enum Name<'a> {
    Single(NameForm<'a>),
    Many(NameForm<'a>),
    Optional(NameForm<'a>),
    Choice(NameForm<'a>),
}

impl NameForm<'_> {
    fn name(&self) -> String {
        match self {
            NameForm::Token { token, .. } => format!("{token:?}"),
            NameForm::Ident(ident) | NameForm::FromConfig(ident) => ident.to_string(),
            NameForm::TokenIdent(ident) => {
                if ident.to_string().ends_with(char::is_lowercase) {
                    // ShebangLine
                    format!("{ident}Token")
                } else {
                    // NL | AT_NO_WS
                    ident.to_string()
                }
            }
            NameForm::Composite(CompositeName {
                parent,
                position,
                kind,
                ..
            }) => {
                let kind = match kind {
                    CompositeKind::Segment => "Segment",
                    CompositeKind::Variant => "Variant",
                };
                format!("{}{kind}{position}", parent.type_name())
            }
        }
    }

    fn span(&self) -> Span {
        match self {
            NameForm::Token { span, .. } => *span,
            NameForm::Ident(ident) | NameForm::TokenIdent(ident) | NameForm::FromConfig(ident) => {
                ident.span()
            }
            NameForm::Composite(CompositeName { span, .. }) => *span,
        }
    }

    fn is_unknown(&self) -> bool {
        matches!(self, Self::Composite(..) | Self::FromConfig(..))
    }
}

impl Name<'_> {
    fn form(&self) -> &NameForm {
        match self {
            Name::Single(form) | Name::Many(form) | Name::Optional(form) | Name::Choice(form) => {
                form
            }
        }
    }

    fn name(&self) -> String {
        self.form().name()
    }

    pub fn span(&self) -> Span {
        self.form().span()
    }

    pub fn is_unknown(&self) -> bool {
        self.form().is_unknown()
    }

    pub fn is_choice(&self) -> bool {
        matches!(self, Self::Choice(..))
    }

    pub fn type_name(&self) -> Ident {
        Ident::new(&self.name().to_pascal_case(), self.span())
    }

    pub fn cast_closure(&self) -> ExprPath {
        let type_name = self.type_name();
        parse_quote!(#type_name::cast)
    }

    fn method_name_inner(&self) -> String {
        match self.form() {
            NameForm::Token { .. } | NameForm::TokenIdent(..) => {
                if self.name().to_snake_case().ends_with("_token") {
                    self.name().to_snake_case()
                } else {
                    format!("{}_token", self.name().to_snake_case())
                }
            }
            _ => self.name().to_snake_case(),
        }
    }

    pub fn iter_fn(&self) -> Ident {
        match self {
            Self::Optional(..) | Self::Single(..) | Self::Choice(..) => format_ident!("find_map"),
            Self::Many(..) => format_ident!("filter_map"),
        }
    }

    pub fn method_name(&self) -> Ident {
        let name = self.method_name_inner();
        match self {
            Self::Optional(..) | Self::Single(..) | Self::Choice(..) => {
                Ident::new(&name, self.span())
            }
            Self::Many(..) => Ident::new(&name.to_plural(), self.span()),
        }
    }

    pub fn return_type(&self) -> TokenStream {
        let type_name = self.type_name();
        match self {
            Self::Optional(..) | Self::Single(..) | Self::Choice(..) => quote!(Option<#type_name>),
            Self::Many(..) => quote!(impl Iterator<Item = #type_name> + '_),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NameCtx<'a> {
    pub parent: &'a Name<'a>,
    pub position: usize,
}

impl NameCtx<'_> {
    pub fn new<'a>(parent: &'a Name<'a>, position: usize) -> NameCtx<'a> {
        NameCtx { parent, position }
    }
}

pub trait ToName {
    fn to_name<'a, 'b: 'a>(&'a self, ctx: NameCtx<'b>) -> Result<Name<'a>>;
}

impl ToName for BasicParseEntry {
    fn to_name<'a, 'b: 'a>(&'a self, ctx: NameCtx<'b>) -> Result<Name<'a>> {
        match self {
            BasicParseEntry::Token { token, span, .. } => Ok(Name::Single(NameForm::Token {
                token: *token,
                span: *span,
            })),
            BasicParseEntry::Ident(id, ..) => {
                if id.to_string().starts_with(char::is_lowercase) {
                    Ok(Name::Single(NameForm::Ident(id)))
                } else {
                    Ok(Name::Single(NameForm::TokenIdent(id)))
                }
            }
            // For {T} and [T], we unwrap them
            BasicParseEntry::Repeated { entries, .. } if entries.len() == 1 => {
                match entries.first().unwrap().to_name(ctx)? {
                    Name::Single(form) | Name::Choice(form) => Ok(Name::Many(form)),
                    Name::Optional(form) => Err(syn::Error::new(
                        form.span(),
                        "Unexpected optional nested inside a repeatition",
                    )),
                    Name::Many(form) => Err(syn::Error::new(
                        form.span(),
                        "Unexpected nested repeatition",
                    )),
                }
            }
            BasicParseEntry::Optional { entries, .. } if entries.len() == 1 => {
                match entries.first().unwrap().to_name(ctx)? {
                    Name::Single(form) | Name::Choice(form) => Ok(Name::Optional(form)),
                    Name::Optional(form) => {
                        Err(syn::Error::new(form.span(), "Unexpected nested optional"))
                    }
                    Name::Many(form) => Err(syn::Error::new(
                        form.span(),
                        "Unexpected repeatition inside an optional",
                    )),
                }
            }

            BasicParseEntry::Repeated { .. } => {
                if let Some(name) = self.config().name.as_ref() {
                    return Ok(Name::Many(NameForm::FromConfig(name)));
                }

                Ok(Name::Many(NameForm::Composite(CompositeName {
                    parent: ctx.parent,
                    position: ctx.position,
                    span: self.span(),
                    kind: CompositeKind::new(ctx.parent),
                })))
            }
            BasicParseEntry::Optional { .. } => {
                if let Some(name) = self.config().name.as_ref() {
                    return Ok(Name::Optional(NameForm::FromConfig(name)));
                }

                Ok(Name::Optional(NameForm::Composite(CompositeName {
                    parent: ctx.parent,
                    position: ctx.position,
                    span: self.span(),
                    kind: CompositeKind::new(ctx.parent),
                })))
            }
            BasicParseEntry::Group { .. } => {
                if let Some(name) = self.config().name.as_ref() {
                    return Ok(Name::Optional(NameForm::FromConfig(name)));
                }

                Ok(Name::Single(NameForm::Composite(CompositeName {
                    parent: ctx.parent,
                    position: ctx.position,
                    span: self.span(),
                    kind: CompositeKind::new(ctx.parent),
                })))
            }
        }
    }
}

impl ToName for ParseEntry {
    fn to_name<'a, 'b: 'a>(&'a self, ctx: NameCtx<'b>) -> Result<Name<'a>> {
        match self {
            ParseEntry::Basic(basic_parse_entry) => basic_parse_entry.to_name(ctx),
            ParseEntry::Choice { .. } => {
                if let Some(name) = self.config().name.as_ref() {
                    return Ok(Name::Choice(NameForm::FromConfig(name)));
                }
                Ok(Name::Choice(NameForm::Composite(CompositeName {
                    parent: ctx.parent,
                    position: ctx.position,
                    span: self.span(),
                    kind: CompositeKind::new(ctx.parent),
                })))
            }
        }
    }
}

impl BasicParseEntry {
    pub fn get_children_names<'a, 'b: 'a>(&'a self, parent: &'b Name) -> Result<Vec<Name<'a>>> {
        if !self.is_composite() {
            return Ok(Vec::with_capacity(0));
        }
        match self {
            BasicParseEntry::Optional { entries, .. }
            | BasicParseEntry::Repeated { entries, .. }
            | BasicParseEntry::Group { entries, .. } => entries
                .iter()
                .enumerate()
                .map(|(pos, s)| s.to_name(NameCtx::new(parent, pos)))
                .collect(),
            _ => unreachable!(),
        }
    }
}

impl ParseEntry {
    pub fn get_children_names<'a, 'b: 'a>(&'a self, parent: &'b Name) -> Result<Vec<Name<'a>>> {
        if !self.is_composite() {
            return Ok(Vec::with_capacity(0));
        }

        match self {
            ParseEntry::Basic(basic_parse_entry) => basic_parse_entry.get_children_names(parent),
            ParseEntry::Choice { entries, .. } => entries
                .iter()
                .enumerate()
                .map(|(pos, s)| s.to_name(NameCtx::new(parent, pos)))
                .collect(),
        }
    }
}
