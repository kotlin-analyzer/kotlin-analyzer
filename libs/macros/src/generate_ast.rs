#![allow(unused)]

use std::collections::{HashMap, HashSet};

use inflector::Inflector;
use itertools::Itertools;
use nonempty::NonEmpty;
use proc_macro2::{extra::DelimSpan, Span, TokenStream};
use quote::{format_ident, quote};
use syn::{spanned::Spanned, token::Paren, Error, Ident, ItemFn, LitStr, Result};
use tokens::Token;

use crate::parse::{
    BasicParseEntry, Config, Field, GenAst, ParseEntry, ParseEntryExt, TopLevelParseEntry,
};

macro_rules! err {
    ($span: expr, $message: literal) => {{
        let span: Span = $span.clone();
        let message: &'static str = $message;
        Err(Error::new(span, message))
    }};
    ($message: literal) => {{
        let message: &'static str = $message;
        Err(Error::new(Span::call_site(), message))
    }};
}

#[derive(Debug, Clone)]
enum SimpleName {
    /// The form of symbols like `==`, `+=`, etc.
    Token { token: Token, span: Span },
    /// The form of an identitifier like name, functionName
    Ident(Ident),
    /// Explicit token name like AT_NO_WS, EQUAL
    TokenIdent(Ident),
}

#[derive(Debug, Clone)]
enum CompositeName {
    Group(NonEmpty<NameWithField>),
    Choice(NonEmpty<NameWithField>),
    Optional(NonEmpty<NameWithField>),
}

#[derive(Debug, Clone)]
enum Name {
    Simple(SimpleName),
    Composite(CompositeName),
}

#[derive(Debug, Clone)]
struct NameWithField {
    name: Box<Name>,
    field: Ident,
    is_multi: bool,
    config_name: Option<Ident>,
}

impl ParseEntryExt {
    fn extract_name(&self, field: Ident, is_multi: bool) -> Result<NameWithField> {
        let config_name = self.config.name.clone();
        match &self.entry {
            ParseEntry::Basic(basic) => match basic {
                BasicParseEntry::CharLit(ch) => resolve_token(&ch.value().to_string(), ch.span())
                    .map(|token| NameWithField {
                        name: Box::new(Name::Simple(SimpleName::Token {
                            token,
                            span: ch.span(),
                        })),
                        field,
                        is_multi,
                        config_name,
                    }),
                BasicParseEntry::StrLit(st) => resolve_token(&st.value().to_string(), st.span())
                    .map(|token| NameWithField {
                        name: Box::new(Name::Simple(SimpleName::Token {
                            token,
                            span: st.span(),
                        })),
                        field,
                        is_multi,
                        config_name,
                    }),
                BasicParseEntry::Ident(id) => {
                    if id.to_string().starts_with(char::is_lowercase) {
                        Ok(NameWithField {
                            name: Box::new(Name::Simple(SimpleName::Ident(id.clone()))),
                            field,
                            is_multi,
                            config_name,
                        })
                    } else {
                        Ok(NameWithField {
                            name: Box::new(Name::Simple(SimpleName::TokenIdent(id.clone()))),
                            field,
                            is_multi,
                            config_name,
                        })
                    }
                }

                BasicParseEntry::Repeated { entries, .. } if entries.len() == 1 => {
                    entries[0].extract_name(field, true)
                }
                BasicParseEntry::Group { entries, .. }
                | BasicParseEntry::Optional { entries, .. }
                    if entries.len() == 1 =>
                {
                    entries[0].extract_name(field, is_multi)
                }

                BasicParseEntry::Group { entries, .. } => {
                    Ok(NameWithField {
                        name: Box::new(Name::Composite(CompositeName::Group(
                            NonEmpty::from_vec(
                                entries
                                    .iter()
                                    .map(|e| e.extract_name(field.clone(), is_multi))
                                    .collect::<Result<Vec<_>>>()?,
                            )
                            // safe to unwrap here
                            .unwrap(),
                        ))),
                        field,
                        is_multi,
                        config_name,
                    })
                }
                // Equivalent of Group where each entry is multi
                BasicParseEntry::Repeated { entries, .. } => {
                    Ok(NameWithField {
                        name: Box::new(Name::Composite(CompositeName::Group(
                            NonEmpty::from_vec(
                                entries
                                    .iter()
                                    .map(|e| e.extract_name(field.clone(), true))
                                    .collect::<Result<Vec<_>>>()?,
                            )
                            // safe to unwrap here
                            .unwrap(),
                        ))),
                        field,
                        is_multi,
                        config_name,
                    })
                }
                BasicParseEntry::Optional { entries, .. } => {
                    Ok(NameWithField {
                        name: Box::new(Name::Composite(CompositeName::Optional(
                            NonEmpty::from_vec(
                                entries
                                    .iter()
                                    .map(|e| e.extract_name(field.clone(), is_multi))
                                    .collect::<Result<Vec<_>>>()?,
                            )
                            // safe to unwrap here
                            .unwrap(),
                        ))),
                        field,
                        is_multi,
                        config_name,
                    })
                }
            },
            // This should not happen in practice cos we handled this already in the parsing logic
            ParseEntry::Choice { entries } if entries.len() == 1 => {
                entries[0].extract_name(field, is_multi)
            }
            ParseEntry::Choice { entries } => Ok(NameWithField {
                name: Box::new(Name::Composite(CompositeName::Choice(
                    NonEmpty::from_vec(
                        entries
                            .iter()
                            .map(|e| e.extract_name(field.clone(), is_multi))
                            .collect::<Result<Vec<_>>>()?,
                    )
                    // safe to unwrap here
                    .unwrap(),
                ))),
                field,
                is_multi,
                config_name,
            }),
        }
    }
}

impl NameWithField {
    fn name(&self) -> String {
        match &*self.name {
            Name::Simple(simple_name) => simple_name.name(),
            Name::Composite(composite_name) => match composite_name {
                // for ("=" "||") or ["=" "||"] we get equalsAndConjuction
                CompositeName::Group(non_empty) | CompositeName::Optional(non_empty) => non_empty
                    .iter()
                    .map(|n| n.name())
                    .collect::<Vec<_>>()
                    .join("And"),
                // for `"=" | "||"` we get equalsOrConjuction
                CompositeName::Choice(non_empty) => non_empty
                    .iter()
                    .map(|n| n.name())
                    .collect::<Vec<_>>()
                    .join("Or"),
                // this is same as group really
            },
        }
    }
    fn iter_fn(&self) -> Ident {
        if self.is_multi {
            format_ident!("filter_map")
        } else {
            format_ident!("filter_map")
        }
    }

    fn method_name(&self, entry: &ParseEntryExt) -> Ident {
        let name = match &*self.name {
            Name::Simple(simple_name) => simple_name.method_name(),
            Name::Composite(_) => self.name().to_snake_case(),
        };

        if self.is_multi {
            format_ident!("{}", name.to_plural())
        } else {
            format_ident!("{}", name)
        }
    }

    fn return_type(&self, entry: &ParseEntryExt) -> TokenStream {
        let type_name = format_ident!("{}", self.type_name());
        if !self.is_multi {
            quote!(Option<#type_name>)
        } else {
            quote!(impl Iterator<Item = #type_name> + '_)
        }
    }

    fn span(&self) -> Span {
        match &*self.name {
            Name::Simple(simple) => simple.span(),
            Name::Composite(box_comp) => match box_comp {
                CompositeName::Group(non_empty) => non_empty.first().name.span(),
                CompositeName::Choice(non_empty) => non_empty.first().name.span(),
                CompositeName::Optional(non_empty) => non_empty.first().name.span(),
            },
        }
    }

    fn type_name(&self) -> String {
        match &*self.name {
            Name::Simple(simple) => simple.type_name(),
            Name::Composite(_) => self.name().to_pascal_case(),
        }
    }

    fn variant_name(&self) -> String {
        match &*self.name {
            Name::Simple(simple) => simple.variant_name(),
            Name::Composite(_) => self.type_name(),
        }
    }

    fn cast_closure(&self) -> TokenStream {
        match &*self.name {
            Name::Simple(simple) => simple.cast_closure(),
            Name::Composite(ret_type_g) => todo!(),
        }
    }
}

trait Named {
    fn name(&self) -> String;
    fn span(&self) -> Span;
    fn type_name(&self) -> String;
    fn variant_name(&self) -> String;
    fn cast_closure(&self) -> TokenStream;
    fn method_name(&self) -> String;
}

impl Name {
    fn name(&self) -> String {
        match self {
            Name::Simple(simple) => simple.name(),
            Name::Composite(ret_type_g) => todo!(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Name::Simple(simple) => simple.span(),
            Name::Composite(box_comp) => match box_comp {
                CompositeName::Group(non_empty) => non_empty.first().name.span(),
                CompositeName::Choice(non_empty) => non_empty.first().name.span(),
                CompositeName::Optional(non_empty) => non_empty.first().name.span(),
            },
        }
    }

    fn type_name(&self) -> String {
        match self {
            Name::Simple(simple) => simple.type_name(),
            Name::Composite(ret_type_g) => todo!(),
        }
    }

    fn variant_name(&self) -> String {
        match self {
            Name::Simple(simple) => simple.variant_name(),
            Name::Composite(ret_type_g) => todo!(),
        }
    }

    fn cast_closure(&self) -> TokenStream {
        match self {
            Name::Simple(simple) => simple.cast_closure(),
            Name::Composite(ret_type_g) => todo!(),
        }
    }

    fn method_name(&self) -> String {
        match self {
            Name::Simple(simple) => simple.method_name(),
            Name::Composite(_) => {
                format!("{}", self.name().to_snake_case())
            }
        }
    }
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
            SimpleName::Ident(id) => self.type_name(),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RetTypeG<T> {
    Optional(T),
    Many(T),
}

// Get rid of this by making simple name represent this
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RetType {
    Optional,
    Many,
}

impl RetType {
    fn iter_fn(&self) -> Ident {
        match self {
            RetType::Optional => format_ident!("find_map"),
            RetType::Many => format_ident!("filter_map"),
        }
    }

    fn method_name(&self, entry: &ParseEntryExt) -> Result<Ident> {
        let name = entry.method_name()?;
        let ident = match self {
            RetType::Optional => format_ident!("{}", name),
            RetType::Many => format_ident!("{}", name.to_plural()),
        };
        Ok(ident)
    }

    fn return_type(&self, entry: &ParseEntryExt) -> Result<TokenStream> {
        let type_name = format_ident!("{}", entry.type_name()?);
        let ident = match self {
            RetType::Optional => quote!(Option<#type_name>),
            RetType::Many => quote!(impl Iterator<Item = #type_name> + '_),
        };
        Ok(ident)
    }
}

#[derive(Debug, Clone)]
enum GenInterface {
    None,
    Single {
        simple_name: SimpleName,
        method: Ident,
        iter_fn: Ident,
        return_type: TokenStream,
        cast: TokenStream,
    },
    NestedMany {
        entry: ParseEntryExt,
        ident: Ident,
        span: Span,
    },
    Many {
        entries: Vec<GenInterface>,
        simple_name: Option<SimpleName>,
        span: Span,
    },
    Enum {
        variants: Vec<GenInterface>,
        name: Option<Ident>,
        span: Span,
        is_nested: bool,
    },
}

struct Generated {
    impls: TokenStream,
    new_types: TokenStream,
}

impl Default for Generated {
    fn default() -> Self {
        Self {
            impls: quote!(),
            new_types: quote!(),
        }
    }
}

impl GenInterface {
    fn generate(self, field_ident: &Ident, is_nested: bool) -> Result<Generated> {
        match self {
            GenInterface::Single {
                method,
                iter_fn,
                return_type,
                cast,
                ..
            } => {
                let method_body = quote! {
                    fn #method(&self) -> #return_type {
                        self.0.children().#iter_fn(#cast)
                    }
                };
                Ok(Generated {
                    impls: method_body,
                    ..Default::default()
                })
            }
            // We make the following assumptions
            // That enums can not contain other enums
            // Note that if an enum is nested within another entry,
            // that if it doesn't provide a name,
            // the top level field would be used. This is not what we want
            // We enforce that logic here
            GenInterface::Enum {
                variants,
                name,
                span,
                is_nested,
            } => {
                if is_nested && name.is_none() {
                    return Err(Error::new(
                        span,
                        "Nested enums are expected to provide a name. Try adding @Name at the end",
                    ));
                }

                let casts = variants
                    .iter()
                    .flat_map(|v| v.simple_name())
                    .map(|v| v.cast_closure());

                let kind_name = if let Some(name) = &name {
                    format_ident!("{}Kind", name)
                } else {
                    format_ident!("{}Kind", field_ident)
                };

                let casts_for_kind = variants
                    .iter()
                    .map(|v| v.simple_name())
                    .collect::<Result<Vec<_>>>()?;

                let mut casts_for_kind = casts_for_kind.into_iter().map(|v| {
                    let variant_name = format_ident!("{}", v.name().to_pascal_case());
                    let cast = v.cast_closure();
                    quote!(#cast(self.0.clone()).map(#kind_name::#variant_name))
                });

                let first_cast = casts_for_kind.next().ok_or_else(|| {
                    Error::new(span, "choice variant expected to have at least one entry")
                })?;

                let mut enum_variants = variants.iter().flat_map(|v| v.simple_name()).map(|v| {
                    let ident = format_ident!("{}", v.name().to_pascal_case());
                    let ty = format_ident!("{}", v.type_name());
                    quote!(#ident(#ty))
                });

                let impls = quote! {
                    pub fn cast(node: SyntaxNode) -> Option<Self> {
                        if #(#casts(node.clone()).is_some())||* {
                            Some(Self(node))
                        } else {
                            None
                        }
                    }

                    pub fn kind(&self) -> #kind_name {
                            #first_cast
                            #(.or(#casts_for_kind))*
                            .unwrap()
                    }
                };

                let (nested_enum, impls) = if let Some(name) = name {
                    let method_name = format_ident!("{}", name.to_string().to_snake_case());

                    (
                        quote! {
                            #[derive(PartialEq, Eq, Hash, Clone)]
                            #[repr(transparent)]
                            pub struct #name(::syntax::SyntaxNode);

                            impl #name {
                                #impls
                            }

                        },
                        quote! {
                            fn #method_name(&self) -> Option<#name> {
                                self.0.children().find_map(#name::cast)
                            }
                        },
                    )
                } else {
                    (quote!(), impls)
                };

                // if it is nested
                // impls would got into the new type and would not be returned
                // for the top level field

                let new_types_from_variants = variants
                    .iter()
                    .filter(|v| matches!(v, GenInterface::NestedMany { .. }))
                    // Refactor: fields passed to generate here are not needed
                    .map(|v| v.clone().generate(field_ident, is_nested))
                    .collect::<Result<Vec<_>>>()?
                    .into_iter()
                    .map(|gen| gen.new_types);

                let new_types = quote! {
                    pub enum #kind_name {
                        #(#enum_variants),*
                    }
                    #nested_enum
                    #(#new_types_from_variants)*
                };

                Ok(Generated { impls, new_types })
            }
            GenInterface::Many { entries, .. } => {
                let collected = entries
                    .into_iter()
                    .flat_map(|e| e.generate(field_ident, true))
                    .fold(Generated::default(), |acc, next| {
                        let next_impls = next.impls;
                        let next_new_types = next.new_types;
                        let acc_impls = acc.impls;
                        let acc_new_types = acc.new_types;
                        Generated {
                            impls: quote! {
                                #acc_impls
                                #next_impls
                            },
                            new_types: quote! {
                                #acc_new_types

                                #next_new_types
                            },
                        }
                    });
                Ok(collected)
            }
            // Here we would create a new type for this
            GenInterface::NestedMany { entry, ident, .. } => Ok(Generated {
                new_types: gen_top_level(TopLevelParseEntry {
                    field: Field { name: ident },
                    ast: entry,
                })?,
                ..Default::default()
            }),
            GenInterface::None => Ok(Default::default()),
        }
    }

    fn simple_name(&self) -> Result<SimpleName> {
        match self {
            GenInterface::None => err!("Unexpected error. None variants does not have simple name"),
            GenInterface::Single { simple_name, .. } => Ok(simple_name.clone()),
            GenInterface::Many { simple_name, span, .. } => simple_name.clone().ok_or_else(|| Error::new(span.clone(), "Expected complex variant within a choice entry to have a name.\nTry adding @Name")),
            GenInterface::Enum { name, span, .. } => name.clone().map(SimpleName::Ident).ok_or_else(|| Error::new(span.clone(), "Unexpected nested choice entry")),
            GenInterface::NestedMany { ident, span, .. } => Ok(SimpleName::Ident(ident.clone())),
        }
    }
}

fn resolve_token(name: &str, span: Span) -> Result<Token> {
    tokens::resolve_token(name).ok_or_else(|| Error::new(span, "No matching Token variant"))
}

impl ParseEntryExt {
    fn simple_name(&self) -> Result<SimpleName> {
        let config_name = self
            .config
            .name
            .clone()
            .map(SimpleName::Ident)
            .ok_or_else(|| {
                Error::new(
                    self.entry.span(),
                    "Missing name for entries. Try adding @Name at the end",
                )
            });

        match &self.entry {
            ParseEntry::Basic(basic) => match basic {
                BasicParseEntry::CharLit(ch) => config_name.or_else(|_| {
                    resolve_token(&ch.value().to_string(), ch.span()).map(|token| {
                        SimpleName::Token {
                            token,
                            span: ch.span(),
                        }
                    })
                }),
                BasicParseEntry::StrLit(st) => config_name.or_else(|_| {
                    resolve_token(&st.value().to_string(), st.span()).map(|token| {
                        SimpleName::Token {
                            token,
                            span: st.span(),
                        }
                    })
                }),
                BasicParseEntry::Ident(id) => config_name.or_else(|_| {
                    if id.to_string().starts_with(char::is_lowercase) {
                        Ok(SimpleName::Ident(id.clone()))
                    } else {
                        Ok(SimpleName::TokenIdent(id.clone()))
                    }
                }),

                BasicParseEntry::Optional { .. }
                | BasicParseEntry::Repeated { .. }
                | BasicParseEntry::Group { .. } => config_name,
            },
            ParseEntry::Choice { .. } => config_name,
        }
    }

    fn method_name(&self) -> Result<String> {
        self.simple_name().map(|name| match name {
            SimpleName::Token { .. } | SimpleName::TokenIdent(_) => {
                format!("get_{}", name.name().to_snake_case())
            }
            SimpleName::Ident(_) => {
                format!("{}", name.name().to_snake_case())
            }
        })
    }

    fn type_name(&self) -> Result<String> {
        self.simple_name().map(|name| name.type_name())
    }

    // TODO: check if we really need context
    fn process(
        &self,
        ret_type: RetType,
        ctx: &mut Context,
        is_nested: bool,
    ) -> Result<GenInterface> {
        if self.config.ignore {
            return Ok(GenInterface::None);
        }
        match &self.entry {
            ParseEntry::Basic(basic) => match basic {
                BasicParseEntry::CharLit(..)
                | BasicParseEntry::StrLit(..)
                | BasicParseEntry::Ident(..) => {
                    let simple_name = self.simple_name()?;
                    if let Some(prev_ret) = ctx.env.get(&simple_name.name()) {
                        return Ok(GenInterface::None);
                    }
                    ctx.env.insert(simple_name.name());

                    Ok(GenInterface::Single {
                        method: ret_type.method_name(self)?,
                        iter_fn: ret_type.iter_fn(),
                        return_type: ret_type.return_type(self)?,
                        cast: simple_name.cast_closure(),
                        simple_name,
                    })
                }
                BasicParseEntry::Optional { entries, .. }
                | BasicParseEntry::Group { entries, .. }
                    if is_nested =>
                {
                    Ok(GenInterface::NestedMany {
                        entry: self.clone(),
                        ident: self.config.name.clone().ok_or_else(|| {
                            Error::new(
                                self.entry.span(),
                                "Expected nested complex variant be named.\nTry adding @Name",
                            )
                        })?,
                        span: self.entry.span(),
                    })
                }
                BasicParseEntry::Optional { entries, .. }
                | BasicParseEntry::Group { entries, .. } => Ok(GenInterface::Many {
                    entries: entries
                        .iter()
                        .map(|e| e.process(ret_type, ctx, true))
                        .collect::<Result<Vec<_>>>()?,
                    simple_name: self.simple_name().ok(),
                    span: self.entry.span(),
                }),
                BasicParseEntry::Repeated { entries, .. } if is_nested => {
                    Ok(GenInterface::NestedMany {
                        entry: self.clone(),
                        ident: self.config.name.clone().ok_or_else(|| {
                            Error::new(
                                self.entry.span(),
                                "Expected nested complex variant be named.\nTry adding @Name",
                            )
                        })?,
                        span: self.entry.span(),
                    })
                }
                BasicParseEntry::Repeated { entries, .. } => Ok(GenInterface::Many {
                    entries: entries
                        .iter()
                        .map(|e| e.process(RetType::Many, ctx, true))
                        .collect::<Result<Vec<_>>>()?,
                    simple_name: self.simple_name().ok(),
                    span: self.entry.span(),
                }),
            },
            ParseEntry::Choice { entries, .. } => {
                let variants = entries
                    .iter()
                    .map(|e| e.process(ret_type, ctx, true))
                    .collect::<Result<Vec<_>>>()?;

                Ok(GenInterface::Enum {
                    variants,
                    name: self.config.name.clone(),
                    span: self.entry.span(),
                    is_nested,
                })
            }
        }
    }
}

pub fn generate_ast(ast: GenAst) -> Result<TokenStream> {
    let entries = ast
        .entries
        .into_iter()
        .map(gen_top_level)
        .collect::<Result<Vec<_>>>()?;
    Ok(quote! {
        #(#entries)*
    })
}

pub fn gen_top_level(top: TopLevelParseEntry) -> Result<TokenStream> {
    let typename = format_ident!("{}", top.field.name.to_string().to_pascal_case());
    let mut context = Context::default();

    let Generated { impls, new_types } = top
        .ast
        .process(RetType::Optional, &mut context, false)?
        .generate(&typename, false)?;

    let syntax_name = format_ident!("{}", top.field.name.to_string().to_screaming_snake_case());
    let field_cast = if new_types.is_empty() {
        quote! {
            pub fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == ::syntax::SyntaxKind::#syntax_name {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }
    } else {
        quote!()
    };

    Ok(quote! {
        #[derive(PartialEq, Eq, Hash, Clone)]
        #[repr(transparent)]
        // todo add doc
        pub struct #typename(::syntax::SyntaxNode);

        #new_types

        impl #typename {
            #field_cast

            #impls
        }

    })
}
#[derive(Debug, Default)]
struct Context {
    env: HashSet<String>,
}

trait ToTokenStream {
    fn to_stream(&self) -> Result<TokenStream>;
}

impl ToTokenStream for TopLevelParseEntry {
    fn to_stream(&self) -> Result<TokenStream> {
        todo!()
    }
}
