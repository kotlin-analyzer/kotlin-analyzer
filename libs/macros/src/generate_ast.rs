#![allow(unused)]

use std::collections::{HashMap, HashSet};

use inflector::Inflector;
use itertools::Itertools;
use nonempty::NonEmpty;
use proc_macro2::{extra::DelimSpan, Span, TokenStream};
use quote::{format_ident, quote};
use syn::{spanned::Spanned, token::Paren, Error, Ident, ItemFn, LitStr, Result};
use tokens::Token;

use crate::{
    name::{CompositeName, Name, Named, SimpleName},
    parse::{
        BasicParseEntry, Config, Field, GenAst, ParseEntry, ParseEntryExt, TopLevelParseEntry,
    },
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
        let name = entry.simple_name()?.method_name();
        let ident = match self {
            RetType::Optional => format_ident!("{}", name),
            RetType::Many => format_ident!("{}", name.to_plural()),
        };
        Ok(ident)
    }

    fn return_type(&self, entry: &ParseEntryExt) -> Result<TokenStream> {
        let simple_name = entry.simple_name()?;
        let type_name = format_ident!("{}", simple_name.type_name());
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

struct Named1<'a, 'b> {
    parent: Option<&'b Ident>,
    position: usize,
    entry: &'a ParseEntryExt,
}

struct MethodName<'a, 'b>(Named1<'a, 'b>);
struct VariantName<'a, 'b>(Named1<'a, 'b>);
struct TypeName<'a, 'b>(Named1<'a, 'b>);

struct GenContext {
    parent: Ident,
    position: usize,
}

trait Generate {
    fn generate(&self, ctx: Option<GenContext>) -> Result<TokenStream>;
}

impl Generate for TopLevelParseEntry {
    fn generate(&self, ctx: Option<GenContext>) -> Result<TokenStream> {
        todo!()
    }
}

impl Generate for ParseEntryExt {
    fn generate(&self, ctx: Option<GenContext>) -> Result<TokenStream> {
        todo!()
    }
}

impl Generate for ParseEntry {
    fn generate(&self, ctx: Option<GenContext>) -> Result<TokenStream> {
        todo!()
    }
}

impl Generate for BasicParseEntry {
    fn generate(&self, ctx: Option<GenContext>) -> Result<TokenStream> {
        match self {
            BasicParseEntry::CharLit(..)
            | BasicParseEntry::StrLit(..)
            | BasicParseEntry::Ident(..) => {
                // let name = self.get
                todo!()
            }
            BasicParseEntry::Optional {
                bracket_token,
                entries,
            } => todo!(),
            BasicParseEntry::Repeated {
                brace_token,
                entries,
            } => todo!(),
            BasicParseEntry::Group {
                paren_token,
                entries,
            } => todo!(),
        }
    }
}
