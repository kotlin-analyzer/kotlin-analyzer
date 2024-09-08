#![allow(unused)]

use std::collections::HashMap;

use inflector::Inflector;
use itertools::Itertools;
use proc_macro2::{extra::DelimSpan, Span, TokenStream};
use quote::{format_ident, quote};
use syn::{spanned::Spanned, token::Paren, Error, Ident, LitStr, Result};
use tokens::Token;

use crate::parse::{Config, Field, GenAst, ParseEntry, ParseEntryExt, TopLevelParseEntry};

#[derive(Debug, Clone)]
enum SimpleName {
    /// The form of symbols like `==`, `+=`, etc.
    Token { token: Token, span: Span },
    /// The form of an identitifier like name, functionName
    Ident(Ident),
    /// Explicit token name like AT_NO_WS, EQUAL
    TokenIdent(Ident),
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
}

enum GenInterface {
    None,
    Single {
        simple_name: SimpleName,
        method: Ident,
        iter_fn: Ident,
        return_type: TokenStream,
        cast: TokenStream,
    },
    Many {
        entries: Vec<GenInterface>,
        simple_name: Option<SimpleName>,
    },
    Enum {
        variants: Vec<GenInterface>,
        name: Option<Ident>,
        span: Span,
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

                let mut casts_for_kind = variants.iter().flat_map(|v| v.simple_name()).map(|v| {
                    let variant_name = format_ident!("{}", v.name().to_pascal_case());
                    let cast = v.cast_closure();
                    quote!(#cast(self.0.clone()).map(#kind_name::#variant_name))
                });

                let first_cast = casts_for_kind
                    .next()
                    .expect("choice variant expected to have at least one entry");

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

                let new_types = quote! {
                    pub enum #kind_name {
                        #(#enum_variants),*
                    }
                    #nested_enum
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
            GenInterface::None => Ok(Default::default()),
        }
    }

    fn simple_name(&self) -> Option<SimpleName> {
        match self {
            GenInterface::None => None,
            GenInterface::Single { simple_name, .. } => Some(simple_name.clone()),
            GenInterface::Many { simple_name, .. } => simple_name.clone(),
            GenInterface::Enum { name, .. } => name.clone().map(SimpleName::Ident),
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
                    self.span(),
                    "Missing name for entries. Try adding @Name at the end",
                )
            });

        match &self.entry {
            ParseEntry::CharLit(ch) => config_name.or_else(|_| {
                resolve_token(&ch.value().to_string(), ch.span()).map(|token| SimpleName::Token {
                    token,
                    span: ch.span(),
                })
            }),
            ParseEntry::StrLit(st) => config_name.or_else(|_| {
                resolve_token(&st.value().to_string(), st.span()).map(|token| SimpleName::Token {
                    token,
                    span: st.span(),
                })
            }),
            ParseEntry::Ident(id) => config_name.or_else(|_| {
                if id.to_string().starts_with(char::is_lowercase) {
                    Ok(SimpleName::Ident(id.clone()))
                } else {
                    Ok(SimpleName::TokenIdent(id.clone()))
                }
            }),

            ParseEntry::Optional { .. }
            | ParseEntry::Repeated { .. }
            | ParseEntry::Choice { .. }
            | ParseEntry::Group { .. } => config_name,
        }
    }

    fn span(&self) -> Span {
        match &self.entry {
            ParseEntry::CharLit(ch) => ch.span(),
            ParseEntry::StrLit(st) => st.span(),
            ParseEntry::Ident(id) => id.span(),
            ParseEntry::Optional {
                bracket_token,
                entries,
            } => bracket_token.span.span(),
            ParseEntry::Repeated {
                brace_token,
                entries,
            } => brace_token.span.span(),
            ParseEntry::Choice { entries } => {
                let first = &entries[0];
                // TODO: uncomment when Span::join becomes stable
                // let last = entries[entries.len() - 1];
                // first.span().join(last.span())
                first.span()
            }
            ParseEntry::Group {
                paren_token,
                entries,
            } => paren_token.span.span(),
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
    fn process(&self, ret_type: RetType, ctx: &mut Context) -> Result<GenInterface> {
        if self.config.ignore {
            return Ok(GenInterface::None);
        }
        match &self.entry {
            ParseEntry::CharLit(_, ..) | ParseEntry::StrLit(_, ..) | ParseEntry::Ident(_, ..) => {
                // let key = self.simple_name()?;
                // let ctx_value = ctx.env.get(&key);
                // if let Some(prev_ret) = ctx_value {
                //     if *prev_ret == ret_type || ret_type == RetType::Optional {
                //         return Ok(GenInterface::None);
                //     }
                // }
                Ok(GenInterface::Single {
                    method: ret_type.method_name(self)?,
                    iter_fn: ret_type.iter_fn(),
                    return_type: ret_type.return_type(self)?,
                    cast: self.simple_name().map(|s| s.cast_closure())?,
                    simple_name: self.simple_name()?,
                })
            }
            ParseEntry::Repeated { entries, .. } => Ok(GenInterface::Many {
                entries: entries
                    .iter()
                    .map(|e| e.process(RetType::Many, ctx))
                    .collect::<Result<Vec<_>>>()?,
                simple_name: self.simple_name().ok(),
            }),
            ParseEntry::Choice { entries, .. } => {
                let variants = entries
                    .iter()
                    .map(|e| e.process(ret_type, ctx))
                    .collect::<Result<Vec<_>>>()?;

                Ok(GenInterface::Enum {
                    variants,
                    name: self.config.name.clone(),
                    span: self.span(),
                })
            }
            ParseEntry::Group { entries, .. } | ParseEntry::Optional { entries, .. } => {
                Ok(GenInterface::Many {
                    entries: entries
                        .iter()
                        .map(|e| e.process(ret_type, ctx))
                        .collect::<Result<Vec<_>>>()?,
                    simple_name: self.simple_name().ok(),
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
    let Some(entry) = top.asts.get(0) else {
        return Err(Error::new(
            top.field.name.span(),
            "top level field should have only one entry",
        ));
    };

    let Generated { impls, new_types } = entry
        .process(RetType::Optional, &mut Context::default())?
        .generate(&typename, false)
        .unwrap_or_default();

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
#[derive(Debug, Default)]
struct Context {
    env: HashMap<SimpleName, RetType>,
    config: Config,
}

impl Context {
    fn set_config(&mut self, config: Config) -> &mut Self {
        self.config.ignore = config.ignore;
        if let Some(_) = config.name {
            self.config.name = config.name;
        }
        self
    }
}
