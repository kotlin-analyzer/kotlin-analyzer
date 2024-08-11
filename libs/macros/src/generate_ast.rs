#![allow(unused)]

use std::collections::HashMap;

use inflector::Inflector;
use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Ident, LitStr};
use tokens::resolve_token;

use crate::parse::{Config, Field, GenAst, ParseEntry, TopLevelParseEntry};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SimpleName {
    Token(String),
    Ident(String),
}

impl SimpleName {
    fn name(&self) -> &str {
        match self {
            SimpleName::Token(name) => name,
            SimpleName::Ident(name) => name,
        }
    }

    fn type_name(&self) -> String {
        match self {
            SimpleName::Token(_) => "SyntaxNode".into(),
            SimpleName::Ident(id) => id.to_string().to_pascal_case(),
        }
    }

    fn variant_name(&self) -> String {
        match self {
            SimpleName::Token(tok) => tok.to_screaming_snake_case(),
            SimpleName::Ident(id) => self.type_name(),
        }
    }

    fn cast_closure(&self) -> TokenStream {
        let variant = format_ident!("{}", self.variant_name());
        match self {
            SimpleName::Token(_) => {
                quote!(::syntax::cast_syntax_kind(::syntax::SyntaxKind::#variant))
            }
            SimpleName::Ident(_) => quote!(#variant::cast),
        }
    }
}

enum GenInterface {
    None,
    Single {
        simple_name: Option<SimpleName>,
        method: Option<Ident>,
        iter_fn: Ident,
        return_type: Option<TokenStream>,
        cast: Option<TokenStream>,
    },
    Many(Vec<GenInterface>),
    Enum {
        variants: Vec<GenInterface>,
        simple_name: Option<SimpleName>,
    },
}
#[derive(Default)]
struct Generated {
    impls: TokenStream,
    new_types: Option<TokenStream>,
    new_type_kinds: Vec<Ident>,
}

impl GenInterface {
    fn generate(self, field_ident: &Ident) -> Option<Generated> {
        match self {
            GenInterface::Single {
                method,
                iter_fn,
                return_type,
                cast,
                ..
            } => {
                let method = method?;
                let return_type = return_type?;
                let cast = cast?;
                let method_body = quote! {
                    fn #method(&self) -> #return_type {
                        self.0.children().#iter_fn(#cast)
                    }
                };
                Some(Generated {
                    impls: method_body,
                    new_types: None,
                    new_type_kinds: Vec::new(),
                })
            }
            GenInterface::Enum {
                variants,
                simple_name,
            } => {
                let casts = variants
                    .iter()
                    .flat_map(|v| v.simple_name())
                    .map(|v| v.cast_closure());

                let name = if let Some(name) = simple_name.as_ref() {
                    format_ident!("{}", name.name())
                } else {
                    format_ident!("{field_ident}")
                };

                let kind_name = format_ident!("{}Kind", name);

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
                            Some(#name(node))
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

                let nested_enum_name = simple_name
                    .as_ref()
                    .map(|name| format_ident!("{}", name.name()));

                let (nested_enum, impls) = if let Some(name) = simple_name.as_ref() {
                    let ident = format_ident!("{}", name.name());
                    let method_name = format_ident!("{}", name.name().to_snake_case());

                    (
                        quote! {
                            #[derive(PartialEq, Eq, Hash, Clone)]
                            #[repr(transparent)]
                            pub struct #ident(::syntax::SyntaxNode);

                            impl #ident {
                                #impls
                            }

                        },
                        quote! {
                            fn #method_name(&self) -> Option<#ident> {
                                self.0.children().find_map(#ident::cast)
                            }
                        },
                    )
                } else {
                    (quote!(), impls)
                };

                // TODO: a lot depends on simple_name
                // if it exists
                // impls would got into the new type and would not be returned
                // but the new type's name would be sent back so the parent type can use it

                let new_types = quote! {
                    pub enum #kind_name {
                        #(#enum_variants),*
                    }
                    #nested_enum
                };

                Some(Generated {
                    impls,
                    new_types: Some(new_types),
                    // TODO
                    new_type_kinds: nested_enum_name.into_iter().collect(),
                })
            }
            GenInterface::Many(entries) => {
                let collected = entries
                    .into_iter()
                    .flat_map(|e| e.generate(field_ident))
                    .fold(Generated::default(), |acc, next| {
                        let next_impls = next.impls;
                        let next_new_types = next.new_types;
                        let acc_impls = acc.impls;
                        let acc_new_types = acc.new_types;
                        let new_type_kinds = acc
                            .new_type_kinds
                            .into_iter()
                            .chain(next.new_type_kinds.into_iter())
                            .collect();
                        Generated {
                            impls: quote! {
                                #acc_impls
                                #next_impls
                            },
                            new_types: next_new_types
                                .map(|next| {
                                    quote! {
                                        #acc_new_types

                                        #next
                                    }
                                })
                                .or(acc_new_types),
                            new_type_kinds,
                        }
                    });
                Some(collected)
            }
            GenInterface::None => None,
        }
    }

    fn simple_name(&self) -> Option<&SimpleName> {
        match self {
            GenInterface::None => None,
            GenInterface::Single { simple_name, .. } => simple_name.as_ref(),
            GenInterface::Many(_) => None,
            GenInterface::Enum { simple_name, .. } => simple_name.as_ref(),
        }
    }
}

impl ParseEntry {
    /// gets the best name of the entry. Usually the name of token or identifier in simple cases
    /// but for multiple entries will try to find an identifier or default to the first token name found
    fn simple_name(&self) -> Option<SimpleName> {
        let mut tmp = self.identifiers().into_iter();
        match tmp.next() {
            a @ Some(SimpleName::Token(_)) => tmp.find(|s| matches!(s, SimpleName::Ident(_))).or(a),
            rest => rest,
        }
    }

    fn identifiers(&self) -> Vec<SimpleName> {
        if self.config().ignore {
            return vec![];
        }
        match self {
            ParseEntry::CharLit(ch, ..) => resolve_token(&ch.value().to_string())
                .map(|t| vec![SimpleName::Token(format!("{:?}", t))])
                .unwrap_or_default(),
            ParseEntry::StrLit(st, ..) => resolve_token(&st.value().to_string())
                .map(|t| vec![SimpleName::Token(format!("{:?}", t))])
                .unwrap_or_default(),
            ParseEntry::Ident(id, ..) => {
                if id.to_string().starts_with(char::is_lowercase) {
                    vec![SimpleName::Ident(id.to_string())]
                } else {
                    vec![SimpleName::Token(id.to_string())]
                }
            }
            ParseEntry::Repeated(entries, ..)
            | ParseEntry::Choice(entries, ..)
            | ParseEntry::Group(entries, ..)
            | ParseEntry::Optional(entries, ..) => entries
                .iter()
                .flat_map(|e| e.identifiers())
                .dedup()
                .collect(),
        }
    }

    fn method_name(&self) -> Option<String> {
        self.simple_name().map(|name| match name {
            SimpleName::Token(n) => format!("get_{}", n.to_snake_case()),
            SimpleName::Ident(n) => {
                format!("{}", n.to_snake_case())
            }
        })
    }

    fn type_name(&self) -> Option<String> {
        self.simple_name().map(|name| name.type_name())
    }

    fn process(&self, ret_type: RetType, ctx: &mut Context) -> GenInterface {
        if self.config().ignore {
            return GenInterface::None;
        }
        match self {
            ParseEntry::CharLit(_, ..) | ParseEntry::StrLit(_, ..) | ParseEntry::Ident(_, ..) => {
                let key = self.simple_name();
                let ctx_value = key.and_then(|k| ctx.env.get(&k));
                if let Some(prev_ret) = ctx_value {
                    if *prev_ret == ret_type || ret_type == RetType::Optional {
                        return GenInterface::None;
                    }
                }
                GenInterface::Single {
                    method: ret_type.method_name(self),
                    iter_fn: ret_type.iter_fn(),
                    return_type: ret_type.return_type(self),
                    cast: self.simple_name().map(|s| s.cast_closure()),
                    simple_name: self.simple_name(), // we can use the name from config here
                }
            }
            ParseEntry::Optional(entries, ..) => {
                GenInterface::Many(entries.iter().map(|e| e.process(ret_type, ctx)).collect())
            }
            ParseEntry::Repeated(entries, ..) => GenInterface::Many(
                entries
                    .iter()
                    .map(|e| e.process(RetType::Many, ctx))
                    .collect(),
            ),
            ParseEntry::Choice(entries, config) => {
                ctx.set_config(config.clone());
                let variants = entries.iter().map(|e| e.process(ret_type, ctx)).collect();

                let simple_name = ctx.config.name.clone().map(SimpleName::Ident);

                GenInterface::Enum {
                    variants,
                    simple_name,
                }
            }
            ParseEntry::Group(entries, config) => {
                ctx.set_config(config.clone());

                GenInterface::Many(entries.iter().map(|e| e.process(ret_type, ctx)).collect())
            }
        }
    }
}

pub fn generate_ast(ast: GenAst) -> TokenStream {
    let entries = ast.entries.into_iter().map(gen_top_level);
    quote! {
        #(#entries)*
    }
}

pub fn gen_top_level(top: TopLevelParseEntry) -> TokenStream {
    let typename = format_ident!("{}", top.field.name.to_string().to_pascal_case());
    let len = top.asts.len();
    let mut entries = top.asts.into_iter();
    let entry = match len {
        1 => entries.next().unwrap(),
        _ => ParseEntry::Group(entries.collect(), Config::default()),
    };

    let Generated {
        impls, new_types, ..
    } = entry
        .process(RetType::Optional, &mut Context::default())
        .generate(&typename)
        .unwrap_or_default();

    let syntax_name = format_ident!("{}", top.field.name.to_string().to_screaming_snake_case());
    let field_cast = if new_types.is_none() {
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

    quote! {
        #[derive(PartialEq, Eq, Hash, Clone)]
        #[repr(transparent)]
        // todo add doc
        pub struct #typename(::syntax::SyntaxNode);

        #new_types

        impl #typename {
            #field_cast

            #impls
        }

    }
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

    fn method_name(&self, entry: &ParseEntry) -> Option<Ident> {
        let name = entry.method_name()?;
        let ident = match self {
            RetType::Optional => format_ident!("{}", name),
            RetType::Many => format_ident!("{}", name.to_plural()),
        };
        Some(ident)
    }

    fn return_type(&self, entry: &ParseEntry) -> Option<TokenStream> {
        let type_name = format_ident!("{}", entry.type_name()?);
        let ident = match self {
            RetType::Optional => quote!(Option<#type_name>),
            RetType::Many => quote!(impl Iterator<Item = #type_name> + '_),
        };
        Some(ident)
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

// entry -> process -> generate
// Entry -> GenInterface -> Generated

#[cfg(test)]
mod test {
    use proc_macro2::Span;
    use syn::{Ident, LitChar, LitStr};

    use super::*;
    use crate::parse::Field;

    fn pretty_print(stream: TokenStream) {
        let syntax_tree = syn::parse_file(&stream.to_string());
        let output = prettyplease::unparse(&syntax_tree.unwrap());
        println!("{}", output);
    }

    #[test]
    fn test_gen_top_level() {
        let field = Field {
            name: Ident::new("kotlinFile", Span::call_site()),
        };
        let output = gen_top_level(TopLevelParseEntry {
            field,
            asts: vec![ParseEntry::Ident(
                Ident::new("packageHeader", Span::call_site()),
                Config::default(),
            )],
        });
        pretty_print(output);
    }

    #[test]
    fn test_gen_top_level_with_choice() {
        let field = Field {
            name: Ident::new("kotlinFile", Span::call_site()),
        };
        let output = gen_top_level(TopLevelParseEntry {
            field,
            asts: vec![ParseEntry::Choice(
                vec![
                    ParseEntry::Ident(
                        Ident::new("Identifier", Span::call_site()),
                        Config::default(),
                    ),
                    ParseEntry::Ident(
                        Ident::new("topLevelObject", Span::call_site()),
                        Config::default(),
                    ),
                ],
                Config::default(),
            )],
        });
        pretty_print(output);
    }

    #[test]
    fn test_gen_top_level_with_group() {
        let field = Field {
            name: Ident::new("kotlinFile", Span::call_site()),
        };
        let output = gen_top_level(TopLevelParseEntry {
            field,
            asts: vec![ParseEntry::Group(
                vec![
                    ParseEntry::Ident(
                        Ident::new("Identifier", Span::call_site()),
                        Config::default(),
                    ),
                    ParseEntry::Ident(
                        Ident::new("topLevelObject", Span::call_site()),
                        Config::default(),
                    ),
                ],
                Config::default(),
            )],
        });
        pretty_print(output);
    }
}
