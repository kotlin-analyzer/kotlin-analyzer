#![allow(unused)]

use inflector::Inflector;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Ident, LitStr};
use tokens::resolve_token;

use crate::parse::{Field, GenAst, ParseEntry, TopLevelParseEntry};

pub fn generate_ast(ast: GenAst) -> TokenStream {
    let entries = ast.entries.into_iter().map(gen_top_level);
    quote! {
        #(#entries)*
    }
}

fn gen_top_level(top: TopLevelParseEntry) -> TokenStream {
    let typename = format_ident!("{}", top.field.name.to_string().to_pascal_case());
    let entries = top
        .asts
        .iter()
        .map(|entry| gen_parse_entry(entry, &top.field.name, 0, RetType::Optional));
    let field = gen_field(&top.field);
    quote! {
        #field
        impl #typename {
            #(#entries)*
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum RetType {
    Optional,
    Many,
}

fn gen_parse_entry(
    entry: &ParseEntry,
    field_ident: &Ident,
    depth: u8,
    ret_type: RetType,
) -> TokenStream {
    let typename = format_ident!("{}", field_ident.to_string().to_pascal_case());
    match entry {
        ParseEntry::CharLit(_) | ParseEntry::StrLit(_) => {
            let name = match entry {
                ParseEntry::CharLit(ch) => ch.value().to_string(),
                ParseEntry::StrLit(st) => st.value().to_string(),
                _ => unreachable!(),
            };
            let token = resolve_token(&name);

            if let Some(token) = token {
                let token_fmt = format!("{:?}", token);

                let method_name = match ret_type {
                    RetType::Optional => format_ident!("get_{}", token_fmt.to_snake_case()),
                    RetType::Many => format_ident!("get_{}", token_fmt.to_snake_case().to_plural()),
                };

                let syntax_name = format_ident!("{}", token_fmt);

                let iter_fn = if matches!(ret_type, RetType::Many) {
                    format_ident!("filter_map")
                } else {
                    format_ident!("find_map")
                };

                let return_type = match ret_type {
                    RetType::Optional => quote!(Option<::syntax::SyntaxNode>),
                    RetType::Many => quote!(impl Iterator<Item = ::syntax::SyntaxNode> + '_),
                };
                quote! {
                    fn #method_name(&self) -> #return_type {
                        self.node().children().#iter_fn(::syntax::cast_syntax_kind(::syntax::SyntaxKind::#syntax_name))
                    }
                }
            } else {
                // could not resolve token - should be rare but can happen
                quote! {}
            }
        }
        ParseEntry::Ident(id) => {
            if id
                .to_string()
                .chars()
                .nth(0)
                .map(|ch| ch.is_lowercase())
                .unwrap_or_default()
            {
                let method_name = match ret_type {
                    RetType::Optional => format_ident!("{}", id.to_string().to_snake_case()),
                    RetType::Many => {
                        format_ident!("{}", id.to_string().to_plural().to_snake_case())
                    }
                };
                let type_name = format_ident!("{}", id.to_string().to_pascal_case());

                let return_type = match ret_type {
                    RetType::Optional => quote!(Option<#type_name>),
                    RetType::Many => quote!(impl Iterator<Item = #type_name> + '_),
                };
                let iter_fn = if matches!(ret_type, RetType::Many) {
                    format_ident!("filter_map")
                } else {
                    format_ident!("find_map")
                };

                quote! {
                    fn #method_name(&self) -> #return_type {
                        self.node().children().#iter_fn(#type_name::cast)
                    }
                }
            } else {
                let method_name = match ret_type {
                    RetType::Optional => format_ident!("get_{}", id.to_string().to_snake_case()),
                    RetType::Many => {
                        format_ident!("get_{}", id.to_string().to_snake_case().to_plural())
                    }
                };

                let syntax_name = format_ident!("{}", id.to_string());

                let iter_fn = if matches!(ret_type, RetType::Many) {
                    format_ident!("filter_map")
                } else {
                    format_ident!("find_map")
                };

                let return_type = match ret_type {
                    RetType::Optional => quote!(Option<::syntax::SyntaxNode>),
                    RetType::Many => quote!(impl Iterator<Item = ::syntax::SyntaxNode> + '_),
                };
                quote! {
                    fn #method_name(&self) -> #return_type {
                        self.node().children().#iter_fn(::syntax::cast_syntax_kind(::syntax::SyntaxKind::#syntax_name))
                    }
                }
            }
        }
        ParseEntry::Optional(entries) => {
            let methods = entries.iter().map(|e| match e {
                ParseEntry::CharLit(_) | ParseEntry::StrLit(_) | ParseEntry::Ident(_) => {
                    gen_parse_entry(e, field_ident, depth + 1, ret_type)
                }
                // treat as the same level
                ParseEntry::Optional(_) => gen_parse_entry(e, field_ident, depth + 1, ret_type),
                ParseEntry::Repeated(_) => quote!(),
                ParseEntry::Choice(_) => quote!(),
                ParseEntry::Group(_) => quote!(),
            });
            quote! {
                #(#methods)*
            }
        }
        ParseEntry::Repeated(entries) => {
            let methods = entries.iter().map(|e| match e {
                ParseEntry::CharLit(_) | ParseEntry::StrLit(_) | ParseEntry::Ident(_) => {
                    gen_parse_entry(e, field_ident, depth + 1, RetType::Many)
                }
                // treat as the same level
                ParseEntry::Optional(_) => {
                    gen_parse_entry(e, field_ident, depth + 1, RetType::Many)
                }
                ParseEntry::Repeated(_) => {
                    gen_parse_entry(e, field_ident, depth + 1, RetType::Many)
                }
                ParseEntry::Choice(_) => quote!(),
                ParseEntry::Group(_) => quote!(),
            });
            quote! {
                #(#methods)*
            }
        }
        ParseEntry::Choice(entries) => {
            // todo: generate kind type here - depth would come in handy here
            let methods = entries.iter().map(|e| match e {
                ParseEntry::Choice(_) => unreachable!("can not have choice within choice"),
                _ => gen_parse_entry(e, field_ident, depth + 1, ret_type),
            });
            quote! {
                #(#methods)*
            }
        }
        ParseEntry::Group(entries) => {
            let methods = entries
                .iter()
                .map(|e| gen_parse_entry(e, field_ident, depth + 1, ret_type));
            quote! {
                #(#methods)*
            }
        }
    }
}

fn gen_field(field: &Field) -> TokenStream {
    let name = field.name.to_string();
    let typename = format_ident!("{}", name.to_pascal_case());
    let syntax_name = format_ident!("{}", name.to_screaming_snake_case());
    quote! {
        #[derive(PartialEq, Eq, Hash, Clone)]
        #[repr(transparent)]
        // todo add doc
        pub struct #typename(::syntax::SyntaxNode);

        impl #typename {
            #[allow(unused)]
            pub fn cast(node: ::syntax::SyntaxNode) -> Option<Self> {
                if node.kind() == ::syntax::SyntaxKind::#syntax_name {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }
    }
}

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
    fn field() {
        let output = gen_field(&Field {
            name: Ident::new("packageHeader", Span::call_site()),
        });
        pretty_print(output);
    }

    #[test]
    fn test_lit_char_entry() {
        let field = Field {
            name: Ident::new("packageHeader", Span::call_site()),
        };
        let output = gen_parse_entry(
            &ParseEntry::CharLit(LitChar::new('=', Span::call_site())),
            &field.name,
            0,
            RetType::Optional,
        );
        pretty_print(output);
    }

    #[test]
    fn test_lit_str_entry() {
        let field = Field {
            name: Ident::new("packageHeader", Span::call_site()),
        };
        let output = gen_parse_entry(
            &ParseEntry::StrLit(LitStr::new("where", Span::call_site())),
            &field.name,
            0,
            RetType::Many,
        );
        pretty_print(output);
    }

    #[test]
    fn test_lit_ident_entry() {
        let field = Field {
            name: Ident::new("kotlinFile", Span::call_site()),
        };
        let output = gen_parse_entry(
            &ParseEntry::Ident(Ident::new("packageHeader", Span::call_site())),
            &field.name,
            0,
            RetType::Many,
        );
        pretty_print(output);
    }
}
