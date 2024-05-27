#![allow(unused)]

use inflector::Inflector;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Ident, LitStr};
use tokens::resolve_token;

use crate::parse::{Field, GenAst, ParseEntry, TopLevelParseEntry};

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

    fn cast_closure(&self) -> TokenStream {
        let ty = self.type_name();
        match self {
            SimpleName::Token(_) => quote!(::syntax::cast_syntax_kind(::syntax::SyntaxKind::#ty)),
            SimpleName::Ident(_) => quote!(#ty::cast),
        }
    }
}

struct Single {
    method: Option<Ident>,
    iter_fn: Option<Ident>,
    return_type: Option<TokenStream>,
    cast: Option<TokenStream>,
}

enum GenInterface {
    Single {
        method: Option<Ident>,
        iter_fn: Ident,
        return_type: Option<TokenStream>,
        cast: Option<TokenStream>,
    },
    Many(Vec<GenInterface>),
    Enum {
        variants: Vec<Ident>,
    },
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
        match self {
            ParseEntry::CharLit(ch) => resolve_token(&ch.value().to_string())
                .map(|t| vec![SimpleName::Token(format!("{:?}", t))])
                .unwrap_or_default(),
            ParseEntry::StrLit(st) => resolve_token(&st.value().to_string())
                .map(|t| vec![SimpleName::Token(format!("{:?}", t))])
                .unwrap_or_default(),
            ParseEntry::Ident(id) => {
                if id.to_string().starts_with(char::is_lowercase) {
                    vec![SimpleName::Ident(id.to_string())]
                } else {
                    vec![SimpleName::Token(id.to_string())]
                }
            }
            ParseEntry::Repeated(entries)
            | ParseEntry::Choice(entries)
            | ParseEntry::Group(entries)
            | ParseEntry::Optional(entries) => {
                entries.iter().flat_map(|e| e.identifiers()).collect()
            }
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
}

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
    let syntax_name = format_ident!("{}", top.field.name.to_string().to_screaming_snake_case());
    quote! {
        #field
        impl #typename {
            #(#entries)*
        }
    };
    quote! {
        #[derive(PartialEq, Eq, Hash, Clone)]
        #[repr(transparent)]
        // todo add doc
        pub struct #typename(::syntax::SyntaxNode);

        impl #typename {
            use ::syntax::{ SyntaxNode, SyntaxKind };
            #[allow(unused)]
            pub fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == SyntaxKind::#syntax_name {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
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

fn process_entry(entry: &ParseEntry, ret_type: RetType) -> GenInterface {
    match entry {
        ParseEntry::CharLit(_) | ParseEntry::StrLit(_) | ParseEntry::Ident(_) => {
            GenInterface::Single {
                method: ret_type.method_name(entry),
                iter_fn: ret_type.iter_fn(),
                return_type: ret_type.return_type(entry),
                cast: entry.simple_name().map(|s| s.cast_closure()),
            }
        }
        ParseEntry::Optional(entries) => {
            GenInterface::Many(entries.iter().map(|e| process_entry(e, ret_type)).collect())
        }
        ParseEntry::Repeated(entries) => GenInterface::Many(
            entries
                .iter()
                .map(|e| process_entry(e, RetType::Many))
                .collect(),
        ),
        ParseEntry::Choice(entries) => {
            let variants = entries
                .iter()
                .flat_map(|e| e.type_name().map(|id| format_ident!("{}", id)))
                .collect();
            GenInterface::Enum { variants }
        }
        ParseEntry::Group(entries) => {
            GenInterface::Many(entries.iter().map(|e| process_entry(e, ret_type)).collect())
        }
    }
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
