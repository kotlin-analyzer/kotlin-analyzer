#![allow(unused)]

use convert_case::{Case, Casing};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::parse::{Field, GenAst, ParseEntry, TopLevelParseEntry};

pub fn generate_ast(ast: GenAst) -> TokenStream {
    quote! {}
}

fn gen_top_level(top: TopLevelParseEntry) -> TokenStream {
    quote! {}
}

fn gen_parse_entries(entries: &[ParseEntry], field_ident: &Ident, depth: u8) -> TokenStream {
    quote! {}
}

// fn gen_cast_token_fn(name: TokenStream) -> TokenStream {
//     quote! {
//         |node| if node.kind() == ::syntax::SyntaxKind::Token(::tokens::Token::#name) {
//             Some()
//         }
//     }
// }

fn gen_parse_entry(entry: &ParseEntry, field_ident: &Ident, depth: u8) -> TokenStream {
    let typename = format_ident!("{}", field_ident.to_string().to_case(Case::Pascal));
    let methods = match entry {
        ParseEntry::CharLit(ch) => {
            let name = ch.value().to_string();
            let method_name = name.to_case(Case::Camel);
            quote! {
                fn #method_name(&self) -> Option<::tokens::Token> {
                    self.node().children().find_map(ShebangLine::cast)
                }
            }
        }
        ParseEntry::StrLit(_) => todo!(),
        ParseEntry::Ident(_) => todo!(),
        ParseEntry::Optional(_) => todo!(),
        ParseEntry::Repeated(_) => todo!(),
        ParseEntry::Choice(_) => todo!(),
        ParseEntry::Group(_) => todo!(),
    };
    quote! {
        impl #typename {

        }
    }
}

fn gen_field(field: Field) -> TokenStream {
    let name = field.name.to_string();
    let typename = format_ident!("{}", name.to_case(Case::Pascal));
    let syntax_name = format_ident!("{}", name.to_case(Case::ScreamingSnake));
    quote! {
        #[derive(PartialEq, Eq, Hash, Clone)]
        #[repr(transparent)]
        pub struct #typename(::syntax::SyntaxNode);

        impl #typename {
            #[allow(unused)]
            pub fn cast(node: ::syntax::SyntaxNode) -> Option<Self> {
                if node.kind() == ::syntax::SyntaxKind::Syntax(::syntax::Syntax::#syntax_name) {
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
    use syn::Ident;

    use super::*;
    use crate::parse::Field;

    #[test]
    fn field() {
        let output = gen_field(Field {
            name: Ident::new("packageHeader", Span::call_site()),
        });
        let syntax_tree = syn::parse_file(&output.to_string());
        let output = prettyplease::unparse(&syntax_tree.unwrap());
        println!("{}", output);
    }
}
