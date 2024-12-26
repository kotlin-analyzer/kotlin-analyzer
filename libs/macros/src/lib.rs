mod combinators;
// mod debug;
mod generate_ast;
mod map;
mod name;
mod parse;

use std::path::PathBuf;

use inflector::Inflector as _;
use parse::{GenAst, TopLevelParseEntry};
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Error, Fields};

#[proc_macro]
pub fn gen_ast(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as GenAst);
    match ast.generate() {
        Ok(stream) => stream.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro]
pub fn gen_single_ast(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as TopLevelParseEntry);
    match ast.generate() {
        Ok(stream) => stream.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro]
pub fn gen_ast_debug(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as TopLevelParseEntry);
    let output_ast = format!("{:#?}", ast);
    let cargo_path: PathBuf = std::env!("CARGO_MANIFEST_DIR").into();
    std::fs::write(cargo_path.join("debug.ron"), output_ast).expect("unable to create file");

    match ast.generate() {
        Ok(stream) => {
            std::fs::write(
                cargo_path.join("debug.rs"),
                pretty_print(stream.clone().into()),
            )
            .expect("unable to create debug.rs file");
            stream.into()
        }
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro_derive(SyntaxKindType)]
pub fn enum_variant_types_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let expanded = match &input.data {
        Data::Enum(data_enum) => {
            let variants = data_enum.variants.iter().map(|variant| {
                let variant_name = &variant.ident;
                let struct_name = syn::Ident::new(
                    &format!("{}", variant_name.to_string().to_pascal_case()),
                    variant_name.span(),
                );

                match &variant.fields {
                    Fields::Unit => {
                        quote! {
                            #[derive(PartialEq, Eq, Hash, Clone)]
                            #[repr(transparent)]
                            pub struct #struct_name(SyntaxNode);

                            impl Cast for #struct_name {
                                fn cast(node: SyntaxNode) -> Option<Self> {
                                    if node.kind() == SyntaxKind::#variant_name {
                                        Some(Self(node))
                                    } else {
                                        None
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        return Error::new(
                            variant.span(),
                            "EnumVariantTypes can only be applied to enums with unit variants",
                        )
                        .to_compile_error();
                    }
                }
            });

            quote! {
                #(#variants)*
            }
        }
        _ => {
            return Error::new(
                input.span(),
                "EnumVariantTypes can only be applied to enums",
            )
            .to_compile_error()
            .into();
        }
    };

    TokenStream::from(expanded)
}

#[allow(unused)]
fn pretty_print(stream: TokenStream) -> String {
    let syntax_tree = syn::parse_file(&stream.to_string());
    prettyplease::unparse(&syntax_tree.unwrap())
}
