mod combinators;
mod generate_ast;
mod map;
mod name;
mod parse;

use std::path::{Path, PathBuf};

use parse::{GenAst, TopLevelParseEntry};
use proc_macro::TokenStream;
use syn::parse_macro_input;

#[proc_macro]
pub fn gen_ast(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as GenAst);
    match generate_ast::generate_ast(ast) {
        Ok(stream) => stream.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro]
pub fn gen_single_ast(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as TopLevelParseEntry);
    match generate_ast::gen_top_level(ast) {
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

    match generate_ast::gen_top_level(ast) {
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

#[allow(unused)]
fn pretty_print(stream: TokenStream) -> String {
    let syntax_tree = syn::parse_file(&stream.to_string());
    prettyplease::unparse(&syntax_tree.unwrap())
}
