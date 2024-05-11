mod combinators;
mod generate_ast;
mod map;
mod parse;

use parse::GenAst;
use proc_macro::TokenStream;
use syn::parse_macro_input;

#[proc_macro]
pub fn gen_ast(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as GenAst);
    eprintln!("{ast:#?}");
    TokenStream::new()
}
