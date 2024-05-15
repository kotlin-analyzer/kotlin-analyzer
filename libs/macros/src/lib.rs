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
    let generated = generate_ast::generate_ast(ast);
    pretty_print(generated.into());
    TokenStream::new()
}

fn pretty_print(stream: TokenStream) {
    let syntax_tree = syn::parse_file(&stream.to_string());
    let output = prettyplease::unparse(&syntax_tree.unwrap());
    eprintln!("{}", output);
}
