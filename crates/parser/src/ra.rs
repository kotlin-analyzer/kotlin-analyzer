//! This module contains code adapted from rust-analyzer's `ra_parser` crate, which is licensed under the MIT License.

mod event;
mod input;
mod lexed_str;
mod output;
mod parser;
mod shortcuts;
mod syntax_kind;
mod token_set;

#[cfg(test)]
mod tests;

pub(crate) use parser::*;
pub(crate) use token_set::*;

use crate::ra::{
    input::Input,
    output::{Output, Step},
};

///// Parse the whole of the input as a given syntactic construct.
/////
///// This covers two main use-cases:
/////
/////   * Parsing a Rust file.
/////   * Parsing a result of macro expansion.
/////
///// That is, for something like
/////
///// ```ignore
///// quick_check! {
/////    fn prop() {}
///// }
///// ```
/////
///// the input to the macro will be parsed with [`PrefixEntryPoint::Item`], and
///// the result will be [`TopEntryPoint::MacroItems`].
/////
///// [`TopEntryPoint::parse`] makes a guarantee that
/////   * all input is consumed
/////   * the result is a valid tree (there's one root node)
// #[derive(Debug)]
// pub enum TopEntryPoint {
//     SourceFile,
//     // Type,
//     // Expr,
// }

// impl TopEntryPoint {
pub fn parse<T>(entry_point: fn(&'_ mut parser::Parser<'_>) -> T, input: &Input) -> Output {
    let _p = tracing::info_span!("parse").entered();
    // let entry_point: fn(&'_ mut parser::Parser<'_>) = match self {
    //     TopEntryPoint::SourceFile => grammar2::annotations::annotation,
    //     // TopEntryPoint::Type => grammar2::entry::top::type_,
    //     // TopEntryPoint::Expr => grammar2::entry::top::expr,
    // };
    let mut p = parser::Parser::new(input);
    entry_point(&mut p);
    let events = p.finish();
    let res = event::process(events);

    if cfg!(debug_assertions) {
        let mut depth = 0;
        let mut first = true;
        for step in res.iter() {
            assert!(depth > 0 || first);
            first = false;
            match step {
                Step::Enter { .. } => depth += 1,
                Step::Exit => depth -= 1,
                Step::Token { .. } | Step::Error { .. } => (),
            }
        }
        assert!(!first, "no tree at all");
        assert_eq!(depth, 0, "unbalanced tree");
    }

    res
}
// }
