//! This module contains code adapted from rust-analyzer's `parser` crate.
//! HKGIC: Means "Heuristic, Kotlin Grammar Is Crazy", i.e. the code is not a direct translation of the grammar rules,
//! but rather a best effort to parse as much as possible of the input, even if it doesn't conform to the grammar.

#![allow(dead_code)]
mod grammar;
mod syntax_kind;
mod version;

pub use syntax_kind::SyntaxKind;
pub use version::KtVersion;

mod event;
mod input;
mod lexed_str;
mod output;
mod parser;
mod shortcuts;
mod token_set;

#[cfg(test)]
mod tests;

pub use T_ as T;
pub(crate) use parser::*;
pub(crate) use token_set::*;

use crate::{
    input::Input,
    output::{Output, Step},
};

///// Parse the whole of the input as a given syntactic construct.
/////
///// [`TopEntryPoint::parse`] makes a guarantee that
/////   * all input is consumed
/////   * the result is a valid tree (there's one root node)

#[derive(Debug)]
pub enum TopEntryPoint {
    KotlinFile,
    Script,
    Type,
    Expr,
}

impl TopEntryPoint {
    pub fn parse(self, input: &Input) -> Output {
        let _p = tracing::info_span!("parse").entered();
        let entry_point: fn(&'_ mut parser::Parser<'_>) = match self {
            TopEntryPoint::KotlinFile => grammar::entry::top::kotlin_file,
            TopEntryPoint::Script => grammar::entry::top::script,
            TopEntryPoint::Type => grammar::entry::top::type_,
            TopEntryPoint::Expr => grammar::entry::top::expr,
        };
        let mut p = parser::Parser::new(input);
        entry_point(&mut p);
        let danglings = p.dangling.drain(..).collect::<Vec<_>>();

        for dangling in danglings {
            dangling.forget(&mut p);
        }
        let (events, errors) = p.finish();
        let res = event::process(events, errors);

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
}
