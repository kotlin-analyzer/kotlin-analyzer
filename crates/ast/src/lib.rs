//! source: https://kotlinlang.org/spec/syntax-and-grammar.html#syntax-grammar
// include!(concat!(env!("OUT_DIR"), "/hello.rs"));
pub mod cast;
pub mod nodes;
pub mod syntax;
//use nodes::*;
// use serde::Serialize;
// use syntax::SyntaxNode;

// // todo: generate for all asts
// impl Serialize for Root {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: serde::Serializer,
//     {
//         match self.kind() {
//             RootKind::File(_) => serializer.serialize_newtype_variant("Root", 0, "File", "<todo>"),
//             RootKind::Script(_) => {
//                 serializer.serialize_newtype_variant("Root", 0, "Script", "<todo>")
//             }
//         }
//     }
// }
