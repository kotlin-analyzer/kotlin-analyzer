//! source: https://kotlinlang.org/spec/syntax-and-grammar.html#syntax-grammar
// include!(concat!(env!("OUT_DIR"), "/hello.rs"));
pub mod cast;
mod nodes;
mod syntax;
//use nodes::*;
// use serde::Serialize;
// use syntax::SyntaxNode;

// #[macro_export]
// macro_rules! ast_node {
//     ($ast:ident, $kind:ident) => {
//         #[derive(PartialEq, Eq, Hash, Clone)]
//         #[repr(transparent)]
//         pub struct $ast(::syntax::SyntaxNode);
//         impl $ast {
//             #[allow(unused)]
//             pub fn cast(node: ::syntax::SyntaxNode) -> Option<Self> {
//                 if node.kind() == $kind {
//                     Some(Self(node))
//                 } else {
//                     None
//                 }
//             }
//         }
//     };
// }

// #[derive(PartialEq, Eq, Hash, Clone)]
// #[repr(transparent)]
// pub struct Root(SyntaxNode);

// pub enum RootKind {
//     File(KotlinFile),
//     Script(KotlinScript),
// }

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

// impl Root {
//     pub fn cast(node: SyntaxNode) -> Option<Self> {
//         if KotlinFile::cast(node.clone()).is_some() || KotlinScript::cast(node.clone()).is_some() {
//             Some(Root(node))
//         } else {
//             None
//         }
//     }

//     pub fn kind(&self) -> RootKind {
//         KotlinFile::cast(self.0.clone())
//             .map(RootKind::File)
//             .or(KotlinScript::cast(self.0.clone()).map(RootKind::Script))
//             .unwrap()
//     }
// }
