use crate::syntax::KotlinNode;
use lady_deirdre::{
    syntax::{Node, SyntaxTree},
    units::{CompilationUnit, Document},
};

#[test]
fn test_kotlin_node() {
    let source = r#"#! shebang
    // This is a comment
    /* multiline comment */
"#;
    KotlinNode::debug(source);

    let doc = Document::<KotlinNode>::new_immutable(source);

    for error in doc.errors() {
        println!("{:#}", error.display(&doc));
    }

    println!("{:#}", doc.display(&doc.root_node_ref()));
}
