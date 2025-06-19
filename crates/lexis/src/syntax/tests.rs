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
    identifier another `quoted ident`
    Åë_ʶῼ٤ _ident _03_name
    ``
    angebange`
"#;
    KotlinNode::debug(source);

    let doc = Document::<KotlinNode>::new_immutable(source);

    for error in doc.errors() {
        println!("{:#}", error.display(&doc));
    }

    println!("{:#}", doc.display(&doc.root_node_ref()));
}

#[test]
fn test_comments() {
    let source = r#"#! shebang shell
        // This is a comment
        /* multiline comment */
        /* nested multiline
            /* comment */
            // comment 2
        */
        indent
    "#;
    KotlinNode::debug(source);

    let doc = Document::<KotlinNode>::new_immutable(source);

    for error in doc.errors() {
        println!("{:#}", error.display(&doc));
    }

    println!("{:#}", doc.display(&doc.root_node_ref()));
}
