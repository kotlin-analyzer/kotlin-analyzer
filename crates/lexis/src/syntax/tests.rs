use crate::syntax::KotlinNode;
use lady_deirdre::{
    syntax::{Node, SyntaxTree},
    units::Document,
};

#[test]
fn test_kotlin_node() {
    let source = r#"#! shebang
    // This is a comment
    /* multiline comment */
    identifier another `quoted ident`
    _ident _03_name
    ``
    angebange`
"#;
    KotlinNode::debug(source);

    let doc = Document::<KotlinNode>::new_immutable(source);

    for error in doc.errors() {
        println!("{:#}", error.display(&doc));
    }
}
