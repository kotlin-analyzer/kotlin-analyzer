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

#[test]
fn test_package_header() {
    let source = r#"package org.jetbrains.kotlin.spec.grammar.parsetree"#;
    KotlinNode::debug(source);

    let doc = Document::<KotlinNode>::new_immutable(source);

    for error in doc.errors() {
        println!("{:#}", error.display(&doc));
    }

    println!("{:#}", doc.display(&doc.root_node_ref()));
}

#[test]
fn test_imports() {
    let source = r#"import org.jetbrains.kotlin.spec.grammar.parsetree
    import org.jetbrains.kotlin.spec.grammar.*;
    import org.jetbrains.kotlin.spec.grammar.parsetrees as myparsetree"#;
    KotlinNode::debug(source);

    let doc = Document::<KotlinNode>::new_immutable(source);

    for error in doc.errors() {
        println!("{:#}", error.display(&doc));
    }

    println!("{:#}", doc.display(&doc.root_node_ref()));
}

#[test]
fn test_types() {
    let source = r#"
    MyType 
    Generic<T> 
    Generic<T, U> 
    Generic<in T, out U> 
    List<*>
    Generic<Nested<T>, U> 
    (MyTypeInParen) 
    @DrawableRes Int 
    @DrawableRes.Mine<T, Y> Int 
    (T1, T2) -> T3 
    suspend (T1, T2) -> T3 
    suspend T.(T1, T2) -> T3 
    suspend T?.(T1, T2) -> T3 
    (a: T1, b: T2) -> T3 
    suspend (T).(T1, T2) -> T3 
    suspend (T?).(T1, T2) -> T3 
    @Annotated (T1, T2) -> T3 
    ((T1, T2) -> T3) 
    ((T1, T2) -> T3)?
    T & Any
    "#;
    let source = "Generic<in T, out U>";
    // let source = source
    //     .lines()
    //     .map(|line| line.trim())
    //     .collect::<Vec<_>>()
    //     .join("\n");

    // KotlinNode::debug(&source);

    let doc = Document::<KotlinNode>::new_immutable(source);

    for error in doc.errors() {
        println!("{:#}", error.display(&doc));
    }

    println!("{:#}", doc.display(&doc.root_node_ref()));
}
