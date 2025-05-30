mod common;

use common::parse;

#[test]
fn integrate() {
    let text = "#! comment // nested #! deep /* more */\n";
    let node = parse(text).syntax();
    println!("{node:?}");
    println!("children count: {}", node.children().count());
    let next = node.children().next().unwrap();
    println!("{next:?}");
    let children = next
        .children_with_tokens()
        .map(|child| format!("{:?}@{:?} - {}", child.kind(), child.text_range(), child))
        .collect::<Vec<_>>();
    print!("{children:?}");
}

#[test]
fn with_ast() {
    let text = "#! comment // nested #! deep /* more */\n";
    let root = parse(text).root();
    println!("{root:?}");

    let node = root.0;
    println!("children count: {}", node.children().count());
    let next = node.children().next().unwrap();
    println!("{next:?}");
    let children = next
        .children_with_tokens()
        .map(|child| format!("{:?}@{:?}", child.kind(), child.text_range()))
        .collect::<Vec<_>>();
    print!("{children:?}");
}
