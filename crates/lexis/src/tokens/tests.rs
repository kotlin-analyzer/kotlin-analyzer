use super::KotlinToken;
use lady_deirdre::lexis::{SourceCode, TokenBuffer};

/// remove the spaces in each line of a multiline string
#[macro_export]
macro_rules! trim_idents {
    ($source: literal) => {
        $source
            .lines()
            .map(|l| l.trim_start())
            .collect::<Vec<_>>()
            .join("\n")
    };
}

#[test]
fn token_buffer_test() {
    // Create a new token buffer with an initial source code.
    let buffer = TokenBuffer::<KotlinToken>::from(trim_idents!(
        r"  
            23419
            2_341_567
            2_341_567r
            0b101_010
            0xff_ff_bb
            23419L
            2_341_567L
            0b101_010L
            0xff_ff_bbL
            this
            this@me
            // comment
            #!
        "
    ));

    // Check the initial substring of the buffer.
    // assert_eq!(buffer.substring(..), "[1, 2, 3]");
    println!("{}", buffer.substring(..));

    for chunk in buffer.chunks(..) {
        print!("{:?}", chunk.token);
        print!(" - {:?}", chunk.string);
        print!("@{:?}", chunk.start());
        print!(":{:?}", chunk.end());
        println!()
    }
}

#[test]
fn test_identifiers() {
    let source = r#"
    identifier another `quoted ident`
    Åë_ʶῼ٤ _ident _03_name 23andme
    ``
    angebange`
"#;

    let buffer = TokenBuffer::<KotlinToken>::from(source);
    for chunk in buffer.chunks(..) {
        print!("{:?}", chunk.token);
        print!(" - {:?}", chunk.string);
        print!("@{:?}", chunk.start());
        print!(":{:?}", chunk.end());
        println!()
    }
}
#[test]
fn token_debug() {
    let source = r#"#! shebang
    // This is a comment
    /* multiline comment */
    identifier another
    `espaced identifier`hey
    if fun()
    _ident _03_name
"#;

    let buffer = TokenBuffer::<KotlinToken>::from(source);
    for chunk in buffer.chunks(..) {
        print!("{:?}", chunk.token);
        print!(" - {:?}", chunk.string);
        print!("@{:?}", chunk.start());
        print!(":{:?}", chunk.end());
        println!()
    }
}
