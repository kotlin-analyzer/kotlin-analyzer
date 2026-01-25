#[macro_use]
mod utils;

#[test]
fn multi_line_str_test() {
    let source = trim_idents!(
        r#""""
    simple
    """
    """
    complex ${ref}
    "line string inside multi"
    """"""
    """""""""" // open with three, 4 quotes inside and 3 closing
    "#
    );

    let lexer = lexer::Lexer::new(&source).spanned_with_src();
    let entries: Vec<_> = lexer.collect();

    insta::assert_debug_snapshot!(entries);
}

#[test]
fn nested_str_test() {
    let source = r#""hey ${echo("test")} stranger""#;
    let lexer = lexer::Lexer::new(&source).spanned_with_src();
    let entries: Vec<_> = lexer.collect();

    insta::assert_debug_snapshot!(entries);
}

#[test]
fn nested_multi_str_test() {
    let source = trim_idents!(
        r#"
    """
    Can multiline strings
    have ${"""
        other multiline strings
    """.trimIndent()}
    I think yes
    """
    "#
    );

    let lexer = lexer::Lexer::new(&source).spanned_with_src();
    let entries: Vec<_> = lexer.collect();

    insta::assert_debug_snapshot!(entries);
}

#[test]
fn keyword_start() {
    let source = trim_idents!(
        r#"package dev.ikeze.kotlinsyntax

        import kotlin.streams.toList
        
        fun String.length(): Int = this.chars().toList().size
        
        fun main() {
            val len = "simple".length()
            println(len)
        }"#
    );

    let lexer = lexer::Lexer::new(&source).spanned_with_src();
    let entries: Vec<_> = lexer.collect();

    insta::assert_debug_snapshot!(entries);
}

#[test]
fn simple() {
    let source = trim_idents!(
        r#"
        0444.10_99e+4f
        [],--
        /* comments */
        //line comment
        #! sh echo "hey"
        hey
        0b101_010
        0xff_ff
        true
        false
        null
        'A'
        '\uffac'
        '\n'
        this
        this@me
        super
        !in//comment
        /* pre */@man
        name@ // post
          @  // both
        !is/* comment */
        var name: String?/*ddjjd*/ = null;
        var name2: String? /*ddjjd*/ // = null;
        super@me
        continue
        continue@where
        return
        return@here
        break
        break@now
        `backticks baby`
        fun hello() = "Hello"
        var funvar = 3
        """
        simple multi line
        """
        """
        complex "multi line"
        """"""
        "#
    );

    let lexer = lexer::Lexer::new(&source).spanned_with_src();
    let entries: Vec<_> = lexer.collect();

    insta::assert_debug_snapshot!(entries);
}
