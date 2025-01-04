#[macro_use]
mod utils;

use tokens::Token::*;

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

    lexer_matches!(&source, [
        TRIPLE_QUOTE_OPEN => 0..3, // "\"\"\"",
        MULTI_LINE_STR_TEXT => 3..11, // "\nsimple\n",
        TRIPLE_QUOTE_CLOSE => 11..14, // "\"\"\"",
        NL => 14..15, // "\n",
        TRIPLE_QUOTE_OPEN => 15..18, // "\"\"\"",
        MULTI_LINE_STR_TEXT => 18..27, // "\ncomplex ",
        MULTI_STR_EXPR_START => 27..29, // "${",
        IDENTIFIER => 29..32, // "ref",
        R_CURL => 32..33, // "}",
        MULTI_LINE_STR_TEXT => 33..34, // "\n",
        QUOTE_OPEN => 34..35, // "\"",
        LINE_STR_TEXT => 35..59, // "line string inside multi",
        QUOTE_CLOSE => 59..60, // "\"",
        MULTI_LINE_STR_TEXT => 60..61, // "\n",
        MULTI_LINE_STRING_QUOTE => 61..64, // "\"\"\"",
        TRIPLE_QUOTE_CLOSE => 64..67, // "\"\"\"",
        NL => 67..68, // "\n",
        TRIPLE_QUOTE_OPEN => 68..71, // "\"\"\"",
        MULTI_LINE_STRING_QUOTE => 71..75, // "\"\"\"\"",
        TRIPLE_QUOTE_CLOSE => 75..78, // "\"\"\"",
        WS => 78..79, // " ",
        LINE_COMMENT => 79..128, // "// open with three, 4 quotes inside and 3 closing",
        NL => 128..129, // "\n",
        EOF => 129..129
    ]);
}

#[test]
fn nested_str_test() {
    let source = r#""hey ${echo("test")} stranger""#;
    lexer_matches!(source, [
        QUOTE_OPEN => 0..1,
        LINE_STR_TEXT => 1..5,
        LINE_STR_EXPR_START => 5..7,
        IDENTIFIER => 7..11,
        L_PAREN => 11..12,
        QUOTE_OPEN => 12..13,
        LINE_STR_TEXT => 13..17,
        QUOTE_CLOSE => 17..18,
        R_PAREN => 18..19,
        R_CURL => 19..20,
        LINE_STR_TEXT => 20..29,
        QUOTE_CLOSE => 29..30,
        EOF => 30..30
    ]);
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

    lexer_matches!(&source, [
        NL => 0..1, // "\n",
        TRIPLE_QUOTE_OPEN => 1..4, // "\"\"\"",
        MULTI_LINE_STR_TEXT => 4..32, // "\nCan multiline strings\nhave ",
        MULTI_STR_EXPR_START => 32..34, // "${",
        TRIPLE_QUOTE_OPEN => 34..37, // "\"\"\"",
        MULTI_LINE_STR_TEXT => 37..62, // "\nother multiline strings\n",
        TRIPLE_QUOTE_CLOSE => 62..65, // "\"\"\"",
        DOT => 65..66, // ".",
        IDENTIFIER => 66..76, // "trimIndent",
        L_PAREN => 76..77, // "(",
        R_PAREN => 77..78, // ")",
        R_CURL => 78..79, // "}",
        MULTI_LINE_STR_TEXT => 79..92, // "\nI think yes\n",
        TRIPLE_QUOTE_CLOSE => 92..95, // "\"\"\"",
        NL => 95..96, // "\n",
        EOF => 96..96
    ]);
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

    dbg_lexer!(&source);

    lexer_matches!(&source, [
        PACKAGE => 0..7, // "package",
        WS => 7..8, // " ",
        IDENTIFIER => 8..11, // "dev",
        DOT => 11..12, // ".",
        IDENTIFIER => 12..17, // "ikeze",
        DOT => 17..18, // ".",
        IDENTIFIER => 18..30, // "kotlinsyntax",
        NL => 30..31, // "\n",
        NL => 31..32, // "\n",
        IMPORT => 32..38, // "import",
        WS => 38..39, // " ",
        IDENTIFIER => 39..45, // "kotlin",
        DOT => 45..46, // ".",
        IDENTIFIER => 46..53, // "streams",
        DOT => 53..54, // ".",
        IDENTIFIER => 54..60, // "toList",
        NL => 60..61, // "\n",
        NL => 61..62, // "\n",
        FUN => 62..65, // "fun",
        WS => 65..66, // " ",
        IDENTIFIER => 66..72, // "String",
        DOT => 72..73, // ".",
        IDENTIFIER => 73..79, // "length",
        L_PAREN => 79..80, // "(",
        R_PAREN => 80..81, // ")",
        COLON => 81..82, // ":",
        WS => 82..83, // " ",
        IDENTIFIER => 83..86, // "Int",
        WS => 86..87, // " ",
        ASSIGNMENT_TOKEN => 87..88, // "=",
        WS => 88..89, // " ",
        THIS => 89..93, // "this",
        DOT => 93..94, // ".",
        IDENTIFIER => 94..99, // "chars",
        L_PAREN => 99..100, // "(",
        R_PAREN => 100..101, // ")",
        DOT => 101..102, // ".",
        IDENTIFIER => 102..108, // "toList",
        L_PAREN => 108..109, // "(",
        R_PAREN => 109..110, // ")",
        DOT => 110..111, // ".",
        IDENTIFIER => 111..115, // "size",
        NL => 115..116, // "\n",
        NL => 116..117, // "\n",
        FUN => 117..120, // "fun",
        WS => 120..121, // " ",
        IDENTIFIER => 121..125, // "main",
        L_PAREN => 125..126, // "(",
        R_PAREN => 126..127, // ")",
        WS => 127..128, // " ",
        L_CURL => 128..129, // "{",
        NL => 129..130, // "\n",
        VAL => 130..133, // "val",
        WS => 133..134, // " ",
        IDENTIFIER => 134..137, // "len",
        WS => 137..138, // " ",
        ASSIGNMENT_TOKEN => 138..139, // "=",
        WS => 139..140, // " ",
        QUOTE_OPEN => 140..141, // "\"",
        LINE_STR_TEXT => 141..147, // "simple",
        QUOTE_CLOSE => 147..148, // "\"",
        DOT => 148..149, // ".",
        IDENTIFIER => 149..155, // "length",
        L_PAREN => 155..156, // "(",
        R_PAREN => 156..157, // ")",
        NL => 157..158, // "\n",
        IDENTIFIER => 158..165, // "println",
        L_PAREN => 165..166, // "(",
        IDENTIFIER => 166..169, // "len",
        R_PAREN => 169..170, // ")",
        NL => 170..171, // "\n",
        R_CURL => 171..172, // "}",
        EOF => 172..172
    ]);
}

#[test]
#[ignore]
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

    dbg_lexer_src!(&source);
}
