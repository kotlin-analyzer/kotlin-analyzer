/// Prints out all the tokens with their respective spans and substrings
/// ```ignore
/// let source = "data class Hey()"
/// dbg_lexer_src!(&source);
/// ```
#[macro_export]
macro_rules! dbg_lexer_src {
    ($source: expr) => {
        let source: &str = $source;
        let lexer = ::lexer::Lexer::new(&source).spanned_with_src();
        println!("Source of 0..{}", source.len() - 1);
        for entry in lexer {
            println!("{},", entry);
        }
    };
}

/// Prints out all the tokens with their respective spans
/// ```ignore
/// let source = "data class Hey()"
/// dbg_lexer!(&source);
/// ```
#[macro_export]
macro_rules! dbg_lexer {
    ($source: expr) => {
        let source: &str = $source;
        let lexer = ::lexer::Lexer::new(&source).spanned();
        println!("Source of 0..{}", source.len() - 1);
        for entry in lexer {
            println!("{},", entry);
        }
    };
}

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
