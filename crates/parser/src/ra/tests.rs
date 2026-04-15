#[macro_export]
macro_rules! panic_context {
    ($($arg:tt)+) => {
        panic_context::PanicContext::new(format!($($arg)+))
    }
}

use std::{
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

use expect_test::expect_file;

use crate::{
    grammar::annotations::annotation,
    grammar::types::ty,
    ra::{self, lexed_str::LexedStr, parser, shortcuts::StrStep},
    version::KtVersion,
};

// use expect_test::expect_file;

// use crate::{Edition, LexedStr, TopEntryPoint};

#[rustfmt::skip]
#[path = "../../test_data/generated/runner.rs"]
mod runner;

// fn infer_edition(file_path: &Path) -> Edition {
//     let file_content = std::fs::read_to_string(file_path).unwrap();
//     if let Some(edition) = file_content.strip_prefix("//@ edition: ") {
//         edition[..4].parse().expect("invalid edition directive")
//     } else {
//         Edition::CURRENT
//     }
// }

// #[test]
// fn lex_ok() {
//     for case in TestCase::list("lexer/ok") {
//         let _guard = panic_context!("{:?}", case.rs);
//         let actual = lex(&case.text, KtVersion::V2_3);
//         expect_file![case.rast].assert_eq(&actual)
//     }
// }

// #[test]
// fn lex_err() {
//     for case in TestCase::list("lexer/err") {
//         let _guard = panic_context!("{:?}", case.rs);
//         let actual = lex(&case.text, KtVersion::V2_3);
//         expect_file![case.rast].assert_eq(&actual)
//     }
// }

// fn lex(text: &str, version: KtVersion) -> String {
//     let lexed = super::lexed_str::LexedStr::new(version, text);

//     let mut res = String::new();
//     for i in 0..lexed.len() {
//         let kind = lexed.kind(i);
//         let text = lexed.text(i);
//         let error = lexed.error(i);

//         let error = error.map(|err| format!(" error: {err}")).unwrap_or_default();
//         writeln!(res, "{kind:?} {text:?}{error}").unwrap();
//     }
//     res
// }

#[test]
fn parse_ok() {
    for case in TestCase::list("parser/ok") {
        let _guard = panic_context!("{:?}", case.kt);
        let (actual, errors) = parse(ty, &case.text, KtVersion::V2_3);
        assert!(
            !errors,
            "errors in an OK file {}:\n{actual}",
            case.kt.display()
        );
        expect_file![case.kast].assert_eq(&actual);
    }
}

#[test]
fn parse_err() {
    for case in TestCase::list("parser/err") {
        let _guard = panic_context!("{:?}", case.kt);
        let (actual, errors) = parse(ty, &case.text, KtVersion::V2_3);
        assert!(
            errors,
            "no errors in an ERR file {}:\n{actual}",
            case.kt.display()
        );
        expect_file![case.kast].assert_eq(&actual)
    }
}

pub fn parse<T>(
    entry_point: fn(&'_ mut parser::Parser<'_>) -> T,
    text: &str,
    version: KtVersion,
) -> (String, bool) {
    let lexed = LexedStr::new(version, text);
    let input = lexed.to_input(version);
    let output = ra::parse(entry_point, &input);

    let mut buf = String::new();
    let mut errors = Vec::new();
    let mut indent = String::new();
    let mut depth = 0;
    let mut len = 0;
    lexed.intersperse_trivia(&output, &mut |step| match step {
        StrStep::Token { kind, text } => {
            assert!(depth > 0);
            len += text.len();
            writeln!(buf, "{indent}{kind:?} {text:?}").unwrap();
        }
        StrStep::Enter { kind } => {
            assert!(depth > 0 || len == 0);
            depth += 1;
            writeln!(buf, "{indent}{kind:?}").unwrap();
            indent.push_str("  ");
        }
        StrStep::Exit => {
            assert!(depth > 0);
            depth -= 1;
            indent.pop();
            indent.pop();
        }
        StrStep::Error { msg, pos } => {
            assert!(depth > 0);
            errors.push(format!("error {pos}: {msg}\n"))
        }
    });

    for (token, msg) in lexed.errors() {
        let pos = lexed.text_start(token);
        errors.push(format!("error {pos}: {msg}\n"));
    }

    let has_errors = !errors.is_empty();
    for e in errors {
        buf.push_str(&e);
    }
    assert_eq!(
        len,
        text.len(),
        "didn't parse all text.\nParsed:\n{}\n\nAll:\n{}\nTree:\n{}\n",
        &text[..len],
        text,
        buf,
    );

    (buf, has_errors)
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct TestCase {
    kt: PathBuf,
    kast: PathBuf,
    text: String,
}

impl TestCase {
    fn list(path: &'static str) -> Vec<TestCase> {
        let crate_root_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let test_data_dir = crate_root_dir.join("test_data");
        let dir = test_data_dir.join(path);

        let mut res = Vec::new();
        let read_dir = fs::read_dir(&dir)
            .unwrap_or_else(|err| panic!("can't `read_dir` {}: {err}", dir.display()));
        for file in read_dir {
            let file = file.unwrap();
            let path = file.path();
            if path.extension().unwrap_or_default() == "kt" {
                let kt = path;
                let kast = kt.with_extension("kast");
                let text = fs::read_to_string(&kt).unwrap();
                res.push(TestCase { kt, kast, text });
            }
        }
        res.sort();
        res
    }
}

#[track_caller]
fn run_and_expect_no_errors(path: &str) {
    run_and_expect_no_errors_with_version(path, KtVersion::V2_3)
}

#[track_caller]
fn run_and_expect_errors(path: &str) {
    run_and_expect_errors_with_version(path, KtVersion::V2_3)
}

#[track_caller]
fn run_and_expect_no_errors_with_version(path: &str, version: KtVersion) {
    let path = PathBuf::from(path);
    let text = std::fs::read_to_string(&path).unwrap();
    let (actual, errors) = parse(annotation, &text, version);
    assert!(
        !errors,
        "errors in an OK file {}:\n{actual}",
        path.display()
    );
    let mut p = PathBuf::from("..");
    p.push(path);
    p.set_extension("kast");
    expect_file![p].assert_eq(&actual)
}

#[track_caller]
fn run_and_expect_errors_with_version(path: &str, version: KtVersion) {
    let path = PathBuf::from(path);
    let text = std::fs::read_to_string(&path).unwrap();
    let (actual, errors) = parse(annotation, &text, version);
    assert!(
        errors,
        "no errors in an ERR file {}:\n{actual}",
        path.display()
    );
    let mut p = PathBuf::from("..");
    p.push(path);
    p.set_extension("kast");
    expect_file![p].assert_eq(&actual)
}
