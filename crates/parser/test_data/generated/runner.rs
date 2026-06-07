mod ok {
    use crate::tests::*;
    #[test]
    fn class_declaration() {
        run_and_expect_no_errors("test_data/parser/inline/ok/class_declaration.kt");
    }
    #[test]
    fn file_annotation() {
        run_and_expect_no_errors("test_data/parser/inline/ok/file_annotation.kt");
    }
    #[test]
    fn fn_declaration() {
        run_and_expect_no_errors("test_data/parser/inline/ok/fn_declaration.kt");
    }
    #[test]
    fn import_list() { run_and_expect_no_errors("test_data/parser/inline/ok/import_list.kt"); }
    #[test]
    fn lambda_literal() {
        run_and_expect_no_errors("test_data/parser/inline/ok/lambda_literal.kt");
    }
    #[test]
    fn multi_annotation() {
        run_and_expect_no_errors("test_data/parser/inline/ok/multi_annotation.kt");
    }
    #[test]
    fn object_declaration() {
        run_and_expect_no_errors("test_data/parser/inline/ok/object_declaration.kt");
    }
    #[test]
    fn package_header() {
        run_and_expect_no_errors("test_data/parser/inline/ok/package_header.kt");
    }
    #[test]
    fn package_header2() {
        run_and_expect_no_errors("test_data/parser/inline/ok/package_header2.kt");
    }
    #[test]
    fn property_declaration() {
        run_and_expect_no_errors("test_data/parser/inline/ok/property_declaration.kt");
    }
    #[test]
    fn shebang_line() { run_and_expect_no_errors("test_data/parser/inline/ok/shebang_line.kt"); }
    #[test]
    fn single_annotation() {
        run_and_expect_no_errors("test_data/parser/inline/ok/single_annotation.kt");
    }
}
mod err {
    use crate::tests::*;
    #[test]
    fn file_annotation() {
        run_and_expect_errors("test_data/parser/inline/err/file_annotation.kt");
    }
}
