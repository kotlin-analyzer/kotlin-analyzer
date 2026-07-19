mod ok {
    use crate::tests::*;
    #[test]
    fn anonymous_function() {
        run_and_expect_no_errors("test_data/parser/inline/ok/anonymous_function.kt");
    }
    #[test]
    fn class_declaration() {
        run_and_expect_no_errors("test_data/parser/inline/ok/class_declaration.kt");
    }
    #[test]
    fn class_then_decl() {
        run_and_expect_no_errors("test_data/parser/inline/ok/class_then_decl.kt");
    }
    #[test]
    fn collection_literal() {
        run_and_expect_no_errors("test_data/parser/inline/ok/collection_literal.kt");
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
    fn if_expression() { run_and_expect_no_errors("test_data/parser/inline/ok/if_expression.kt"); }
    #[test]
    fn import_list() { run_and_expect_no_errors("test_data/parser/inline/ok/import_list.kt"); }
    #[test]
    fn lambda_literal() {
        run_and_expect_no_errors("test_data/parser/inline/ok/lambda_literal.kt");
    }
    #[test]
    fn line_string_literal() {
        run_and_expect_no_errors("test_data/parser/inline/ok/line_string_literal.kt");
    }
    #[test]
    fn literal_constant() {
        run_and_expect_no_errors("test_data/parser/inline/ok/literal_constant.kt");
    }
    #[test]
    fn multi_annotation() {
        run_and_expect_no_errors("test_data/parser/inline/ok/multi_annotation.kt");
    }
    #[test]
    fn multi_line_string_literal() {
        run_and_expect_no_errors("test_data/parser/inline/ok/multi_line_string_literal.kt");
    }
    #[test]
    fn object_declaration() {
        run_and_expect_no_errors("test_data/parser/inline/ok/object_declaration.kt");
    }
    #[test]
    fn object_literal() {
        run_and_expect_no_errors("test_data/parser/inline/ok/object_literal.kt");
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
    fn parenthesized_expression() {
        run_and_expect_no_errors("test_data/parser/inline/ok/parenthesized_expression.kt");
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
    #[test]
    fn when_expression() {
        run_and_expect_no_errors("test_data/parser/inline/ok/when_expression.kt");
    }
}
mod err {
    use crate::tests::*;
    #[test]
    fn file_annotation() {
        run_and_expect_errors("test_data/parser/inline/err/file_annotation.kt");
    }
}
