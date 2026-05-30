mod ok {
    use crate::tests::*;
    #[test]
    fn class_declaration() {
        run_and_expect_no_errors("test_data/parser/inline/ok/class_declaration.kt");
    }
    #[test]
    fn fn_declaration() {
        run_and_expect_no_errors("test_data/parser/inline/ok/fn_declaration.kt");
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
    fn single_annotation() {
        run_and_expect_no_errors("test_data/parser/inline/ok/single_annotation.kt");
    }
}
mod err {
    use crate::tests::*;
}
