mod ok {
    use crate::tests::*;
    #[test]
    fn multi_annotation() {
        run_and_expect_no_errors("test_data/parser/inline/ok/multi_annotation.kt");
    }
    #[test]
    fn single_annotation() {
        run_and_expect_no_errors("test_data/parser/inline/ok/single_annotation.kt");
    }
}
