use macros::gen_ast;

#[test]
fn print_input() {
    gen_ast!(
        kotlinFile:
            [shebangLine]
            {NL}
            {fileAnnotation}
            packageHeader
            importList
            {topLevelObject}
            EOF

        script:
            [shebangLine]
            {NL}
            {fileAnnotation}
            packageHeader
            "idea"
            importList
            {statement semi}
            EOF
    );
}
