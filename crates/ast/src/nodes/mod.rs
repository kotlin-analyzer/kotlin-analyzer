// use crate::ast_node;
use macros::{gen_ast, gen_single_ast};
use syntax::SyntaxNode;

gen_single_ast! {
    simpleIdentifier:
        Identifier
        | "abstract"
        | "annotation"
        | "by"
        | "catch"
        | "companion"
        | "constructor"
        | "crossinline"
        | "data"
        | "dynamic"
        | "enum"
        | "external"
        | "final"
        | "finally"
        | "get"
        | "import"
        | "infix"
        | "init"
        | "inline"
        | "inner"
        | "internal"
        | "lateinit"
        | "noinline"
        | "open"
        | "operator"
        | "out"
        | "override"
        | "private"
        | "protected"
        | "public"
        | "reified"
        | "sealed"
        | "tailrec"
        | "set"
        | "vararg"
        | "where"
        | "field"
        | "property"
        | "receiver"
        | "param"
        | "setparam"
        | "delegate"
        | "file"
        | "expect"
        | "actual"
        | "const"
        | "suspend"
        | "value"
}

gen_single_ast! {
    identifier:
        simpleIdentifier {{NL} "." simpleIdentifier}
}

gen_single_ast! {
    annotationUseSiteTarget:
        (AT_NO_WS | AT_PRE_WS)@At
        ("field" | "property" | "get" | "set" | "receiver" | "param" | "setparam" | "delegate")@AnnotationTarget
        {NL} ':'
}

gen_ast! {
    functionModifier:
    "tailrec"
    | "operator"
    | "infix"
    | "inline"
    | "external"
    | "suspend"

  propertyModifier:
    "const"

  inheritanceModifier:
    "abstract"
    | "final"
    | "open"

  parameterModifier:
    "vararg"
    | "noinline"
    | "crossinline"

  reificationModifier:
    "reified"

  platformModifier:
    "expect"
    | "actual"
}

gen_ast! {
    classModifier:
    "enum"
    | "sealed"
    | "annotation"
    | "data"
    | "inner"
    | "value"

  memberModifier:
    "override"
    | "lateinit"

  visibilityModifier:
    "public"
    | "private"
    | "internal"
    | "protected"

  varianceModifier:
    "in"
    | "out"

}
