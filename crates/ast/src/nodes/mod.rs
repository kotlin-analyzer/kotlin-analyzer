//! These provides ASTS types, generated by a proc macro
//! NB: places where `@ is used after an identifier is used to provide a new name for that AST section
//! while, `_` before, is used to ignore a section from making it to the code
//! The source for the listing here is taken from the Kotlin specifications
use macros::{gen_ast, gen_single_ast};

#[allow(unused)]
use macros::gen_ast_debug;
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
        (AT_NO_WS | AT_PRE_WS)@AnnotationUseSiteTargetAt
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

gen_single_ast! {
  assignmentAndOperator:
  "+="
  | "-="
  | "*="
  | "/="
  | "%="
}

gen_ast_debug! {
  postfixUnaryOperator:
  "++"
  | "--"
  | ("!" excl)@BangExcl
}

gen_ast! {
  equalityOperator:
    "!="
    | "!=="
    | "=="
    | "==="

  comparisonOperator:
    "<"
    | ">"
    | "<="
    | ">="

  inOperator:
  "in"
  | NOT_IN

  isOperator:
    "is"
    | NOT_IS

  additiveOperator:
    "+"
    | "-"

  multiplicativeOperator:
    "*"
    | "/"
    | "%"

  asOperator:
    "as"
    | "as?"

  prefixUnaryOperator:
    "++"
    | "--"
    | "-"
    | "+"
    | excl

  excl:
    "!"
    | EXCL_WS

  memberAccessOperator:
    ({NL} ".")@MemberAccessOperatorDot
    | ({NL} safeNav)@MemberAccessOperatorDotSafeNav
    | "::"

  safeNav:
    QUEST_NO_WS "."
}

gen_single_ast! {
  fileAnnotation:
  (AT_NO_WS | AT_PRE_WS)@FileAnnotationAt
  "file"
  {NL}
  ":"
  {NL}
  (("[" (unescapedAnnotation {unescapedAnnotation}) "]")@BoxedUnescapedAnnotation | unescapedAnnotation)@FileUnescapedAnnotation
  {NL}
}

gen_single_ast! {
  unescapedAnnotation:
  constructorInvocation
  | userType
}

gen_single_ast! {
  constructorInvocation:
  userType {NL} valueArguments
}

gen_single_ast! {
    valueArguments:
    "(" {NL} [valueArgument {{NL} "," {NL} valueArgument} [{NL} ","] {NL}] ")"
}

gen_single_ast! {
  valueArgument:
  [annotation]
  {NL}
  [simpleIdentifier {NL} "=" {NL}]
  ["*"]
  {NL}
  expression
}

gen_single_ast! {
  annotation:
  (singleAnnotation | multiAnnotation) {NL}
}

gen_single_ast! {
  userType:
  simpleUserType {{NL} "." {NL} simpleUserType}
}

gen_single_ast! {
  simpleUserType:
  simpleIdentifier [{NL} typeArguments]
}

gen_single_ast! {
  shebangLine:
  ShebangLine (NL {NL})
}

gen_single_ast! {
  expression:
  disjunction
}

gen_ast! {
  disjunction:
    conjunction {{NL} "||" {NL} conjunction}

  conjunction:
    equality {{NL} "&&" {NL} equality}

  equality:
    comparison {equalityOperator {NL} comparison}

  comparison:
    genericCallLikeComparison {comparisonOperator {NL} genericCallLikeComparison}
}

gen_single_ast! {
  genericCallLikeComparison:
  infixOperation {callSuffix}
}

gen_single_ast! {
  infixOperation:
    elvisExpression {(inOperator {NL} elvisExpression) | (isOperator {NL} TYPE)}
}

gen_ast! {
  elvisExpression:
    infixFunctionCall {{NL} elvis {NL} infixFunctionCall}

  elvis:
    QUEST_NO_WS ":"
}

gen_ast! {
infixFunctionCall:
  rangeExpression {simpleIdentifier {NL} rangeExpression}

rangeExpression:
  additiveExpression {(".." | "..<") {NL} additiveExpression}

additiveExpression:
  multiplicativeExpression {additiveOperator {NL} multiplicativeExpression}

multiplicativeExpression:
  asExpression {multiplicativeOperator {NL} asExpression}

asExpression:
  prefixUnaryExpression {{NL} asOperator {NL} TYPE}

prefixUnaryExpression:
  {unaryPrefix} postfixUnaryExpression

postfixUnaryExpression:
  primaryExpression {postfixUnarySuffix}
}

gen_single_ast! {
  primaryExpression:
    parenthesizedExpression
    | simpleIdentifier
    | literalConstant
    | stringLiteral
    | callableReference
    | functionLiteral
    | objectLiteral
    | collectionLiteral
    | thisExpression
    | superExpression
    | ifExpression
    | whenExpression
    | tryExpression
    | jumpExpression
}

gen_single_ast! {
  parenthesizedExpression:
  "("
  {NL}
  expression
  {NL}
  ")"
}

gen_single_ast! {
    unaryPrefix:
    annotation
    | label
    | (prefixUnaryOperator {NL})@UnaryPrefixPrefixUnaryOperator
}

gen_single_ast! {
  postfixUnarySuffix:
    postfixUnaryOperator
    | typeArguments
    | callSuffix
    | indexingSuffix
    | navigationSuffix
}

// gen_single_ast! {
//   kotlinFile:
//   [shebangLine]
//   {NL}
//   {fileAnnotation}
//   packageHeader
//   importList
//   {topLevelObject}
//   EOF
// }
