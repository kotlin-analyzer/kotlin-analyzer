use lady_deirdre::{
    lexis::TokenRef,
    syntax::{Node, NodeRef},
};

#[cfg(test)]
mod tests;

use crate::tokens::KotlinToken;

// TODO: Add parameter to complex type

#[derive(Node)]
#[token(KotlinToken)]
#[define(HIDDEN = $Whitespace | LineComment | DelimitedComment)]
#[define(HIDDEN_WITH_NL = $NL | HIDDEN)]
#[trivia(HIDDEN)]
#[recovery(
    $LCurl,
    $RCurl,
    [$LCurl..$RCurl],
    $LParen,
    $RParen,
    [$LParen..$RParen],
    $LSquare,
    $RSquare,
    [$LSquare..$RSquare],
    $LAngle,
    $RAngle,
    [$LAngle..$RAngle],
    $Var,
    $Val,
    $Fun,
    $If,
    $Else,
    $When,
    $For,
    $While,
    $Do,
    $Class,
    $Interface,
    $Object,
    $Enum,
    $Companion,
    $AtNoWs,
)]
#[define(ANNOTATION_TYPE = ($AtField | $AtProperty | $AtGet | $AtSet | $AtReceiver | $AtParam | $AtSetparam | $AtDelegate))]
#[define(SOFT_KEYWORDS_SANS_SUSPEND = (
    | $Abstract
    | $Annotation
    | $By
    | $Catch
    | $Companion
    | $Constructor
    | $Crossinline
    | $Data
    | $Dynamic
    | $Enum
    | $External
    | $Final
    | $Finally
    | $Get
    | $Import
    | $Infix
    | $Init
    | $Inline
    | $Inner
    | $Internal
    | $Lateinit
    | $Noinline
    | $Open
    | $Operator
    | $Out
    | $Override
    | $Private
    | $Protected
    | $Public
    | $Reified
    | $Sealed
    | $Tailrec
    | $Set
    | $Vararg
    | $Where
    | $Field
    | $Property
    | $Receiver
    | $Param
    | $Setparam
    | $Delegate
    | $File
    | $Expect
    | $Actual
    | $Const
    | $Value
))]
#[define(SOFT_KEYWORDS = ( SOFT_KEYWORDS_SANS_SUSPEND | $Suspend))]
pub enum KotlinNode {
    #[root]
    #[rule(value: KotlinFile)]
    Root {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        value: NodeRef,
    },

    #[rule($NL* shebang_line: ShebangLine? test_package:ImportList)]
    /// shebangLine? NL* fileAnnotation* packageHeader importList topLevelObject* EOF
    KotlinFile {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        shebang_line: NodeRef,
        // #[child]
        // file_annotations: Vec<NodeRef>,
        #[child]
        test_package: NodeRef,
    },

    // #[rule(shebang_line: ShebangLine? $NL* ident_test: SimpleIdentifier+ $NL*)]
    // /// shebangLine? NL* fileAnnotation* packageHeader importList (statement semi)* EOF
    // Script {
    //     #[node]
    //     node: NodeRef,
    //     #[parent]
    //     parent: NodeRef,
    //     #[child]
    //     shebang_line: NodeRef,
    //     #[child]
    //     ident_test: Vec<NodeRef>,
    // },
    /// Matches `ShebangLine NL+`
    #[trivia($Whitespace)]
    #[rule(start: $ShebangLineStart content: ^[$NL | $Whitespace]+ $NL)]
    // #[parser(parse_shebang_line(session))]
    #[denote(SHEBANG_LINE)]
    #[describe("shebangline", "#! ...")]
    ShebangLine {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        start: TokenRef,
        #[child]
        content: Vec<TokenRef>,
    },

    /// Matches `(AT_NO_WS | AT_PRE_WS) FILE NL* COLON NL* (LSQUARE unescapedAnnotation+ RSQUARE | unescapedAnnotation) NL*`
    // Becuase this comes always before $NL* in Kotlin file we don't need to handle AT_PRE_WS
    #[trivia($NL | HIDDEN)]
    #[denote(FILE_ANNOTATION)]
    #[rule($AtNoWs $File $Colon ($LSquare  UnescapedAnnotation+  $RSquare | UnescapedAnnotation ))]
    #[describe("file annotation", "file")]
    FileAnnotation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    #[rule(($Semicolon | $NL) $NL*)]
    #[secondary]
    #[describe("semi colon", ";")]
    Semi {
         #[node]
        node: NodeRef,
        #[parent]
         parent: NodeRef,
    },

    #[rule(($Semicolon | $NL)+)]
    #[denote(SEMIS)]
    #[secondary]
    #[describe("semi colon", ";")]
    Semis {
         #[node]
        node: NodeRef,
        #[parent]
         parent: NodeRef,
    },

    /// Matches `(PACKAGE identifier semi?)?`
    #[denote(PACKAGE_HEADER)]
    #[rule(package_token: $Package package_name: Identifier Semi?)]
    #[describe("package header", "package")]
    PackageHeader {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        package_token: TokenRef,
        #[child]
        package_name: NodeRef,
    },

    #[rule(imports: ImportHeader+)]
    #[denote(IMPORT_LIST)]
    ImportList {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
         #[child]
        imports: Vec<NodeRef>,
    },

    #[denote(IMPORT_HEADER)]
    #[describe("import header", "import")]
    #[rule(import_token: $Import reference: ImportReference Semi?)]
    ImportHeader {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        import_token: TokenRef,
        #[child]
        reference: NodeRef,
    },

    // #[denote(TOP_LEVEL_OBJECT)]
    // #[rule(decl: Declaration Semis?)]
    TopLevelObject {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        decl: NodeRef,
    },

    #[denote(DECLARATION)]
    #[rule(
        TypeAlias
    )]
    Declaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    #[rule(
        modifiers: Modifiers?
        $TypeAlias $NL* new_type: SimpleType $NL*
        $Assignment $NL* target: Type
        
    )]
    #[denote(TYPE_ALIAS)]
    TypeAlias {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        #[child]
        new_type: NodeRef,
        #[child]
        target: NodeRef,
    },

    /// TODO(semantics): This can match false positives like `import foo.bar.* as baz` and `import foo.*.*`
    #[rule(base: SimpleIdentifier ($Dot (references: SimpleIdentifier | all: $Mult))* ($As as_target: SimpleIdentifier)?)]
    #[denote(IMPORT_REFERENCE)]
    ImportReference {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        base: NodeRef,
        #[child]
        references: Vec<NodeRef>,
        #[child]
        all: Vec<TokenRef>,
        #[child]
        as_target: NodeRef,
    },

    /// Matches `Identifier | SOFT_KEYWORDS`
    #[rule(identifier_token: $Identifier | soft_keyword: SOFT_KEYWORDS)]
    #[describe("simple identifier")]
    #[denote(SIMPLE_IDENTIFIER)]
    SimpleIdentifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier_token: TokenRef,
        #[child]
        soft_keyword: TokenRef,
    },

    /// Variant of SimpleIdentifier that does not allow suspend
    /// Used in Type definitions and other places where suspend is not allowed
    #[rule(identifier_token:$Identifier | soft_keyword: SOFT_KEYWORDS_SANS_SUSPEND)]
    #[describe("simple identifier")]
    SimpleTypeIdentifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier_token: TokenRef,
        #[child]
        soft_keyword: TokenRef,
    },

    /// Matches `simpleIdentifier (NL* DOT simpleIdentifier)*`
    #[rule(base: SimpleIdentifier ($NL* $Dot references: SimpleIdentifier)*)]
    #[denote(IDENTIFIER)]
    #[describe("identifier")]
    Identifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        base: NodeRef,
        #[child]
        references: Vec<NodeRef>,
    },

    // SECTION: types
    // Section Notes : Quest is always $QuestNoWs
    /// Matches typeModifiers? (functionType | parenthesizedType | nullableType | typeReference | definitelyNonNullableType)
    #[rule(
        modifiers: TypeModifier* 
        (complex: ComplexType | simple: SimpleType) 
        extra: AdditionalType?
    )]
    #[denote(TYPE)]
    Type {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: Vec<NodeRef>,
        #[child]
        complex: NodeRef,
        #[child]
        simple: NodeRef,
        #[child]
        extra: NodeRef,
    },

    #[rule(
        (nullable: $QuestNoWs | receiver_dot: $Dot | intersection: $Amp) 
        receiver_dot: $Dot? 
        type_ref: Type?
    )]
    AdditionalType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        nullable: TokenRef,
        #[child]
        intersection: TokenRef,
        #[child]
        // should always be one.
        receiver_dot: Vec<TokenRef>,
        #[child]
        type_ref: NodeRef,
    },

    #[denote(COMPLEX_TYPE)]
    #[rule(
        ($LParen bracketed:FunctionTypeParameter+{$Comma} $RParen) // Comma-separated list: (foo.x, bar -> baz, fuz)
        ($Arrow return_type:Type)? // Optional "-> res" suffix for functions
    )]
    ComplexType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        bracketed: Vec<NodeRef>,
        #[child]
        return_type: NodeRef,
    },

    #[rule(base: SimpleTypeIdentifier generic_args:(TypeArguments)?)]
    SimpleType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        base: NodeRef,
        #[child]
        generic_args: NodeRef,
    },

    /// Make sure that this is only used inside function type parameters
    #[rule(
        type_or_param: Type
        ($Colon parameter_type: Type)?
    )]
    #[denote(TYPE_IDENTIFIER_OR_PARAMETER)]
    FunctionTypeParameter {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        type_or_param: NodeRef,
        #[child]
        parameter_type: NodeRef,
    },


    /// Matches `typeProjectionModifiers? type | MULT`
    /// Variance modifiers are `IN` or `OUT` but `OUT` is already part of SimpleTypeIdentifier
    /// Becuase of the LL(1) nature of the parser, we have to use `Type` instead of `$Out`
    /// If following_type is not Nil, then inner_type_or_out must be `out` TypeProjection
    #[rule((in_projection: $In | inner_type_or_out: Type) following_type: Type? | star_projection: $Mult)]
    #[denote(TEMP7)]
    TypeProjection {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        in_projection: TokenRef,
        #[child]
        star_projection: TokenRef,
        #[child]
        inner_type_or_out: NodeRef,
        #[child]
        following_type: NodeRef,
    },

    // SECTION: classes

    // classDeclaration
    //     : modifiers? (CLASS | (FUN NL*)? INTERFACE) NL* simpleIdentifier
    //       (NL* typeParameters)? (NL* primaryConstructor)?
    //       (NL* COLON NL* delegationSpecifiers)?
    //       (NL* typeConstraints)?
    //       (NL* classBody | NL* enumClassBody)?
    //     ;

    // primaryConstructor
    //     : (modifiers? CONSTRUCTOR NL*)? classParameters
    //     ;

    // classBody
    //     : LCURL NL* classMemberDeclarations NL* RCURL
    //     ;

    // classParameters
    //     : LPAREN NL* (classParameter (NL* COMMA NL* classParameter)* (NL* COMMA)?)? NL* RPAREN
    //     ;

    // classParameter
    //     : modifiers? (VAL | VAR)? NL* simpleIdentifier COLON NL* type (NL* ASSIGNMENT NL* expression)?
    //     ;

    // delegationSpecifiers
    //     : annotatedDelegationSpecifier (NL* COMMA NL* annotatedDelegationSpecifier)*
    //     ;

    // delegationSpecifier
    //     : constructorInvocation
    //     | explicitDelegation
    //     | userType
    //     | functionType
    //     | SUSPEND NL* functionType
    //     ;

    // constructorInvocation
    //     : userType NL* valueArguments
    //     ;

ConstructorInvocation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        user_type: NodeRef,
        #[child]
        value_arguments: NodeRef,
    },

// annotatedDelegationSpecifier
//     : annotation* NL* delegationSpecifier
//     ;

// explicitDelegation
//     : (userType | functionType) NL* BY NL* expression
//     ;

// typeParameters
//     : LANGLE NL* typeParameter (NL* COMMA NL* typeParameter)* (NL* COMMA)? NL* RANGLE
//     ;

// typeParameter
//     : typeParameterModifiers? NL* simpleIdentifier (NL* COLON NL* type)?
//     ;

// typeConstraints
//     : WHERE NL* typeConstraint (NL* COMMA NL* typeConstraint)*
//     ;

// typeConstraint
//     : annotation* simpleIdentifier NL* COLON NL* type
//     ;
    

    // SECTION: classMembers

    // classMemberDeclarations
    //     : (classMemberDeclaration semis?)*
    //     ;

    // classMemberDeclaration
    //     : declaration
    //     | companionObject
    //     | anonymousInitializer
    //     | secondaryConstructor
    //     ;

    // anonymousInitializer
    //     : INIT NL* block
    //     ;

    // companionObject
    //     : modifiers? COMPANION NL* DATA? NL* OBJECT
    //       (NL* simpleIdentifier)?
    //       (NL* COLON NL* delegationSpecifiers)?
    //       (NL* classBody)?
    //     ;

    // functionValueParameters
    //     : LPAREN NL* (functionValueParameter (NL* COMMA NL* functionValueParameter)* (NL* COMMA)?)? NL* RPAREN
    //     ;

    // functionValueParameter
    //     : parameterModifiers? parameter (NL* ASSIGNMENT NL* expression)?
    //     ;

    // functionDeclaration
    //     : modifiers?
    //       FUN (NL* typeParameters)? (NL* receiverType NL* DOT)? NL* simpleIdentifier
    //       NL* functionValueParameters
    //       (NL* COLON NL* type)?
    //       (NL* typeConstraints)?
    //       (NL* functionBody)?
    //     ;

    /// Matches `block | ASSIGNMENT NL* expression`
    #[denote(FUNCTION_BODY)]
    #[rule(block: Block | ($Assignment $NL* expression: Expression))]
    FunctionBody {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        block: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `annotation* NL* simpleIdentifier (NL* COLON NL* type)?`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(annotation: Annotation* identifier: SimpleIdentifier ($Colon type_ref: Type)?)]
    #[denote(VARIABLE_DECLARATION)]
    VariableDeclaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotation: Vec<NodeRef>,
        #[child]
        identifier: NodeRef,
        #[child]
        type_ref: NodeRef,
    },

    /// Matches `LPAREN NL* variableDeclaration (NL* COMMA NL* variableDeclaration)* (NL* COMMA)? NL* RPAREN`
    #[rule(
        $LParen
        variable_declarations: VariableDeclaration+{$Comma}
        $RParen
    )]
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(MULTI_VARIABLE_DECLARATION)]
    MultiVariableDeclaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        variable_declarations: Vec<NodeRef>,
    },

    // propertyDeclaration
    //     : modifiers? (VAL | VAR)
    //       (NL* typeParameters)?
    //       (NL* receiverType NL* DOT)?
    //       (NL* (multiVariableDeclaration | variableDeclaration))
    //       (NL* typeConstraints)?
    //       (NL* (ASSIGNMENT NL* expression | propertyDelegate))?
    //       (NL* SEMICOLON)? NL* (getter? (NL* semi? setter)? | setter? (NL* semi? getter)?)
    //     ;

    // propertyDelegate
    //     : BY NL* expression
    //     ;

    // getter
    //     : modifiers? GET
    //       (NL* LPAREN NL* RPAREN (NL* COLON NL* type)? NL* functionBody)?
    //     ;

    // setter
    //     : modifiers? SET
    //       (NL* LPAREN NL* functionValueParameterWithOptionalType (NL* COMMA)? NL* RPAREN (NL* COLON NL* type)? NL* functionBody)?
    //     ;
    #[denote(SETTER)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        modifiers: Modifiers? $Set 
        (fn_type: FunctionValueParameterWithOptionalType
            ($Comma)? $RParen
            ($Colon type_ref: Type)?  function_body: FunctionBody
        )
    )]
    Setter {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        #[child]
        fn_type: NodeRef,
        #[child]
        type_ref: NodeRef,
        #[child]
        function_body: NodeRef,
    },

    /// Matches `LPAREN NL* (functionValueParameterWithOptionalType (NL* COMMA NL* functionValueParameterWithOptionalType)* (NL* COMMA)?)? NL* RPAREN`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $LParen
        fn_value_parameter_opt_type: FunctionValueParameterWithOptionalType+{$Comma}
        $RParen
    )]
    #[denote(PARAMETERS_WITH_OPTIONAL_TYPE)]
    ParametersWithOptionalType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        fn_value_parameter_opt_type: Vec<NodeRef>,
    },

    /// Matches `parameterModifiers? parameterWithOptionalType (NL* ASSIGNMENT NL* expression)?`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        parameter_with_opt_type: ParameterWithOptionalType+
        ($Assignment expression: Expression)?
    )]
    #[denote(FUNCTION_VALUE_PARAMETER_WITH_OPTIONAL_TYPE)]
    FunctionValueParameterWithOptionalType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        /// Todo: This captures parameter modifiers.
        parameter_with_opt_type: Vec<NodeRef>,
        #[child]
        expression: NodeRef,
    },

    /// Matches `simpleIdentifier NL* COLON NL* type`
    #[rule(identifier: SimpleIdentifier ($Colon type_ref: Type)?)]
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(PARAMETER_WITH_OPTIONAL_TYPE)]
    ParameterWithOptionalType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        type_ref: NodeRef,
    },

    /// Matches `simpleIdentifier NL* COLON NL* type`
    #[rule(identifier: SimpleIdentifier $Colon type_ref: Type)]
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(PARAMETER)]
    Parameter {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        type_ref: NodeRef,
    },

    // objectDeclaration
    //     : modifiers? OBJECT
    //       NL* simpleIdentifier
    //       (NL* COLON NL* delegationSpecifiers)?
    //       (NL* classBody)?
    //     ;

    // #[rule(modifiers: Modifiers? $Object $NL* identifier: SimpleIdentifier (delegation_specifiers: DelegationSpecifiers)? (class_body: ClassBody)?)]
    // #[denote(OBJECT_DECLARATION)]
    ObjectDeclaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        delegation_specifiers: NodeRef,
        #[child]
        class_body: NodeRef,
    },

    // secondaryConstructor
    //     : modifiers? CONSTRUCTOR NL* functionValueParameters (NL* COLON NL* constructorDelegationCall)? NL* block?
    //     ;

    // constructorDelegationCall
    //     : (THIS | SUPER) NL* valueArguments
    //     ;

    // SECTION: statements

    /// Matches `(statement (semis statement)*)? semis?`
    #[rule(statements: Statement (Semis statements: Statement)* Semis?)]
    #[denote(STATEMENTS)]
    #[describe("statements", "stmts")]
    Statements {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        statements: Vec<NodeRef>,
    },

    // statement
    //     : (label | annotation)* ( declaration | assignment | loopStatement | expression)
    //     ;
    /// Todo: complete the rule
    #[rule(
        (label: Label | annotation: Annotation)* expression: Expression
    )]
    Statement {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        label: Vec<NodeRef>,
        #[child]
        annotation: Vec<NodeRef>,
        // #[child]
        // declaration: NodeRef,
        // #[child]
        // assignment: NodeRef,
        // #[child]
        // loop_statement: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `simpleIdentifier (AT_NO_WS | AT_POST_WS) NL*`
    #[denote(LABEL)]
    #[rule(identifier: SimpleIdentifier $AtNoWs $NL*)]
    Label {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
    },

    // controlStructureBody
    //     : block
    //     | statement
    //     ;
    #[rule(block: Block | statement: Statement)]
    #[denote(CONTROL_STRUCTURE_BODY)]
    ControlStructureBody {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        block: NodeRef,
        #[child]
        statement: NodeRef,
    },

    /// Matches `LCURL NL* statements NL* RCURL`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule($LCurl statements: Statements $RCurl)]
    Block {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        statements: NodeRef,
    },

    // loopStatement
    //     : forStatement
    //     | whileStatement
    //     | doWhileStatement
    //     ;

    // forStatement
    //     : FOR NL* LPAREN annotation* (variableDeclaration | multiVariableDeclaration)
    //       IN expression RPAREN NL* controlStructureBody?
    //     ;

    // whileStatement
    //     : WHILE NL* LPAREN expression RPAREN NL* (controlStructureBody | SEMICOLON)
    //     ;

    // doWhileStatement
    //     : DO NL* controlStructureBody? NL* WHILE NL* LPAREN expression RPAREN
    //     ;

    // assignment
    //     : (directlyAssignableExpression ASSIGNMENT | assignableExpression assignmentAndOperator) NL* expression
    //     ;

    // SECTION: expressions

// expression
//     : disjunction
//     ;
    #[denote(EXPRESSION)]
    #[rule(disjunction: $Range)] // TODO: Change to disjunction
    #[describe("expression", "expr")]
    Expression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        disjunction: TokenRef,
    },

    // disjunction
    //     : conjunction (NL* DISJ NL* conjunction)*
    //     ;

    // conjunction
    //     : equality (NL* CONJ NL* equality)*
    //     ;

    // equality
    //     : comparison (equalityOperator NL* comparison)*
    //     ;

    // comparison
    //     : genericCallLikeComparison (comparisonOperator NL* genericCallLikeComparison)*
    //     ;

    // genericCallLikeComparison
    //     : infixOperation callSuffix*
    //     ;

    // infixOperation
    //     : elvisExpression (inOperator NL* elvisExpression | isOperator NL* type)*
    //     ;

    // elvisExpression
    //     : infixFunctionCall (NL* elvis NL* infixFunctionCall)*
    //     ;

    // elvis
    //     : QUEST_NO_WS COLON
    //     ;

    // infixFunctionCall
    //     : rangeExpression (simpleIdentifier NL* rangeExpression)*
    //     ;

    // rangeExpression
    //     : additiveExpression ((RANGE | RANGE_UNTIL) NL* additiveExpression)*
    //     ;

    // additiveExpression
    //     : multiplicativeExpression (additiveOperator NL* multiplicativeExpression)*
    //     ;

    // multiplicativeExpression
    //     : asExpression (multiplicativeOperator NL* asExpression)*
    //     ;

    // asExpression
    //     : prefixUnaryExpression (NL* asOperator NL* type)*
    //     ;

    // prefixUnaryExpression
    //     : unaryPrefix* postfixUnaryExpression
    //     ;

    // unaryPrefix
    //     : annotation
    //     | label
    //     | prefixUnaryOperator NL*
    //     ;

    // postfixUnaryExpression
    //     : primaryExpression postfixUnarySuffix*
    //     ;

    // postfixUnarySuffix
    //     : postfixUnaryOperator
    //     | typeArguments
    //     | callSuffix
    //     | indexingSuffix
    //     | navigationSuffix
    //     ;

    // directlyAssignableExpression
    //     : postfixUnaryExpression assignableSuffix
    //     | simpleIdentifier
    //     | parenthesizedDirectlyAssignableExpression
    //     ;

    // parenthesizedDirectlyAssignableExpression
    //     : LPAREN NL* directlyAssignableExpression NL* RPAREN
    //     ;

    // assignableExpression
    //     : prefixUnaryExpression
    //     | parenthesizedAssignableExpression
    //     ;

    // parenthesizedAssignableExpression
    //     : LPAREN NL* assignableExpression NL* RPAREN
    //     ;

    // assignableSuffix
    //     : typeArguments
    //     | indexingSuffix
    //     | navigationSuffix
    //     ;

    // indexingSuffix
    //     : LSQUARE NL* expression (NL* COMMA NL* expression)* (NL* COMMA)? NL* RSQUARE
    //     ;

    // navigationSuffix
    //     : memberAccessOperator NL* (simpleIdentifier | parenthesizedExpression | CLASS)
    //     ;

    // callSuffix
    //     : typeArguments? (valueArguments? annotatedLambda | valueArguments)
    //     ;

    // annotatedLambda
    //     : annotation* label? NL* lambdaLiteral
    //     ;

    AnnotatedLambda {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotation: Vec<NodeRef>,
        #[child]
        label: NodeRef,
        #[child]
        lambda_literal: NodeRef,
    },

    /// Matches `LANGLE NL* typeProjection (NL* COMMA NL* typeProjection)* (NL* COMMA)? NL* RANGLE`
    #[trivia($NL | HIDDEN)]
    #[rule($LAngle args: TypeProjection+{$Comma} $RAngle)]
    #[denote(TYPE_ARGUMENTS)]
    TypeArguments {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        args: Vec<NodeRef>,
    },

    /// Matches LPAREN NL* (valueArgument (NL* COMMA NL* valueArgument)* (NL* COMMA)? NL*)? RPAREN
    #[denote(VALUE_ARGUMENTS)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $LParen
        ((value_argument: ValueArgument)+{$Comma})?
        $RParen
    )]
    #[describe("value arguments", "args")]
    ValueArguments {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        value_argument: Vec<NodeRef>,
    },

    /// Matches `annotation? NL* (simpleIdentifier NL* ASSIGNMENT NL*)? MULT? NL* expression`
    #[denote(VALUE_ARGUMENT)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        annotation: Annotation?  (identifier: SimpleIdentifier?  $Assignment )? mult: $Mult?  expression: Expression
    )]
    #[describe("value argument", "value")]
    ValueArgument {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotation: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        mult: TokenRef,
        #[child]
        expression: NodeRef,
    },

    // primaryExpression
    //     : parenthesizedExpression
    //     | simpleIdentifier
    //     | literalConstant
    //     | stringLiteral
    //     | callableReference
    //     | functionLiteral
    //     | objectLiteral
    //     | collectionLiteral
    //     | thisExpression
    //     | superExpression
    //     | ifExpression
    //     | whenExpression
    //     | tryExpression
    //     | jumpExpression
    //     ;

    // parenthesizedExpression
    //     : LPAREN NL* expression NL* RPAREN
    //     ;

    // collectionLiteral
    //     : LSQUARE NL* (expression (NL* COMMA NL* expression)* (NL* COMMA)? NL*)? RSQUARE
    //     ;

    // literalConstant
    //     : BooleanLiteral
    //     | IntegerLiteral
    //     | HexLiteral
    //     | BinLiteral
    //     | CharacterLiteral
    //     | RealLiteral
    //     | NullLiteral
    //     | LongLiteral
    //     | UnsignedLiteral
    //     ;

    // stringLiteral
    //     : lineStringLiteral
    //     | multiLineStringLiteral
    //     ;

    // lineStringLiteral
    //     : QUOTE_OPEN (lineStringContent | lineStringExpression)* QUOTE_CLOSE
    //     ;

    // multiLineStringLiteral
    //     : TRIPLE_QUOTE_OPEN (multiLineStringContent | multiLineStringExpression | MultiLineStringQuote)* TRIPLE_QUOTE_CLOSE
    //     ;

    // lineStringContent
    //     : LineStrText
    //     | LineStrEscapedChar
    //     | LineStrRef
    //     ;

    // lineStringExpression
    //     : LineStrExprStart NL* expression NL* RCURL
    //     ;

    // multiLineStringContent
    //     : MultiLineStrText
    //     | MultiLineStringQuote
    //     | MultiLineStrRef
    //     ;

    // multiLineStringExpression
    //     : MultiLineStrExprStart NL* expression NL* RCURL
    //     ;

    // lambdaLiteral
    //     : LCURL NL* (lambdaParameters? NL* ARROW NL*)? statements NL* RCURL
    //     ;

    // lambdaParameters
    //     : lambdaParameter (NL* COMMA NL* lambdaParameter)* (NL* COMMA)?
    //     ;

    // lambdaParameter
    //     : variableDeclaration
    //     | multiVariableDeclaration (NL* COLON NL* type)?
    //     ;

    // anonymousFunction
    //     : SUSPEND?
    //       NL*
    //       FUN
    //       (NL* type NL* DOT)?
    //       NL* parametersWithOptionalType
    //       (NL* COLON NL* type)?
    //       (NL* typeConstraints)?
    //       (NL* functionBody)?
    //     ;

    // functionLiteral
    //     : lambdaLiteral
    //     | anonymousFunction
    //     ;

    // objectLiteral
    //     : DATA? NL* OBJECT (NL* COLON NL* delegationSpecifiers NL*)? (NL* classBody)?
    //     ;

    // thisExpression
    //     : THIS
    //     | THIS_AT
    //     ;

    // superExpression
    //     : SUPER (LANGLE NL* type NL* RANGLE)? (AT_NO_WS simpleIdentifier)?
    //     | SUPER_AT
    //     ;

    // ifExpression
    //     : IF NL* LPAREN NL* expression NL* RPAREN NL*
    //       ( controlStructureBody
    //       | controlStructureBody? NL* SEMICOLON? NL* ELSE NL* (controlStructureBody | SEMICOLON)
    //       | SEMICOLON)
    //     ;

    /// Matches `LPAREN (annotation* NL* VAL NL* variableDeclaration NL* ASSIGNMENT NL*)? expression RPAREN`
    #[rule(
        $LParen
        (annotations: Annotation* $Val variable_declaration: VariableDeclaration $Assignment)?
        expression: Expression
        $RParen
    )]
    #[denote(WHEN_SUBJECT)]
    WhenSubject {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotations: Vec<NodeRef>,
        #[child]
        variable_declaration: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `WHEN NL* whenSubject? NL* LCURL NL* (whenEntry NL*)* NL* RCURL`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $When when_subject: WhenSubject? $LCurl 
            when_entries: WhenEntry* 
        $RCurl
    )]
    #[denote(WHEN_EXPRESSION)]
    WhenExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        when_subject: NodeRef,
        #[child]
        when_entries: Vec<NodeRef>,
    },

    /// Matches `whenCondition (NL* COMMA NL* whenCondition)* (NL* COMMA)? NL* ARROW NL* controlStructureBody semi?`
    /// or `ELSE NL* ARROW NL* controlStructureBody semi?`
    #[rule(
        (conditions: WhenCondition+{$Comma} $Arrow body: ControlStructureBody Semi?)
        | ($Else $Arrow body: ControlStructureBody Semi?)
    )]
    #[denote(WHEN_ENTRY)]
    WhenEntry {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        conditions: Vec<NodeRef>,
        #[child]
        body: NodeRef,
    },

    /// Matches expression | rangeTest | typeTest
    #[rule(
        expression: Expression
        | range_test: RangeTest
        | type_test: TypeTest
    )]
    #[denote(WHEN_CONDITION)]
    WhenCondition {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expression: NodeRef,
        #[child]
        range_test: NodeRef,
        #[child]
        type_test: NodeRef,
    }, 

    /// Matches inOperator NL* expression
    #[rule(in_operator: InOperator $NL* expression: Expression)]
    #[denote(RANGE_TEST)]
    RangeTest {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        in_operator: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches isOperator NL* type
    #[rule(is_operator: IsOperator $NL* type_ref: Type)]
    #[denote(TYPE_TEST)]
    TypeTest {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        is_operator: NodeRef,
        #[child]
        type_ref: NodeRef,
    },

    /// Matches TRY NL* block ((NL* catchBlock)+ (NL* finallyBlock)? | NL* finallyBlock)
    #[denote(TRY_EXPRESSION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $Try block: Block 
        (
            (catch_blocks: CatchBlock+ (finally_block: FinallyBlock)?)
            | (finally_block: FinallyBlock)
        )
    )]
    TryExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        block: NodeRef,
        #[child]
        catch_blocks: Vec<NodeRef>,
        #[child]
        finally_block: NodeRef,
    },

    /// Matches `CATCH NL* LPAREN annotation* simpleIdentifier COLON type (NL* COMMA)? RPAREN NL* block`
    #[denote(CATCH_BLOCK)]
    #[rule(
        $Catch $NL* 
        $LParen 
            annotations: Annotation*
            identifier: SimpleIdentifier $Colon type_ref: Type ($NL* $Comma)?
        $RParen $NL*
        block: Block
    )]
    CatchBlock {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotations: Vec<NodeRef>,
        #[child]
        identifier: NodeRef,
        #[child]
        type_ref: NodeRef,
        #[child]
        block: NodeRef,
    },

    /// Matches `FINALLY NL* block`
    #[denote(FINALLY_BLOCK)]
    #[rule($Finally $NL* block: Block)]
    FinallyBlock {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        block: NodeRef,
    },

    /// Matches `THROW NL* expression | (RETURN | RETURN_AT) expression? | CONTINUE | CONTINUE_AT | BREAK | BREAK_AT`
    #[denote(JUMP_EXPRESSION)]
    #[rule(
        throw_token: $Throw $NL* expression: Expression
        | (return_token: ($Return | $ReturnAt) expression: Expression?)
        | continue_token: $Continue
        | continue_at_token: $ContinueAt
        | break_token: $Break
        | break_at_token: $BreakAt
    )]
    JumpExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        throw_token: TokenRef,
        #[child]
        expression: NodeRef,
        #[child]
        return_token: TokenRef,
        #[child]
        continue_token: TokenRef,
        #[child]
        continue_at_token: TokenRef,
        #[child]
        break_token: TokenRef,
        #[child]
        break_at_token: TokenRef,
    },

    // callableReference
    //     : receiverType? COLONCOLON NL* (simpleIdentifier | CLASS)
    //     ;

    /// Matches assignment and operators: ADD_ASSIGNMENT, SUB_ASSIGNMENT, MULT_ASSIGNMENT, DIV_ASSIGNMENT, MOD_ASSIGNMENT
    #[rule($AddAssignment | $SubAssignment | $MultAssignment | $DivAssignment | $ModAssignment)]
    #[denote(ASSIGNMENT_AND_OPERATOR)]
    AssignmentAndOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches equality operators: EXCL_EQ, EXCL_EQEQ, EQEQ, EQEQEQ
    #[rule($ExclEq | $ExclEqEq | $EqEq | $EqEqEq)]
    #[denote(EQUALITY_OPERATOR)]
    EqualityOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches comparison operators: LANGLE, RANGLE, LE, GE
    #[rule($LAngle | $RAngle | $Le | $Ge)]
    #[denote(COMPARISON_OPERATOR)]
    ComparisonOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches IN | NOT_IN
    #[rule($In | NotIn)]
    #[denote(IN_OPERATOR)]
    InOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches IS | NOT_IS
    #[rule($Is | NotIs)]
    #[denote(IS_OPERATOR)]
    IsOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches ADD | SUB
    #[rule($Add | $Sub)]
    #[denote(ADDITIVE_OPERATOR)]
    AdditiveOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },


    /// Matches MULT | DIV | MOD
    #[rule($Mult | $Div | $Mod)]
    #[denote(MULTIPLICATIVE_OPERATOR)]
    MultiplicativeOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches AS | AS_SAFE
    #[rule($As | $AsSafe)]
    #[denote(AS_OPERATOR)]
    AsOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `INCR | DECR | SUB | ADD | EXCL_NO_WS excl``
    #[rule($Incr | $Decr | $Sub | $Add | $ExclNoWs Excl)]
    #[denote(PREFIX_UNARY_OPERATOR)]
    PrefixUnaryOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches INCR | DECR | EXCL_NO_WS excl
    #[rule($Incr | $Decr | $ExclNoWs Excl)]
    #[denote(POSTFIX_UNARY_OPERATOR)]
    PostfixUnaryOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },


    /// Matches EXCL_NO_WS or EXCL_WS
    // Todo: consider removing
    #[rule($ExclNoWs)]
    #[denote(EXCL)]
    #[secondary]
    #[describe("excl", "!")]
    Excl {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `DOT | SAFE_NAV | COLONCOLON`
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(MEMBER_ACCESS_OPERATOR)]
    #[rule($Dot | SafeNav |  $ColonColon)]
    MemberAccessOperator {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `QUEST_NO_WS DOT`
    #[rule($QuestNoWs $Dot)]
    #[denote(SAFE_NAV)]
    SafeNav {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    // SECTION: modifiers
    /// Matches `(annotation | modifier)+`
    #[rule((Annotation | Modifier)+)]
    #[denote(T21)]
    Modifiers {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `(annotation | parameterModifier)+`
    #[rule((Annotation | ParameterModifier)+)]
    #[denote(T28)]
    ParameterModifiers {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `(classModifier| memberModifier| visibilityModifier| functionModifier| propertyModifier| inheritanceModifier| parameterModifier| platformModifier) NL*`
    #[rule((ClassModifier| MemberModifier| VisibilityModifier| FunctionModifier| PropertyModifier| InheritanceModifier| ParameterModifier| PlatformModifier) $NL*)]
    #[denote(T29)]
    Modifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `annotation | SUSPEND NL*`
    #[rule(annotation: Annotation | suspend: $Suspend $NL*)]
    #[denote(TYPE_MODIFIER)]
    TypeModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotation: NodeRef,
        #[child]
        suspend: TokenRef,
    },

    /// Alternative to TypeModifier that does not conflict with SUSPEND
    #[rule(Annotation)]
    #[denote(T31)]
    NonSuspendTypeModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches ENUM | SEALED | ANNOTATION | DATA | INNER | VALUE
    #[rule($Enum | $Sealed | $Annotation | $Data | $Inner | $Value)]
    ClassModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches OVERRIDE | LATEINIT
    #[rule($Override | $Lateinit)]
    MemberModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches PUBLIC | PRIVATE | INTERNAL | PROTECTED
    #[rule($Public | $Private | $Internal | $Protected)]
    VisibilityModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches IN | OUT
    #[rule($In | $Out)]
    VarianceModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    // typeParameterModifiers
    //     : typeParameterModifier+
    //     ;
    #[rule(ReificationModifier | (VarianceModifier $NL*) | Annotation)]
    #[denote(T20)]
    TypeParameterModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches TAILREC | OPERATOR | INFIX | INLINE | EXTERNAL | SUSPEND
    #[rule($Tailrec | $Operator | $Infix | $Inline | $External | $Suspend)]
    FunctionModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches CONST
    #[rule($Const)]
    PropertyModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches ABSTRACT | FINAL | OPEN
    #[rule($Abstract | $Final | $Open)]
    InheritanceModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches VARARG | NOINLINE | CROSSINLINE
    #[rule($Vararg | $Noinline | $Crossinline)]
    ParameterModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches REIFIED
    #[rule($Reified)]
    ReificationModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches EXPECT | ACTUAL
    #[rule($Expect | $Actual)]
    PlatformModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /* SECTION: annotations */
    /// Matches either singleAnnotation or multiAnnotation as described in
    /// https://github.com/Kotlin/kotlin-spec/blob/release/grammar/src/main/antlr/KotlinParser.g4#L852
    /// Note that there is no reason to capture them individually and also it would not be supported by lady-deirdre
    /// Another decision we made was to change NL? at the start to NL* as that caused some conflicts in indirect useage in [Self::TypeArguments]
    #[trivia($NL)]
    #[rule((annotation_type: ANNOTATION_TYPE $Colon  | at: $AtNoWs) (($LSquare multi: UnescapedAnnotation+ $RSquare) | single: UnescapedAnnotation))  ]
    #[denote(TEMP6)]
    Annotation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotation_type: TokenRef,
        #[child]
        at: TokenRef,
        #[child]
        multi: Vec<NodeRef>,
        #[child]
        single: NodeRef,
    },

    /// TODO: Matches constructorInvocation| userType
    #[rule(type_ref: SimpleType)]
    #[denote(UNESCAPED_ANNOTATION)]
    #[describe("Unescaped Annotation")]
    UnescapedAnnotation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        type_ref: NodeRef,
    },

    /* Leaf Nodes - Mostly complex tokens that could not be handled by the lexer */
    // Section: separators and operations with Hidden
    /// Matches `"@" (Hidden | NL)`
    /// Since `Hidden` is trivia, we only add newline explicitly
    #[rule($AtNoWs $NL?)]
    #[denote(AT_POST_WS)]
    #[secondary]
    #[describe("AT_POST_WS", "@")]
    AtPostWs {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `(Hidden | NL) "@"`
    #[rule($NL? $AtNoWs)]
    #[denote(AT_PRE_WS)]
    #[secondary]
    #[describe("AT_PRE_WS", "@")]
    AtPreWs {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `"!is" (Hidden | NL)`
    #[rule($NotIs $NL?)]
    #[denote(NOT_IS)]
    #[secondary]
    #[describe("NOT_IS", "!is")]
    NotIs {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `"!in" (Hidden | NL)`
    #[rule($NotIn $NL?)]
    #[denote(NOT_IN)]
    #[secondary]
    #[describe("NOT_IN", "!in")]
    NotIn {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    // Section: Comments
    /// Matches `"//" (^['\r', '\n'])*`

    #[rule($LineCommentStart ^[$NL]* $NL)]
    #[denote(LINE_COMMENT)]
    #[trivia]
    #[describe("comment", "'//...'")]
    #[secondary]
    LineComment {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `_*'/*' ( DelimitedComment | . )*? '*/'`
    #[rule($DelimitedCommentStart (^[$DelimitedCommentEnd | $DelimitedCommentStart] | DelimitedComment)* $DelimitedCommentEnd)]
    #[denote(DELIMITED_COMMENT)]
    #[trivia]
    // #[parser(parse_delimited_comment(session))]
    #[describe("delimited comment")]
    #[secondary]
    DelimitedComment {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },
}
