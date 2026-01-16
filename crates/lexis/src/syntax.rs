#![allow(mismatched_lifetime_syntaxes)]

use lady_deirdre::{
    lexis::TokenRef,
    syntax::{Node, NodeRef},
};

#[cfg(test)]
mod tests;

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
#[define(SOFT_KEYWORDS_SANS_SUSPEND_WHERE = (
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
#[define(SOFT_KEYWORDS_SANS_SUSPEND = ( SOFT_KEYWORDS_SANS_SUSPEND_WHERE | $Where))]
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
    
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(shebang_line: ShebangLine? file_annotations: FileAnnotation* package_header:PackageHeader? import_list: ImportList? top_level_object: TopLevelObject+)]
    /// shebangLine? NL* fileAnnotation* packageHeader importList topLevelObject* EOF
    KotlinFile {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        shebang_line: NodeRef,
        #[child]
        file_annotations: Vec<NodeRef>,
        #[child]
        package_header: NodeRef,
        #[child]
        import_list: NodeRef,
        #[child]
        top_level_object: Vec<NodeRef>,
    },

    #[trivia(HIDDEN_WITH_NL)]
     #[rule(shebang_line: ShebangLine? file_annotations: FileAnnotation* package_header:PackageHeader? import_list: ImportList? (statements: Statement Semi)*)]
    /// shebangLine? NL* fileAnnotation* packageHeader importList (statement semi)* EOF
    Script {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        shebang_line: NodeRef,
        #[child]
        file_annotations: Vec<NodeRef>,
        #[child]
        package_header: NodeRef,
        #[child]
        import_list: NodeRef,
        #[child]
        statements: Vec<NodeRef>,
    },


    /// Matches `ShebangLine NL+`
    #[trivia($Whitespace)]
    #[rule(start: $ShebangLineStart content: ^[$NL | $Whitespace]+ $NL)]
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
    // Because this comes always before $NL* in Kotlin file we don't need to handle AT_PRE_WS
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

    #[rule(decl: Declaration Semis?)]
    TopLevelObject {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        decl: NodeRef,
    },

    /// Matches `classDeclaration | functionDeclaration | propertyDeclaration | typeAlias`
    #[denote(DECLARATION)]
    #[rule(class_declaration: ClassDeclaration | function_declaration: FunctionDeclaration | property_declaration: PropertyDeclaration | type_alias: TypeAlias)]
    Declaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        class_declaration: NodeRef,
        #[child]
        function_declaration: NodeRef,
        #[child]
        property_declaration: NodeRef,
        #[child]
        type_alias: NodeRef,
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
    #[rule(identifier_token: $Identifier | soft_keyword: SOFT_KEYWORDS_SANS_SUSPEND_WHERE)]
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

    /// Make sure to not use this in Type directly or indirectly
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(first: SimpleType ($Dot others: SimpleType)*)]
    #[denote(USER_TYPE)]
    UserType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        first: NodeRef,
        #[child]
        others: Vec<NodeRef>,
    },

    /// Matches `LPAREN NL* type NL* RPAREN`
    #[denote(PARENTHESIZED_TYPE)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule($LParen type_ref: Type $RParen)]
    ParenthesizedType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        type_ref: NodeRef,
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

    ///  Matches `modifiers? (CLASS | (FUN NL*)? INTERFACE) NL* simpleIdentifier
    ///       (NL* typeParameters)? (NL* primaryConstructor)?
    ///       (NL* COLON NL* delegationSpecifiers)?
    ///       (NL* typeConstraints)?
    ///       (NL* classBody | NL* enumClassBody)?`
    #[denote(CLASS_DECLARATION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        modifiers: Modifiers?
        ($Class | ($Fun? $Interface)) identifier: SimpleIdentifier
        (type_parameters: TypeParameters)?
        (primary_constructor: PrimaryConstructor)?
        ($Colon delegation_specifiers: DelegationSpecifiers)?
        (type_constraints: TypeConstraints)?
        (class_body: ClassBody | enum_class_body: EnumClassBody)?
    )]
    ClassDeclaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        type_parameters: NodeRef,
        #[child]
        primary_constructor: NodeRef,
        #[child]
        delegation_specifiers: NodeRef,
        #[child]
        type_constraints: NodeRef,
        #[child]
        class_body: NodeRef,
        #[child]
        enum_class_body: NodeRef,
    },

    /// Matches `(modifiers? CONSTRUCTOR NL*)? classParameters`
    #[denote(PRIMARY_CONSTRUCTOR)]
    #[rule((modifiers: Modifiers? $Constructor $NL*)? class_parameters: ClassParameters)]
    PrimaryConstructor {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        #[child]
        class_parameters: NodeRef,
    },

    // classBody
    //     : LCURL NL* classMemberDeclarations NL* RCURL
    //     ;
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(CLASS_BODY)]
    #[rule($LCurl class_member_declarations: ClassMemberDeclarations $RCurl)]
    ClassBody {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        class_member_declarations: NodeRef,
    },

    /// Matches `LPAREN NL* (classParameter (NL* COMMA NL* classParameter)* (NL* COMMA)?)? NL* RPAREN`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $LParen
        (class_parameter: ClassParameter+{$Comma})?
        $RParen
    )]
    ClassParameters {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        class_parameter: Vec<NodeRef>,
    },

    /// Matches modifiers? (VAL | VAR)? NL* simpleIdentifier COLON NL* type (NL* ASSIGNMENT NL* expression)?
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(/*modifiers: Modifiers? */ (val_or_var: ($Val | $Var))? simple_identifier: SimpleIdentifier $Colon type_ref: Type ( $Assignment expression: Expression)?)]
    ClassParameter {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        // #[child]
        // modifiers: Vec<NodeRef>,
        #[child]
        val_or_var: TokenRef,
        #[child]
        simple_identifier: NodeRef,
        #[child]
        type_ref: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `annotatedDelegationSpecifier (NL* COMMA NL* annotatedDelegationSpecifier)*`
    #[denote(DELEGATION_SPECIFIERS)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(annotated_delegation_specifier: AnnotatedDelegationSpecifier+{$Comma})]
    DelegationSpecifiers {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotated_delegation_specifier: Vec<NodeRef>,
    },

    /// Matches `constructorInvocation | explicitDelegation | userType | functionType | SUSPEND NL* functionType`
    #[denote(DELEGATION_SPECIFIER)]
    #[rule(
        constructor_invocation: ConstructorInvocation
        | explicit_delegation: ExplicitDelegation
        | user_type: UserType
        | function_type: FunctionType
        | ( $Suspend $NL* suspend_function_type: FunctionType)
    )]
    DelegationSpecifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        constructor_invocation: NodeRef,
        #[child]
        explicit_delegation: NodeRef,
        #[child]
        user_type: NodeRef,
        #[child]
        function_type: NodeRef,
        #[child]
        suspend_function_type: NodeRef,
    },

    /// Matches `userType NL* valueArguments`
    #[denote(CONSTRUCTOR_INVOCATION)]
    #[rule(user_type: UserType $NL* value_arguments: ValueArguments)]
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

    /// Matches `annotation* NL* delegationSpecifier`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(annotations: Annotation* delegation_specifier: DelegationSpecifier)]
    #[denote(ANNOTATED_DELEGATION_SPECIFIER)]
    AnnotatedDelegationSpecifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotations: Vec<NodeRef>,
        #[child]
        delegation_specifier: NodeRef,
    },

    /// Matches ` (userType | functionType) NL* BY NL* expression`
    // TODO: handle limiting to funtionType in semantics phase
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(EXPLICIT_DELEGATION)]
    #[rule(
        (user_type: UserType | complex_type: ComplexType)
         $By expression: Expression
    )]
    ExplicitDelegation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        user_type: NodeRef,
        #[child]
        complex_type: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches ` LANGLE NL* typeParameter (NL* COMMA NL* typeParameter)* (NL* COMMA)? NL* RANGLE`
    #[denote(TYPE_PARAMETERS)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $LAngle
        type_parameters: TypeParameter+{$Comma}
        $RAngle
    )]
    TypeParameters {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        type_parameters: Vec<NodeRef>,
    },
    /// Matches `typeParameterModifiers? NL* simpleIdentifier (NL* COLON NL* type)?`
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(TYPE_PARAMETER)]
    #[rule(/* type_parameter_modifiers: TypeParameterModifiers?*/ simple_identifier: SimpleIdentifier ($Colon type_ref: Type)?)]
    TypeParameter {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        // #[child]
        // type_parameter_modifiers: Vec<NodeRef>,
        #[child]
        simple_identifier: NodeRef,
        #[child]
        type_ref: NodeRef,
    },

    /// Matches `WHERE NL* typeConstraint (NL* COMMA NL* typeConstraint)*`
    #[denote(TYPE_CONSTRAINTS)]
    #[rule($Where (type_constraints: TypeConstraint ($Comma type_constraints: TypeConstraint)*)?)]
    #[trivia(HIDDEN_WITH_NL)]
    TypeConstraints {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        type_constraints: Vec<NodeRef>,
    },

    /// Matches `annotation* NL* simpleIdentifier NL* COLON NL* type`
    #[denote(TYPE_CONSTRAINT)]
    #[rule(/*annotations: Vec<NodeRef>, */ identifier: SimpleIdentifier $Colon type_ref: Type)]
    #[trivia(HIDDEN_WITH_NL)]
    TypeConstraint {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        // annotations: Vec<NodeRef>,
        identifier: NodeRef,
        #[child]
        type_ref: NodeRef,
    },

    // SECTION: classMembers

    /// Matches `(classMemberDeclaration semis?)*`
    #[denote(CLASS_MEMBER_DECLARATIONS)]
    #[rule((class_member_declaration: ClassMemberDeclaration Semis?)*)]
    ClassMemberDeclarations {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        class_member_declaration: Vec<NodeRef>,
    },

    /// Matches `declaration | companionObject | anonymousInitializer | secondaryConstructor`
    #[denote(CLASS_MEMBER_DECLARATION)]
    #[rule(
        declaration: Declaration
        | companion_object: CompanionObject
        | anonymous_initializer: AnonymousInitializer
        | secondary_constructor: SecondaryConstructor
    )]
    ClassMemberDeclaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        declaration: NodeRef,
        #[child]
        companion_object: NodeRef,
        #[child]
        anonymous_initializer: NodeRef,
        #[child]
        secondary_constructor: NodeRef,
    },

    /// Matches `INIT NL* block`
    #[denote(ANONYMOUS_INITIALIZER)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule($Init $NL* block: Block)]
    AnonymousInitializer {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        block: NodeRef,
    },

    /// Matches `modifiers? COMPANION NL* DATA? NL* OBJECT 
    /// (NL* simpleIdentifier)? 
    /// (NL* COLON NL* delegationSpecifiers)? 
    /// (NL* classBody)?`
    #[denote(COMPANION_OBJECT)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        modifiers: Modifiers? $Companion $Data? $Object
        (identifier: SimpleIdentifier)?
        ($Colon delegation_specifiers: DelegationSpecifiers)?
        (class_body: ClassBody)?
    )]
    CompanionObject {
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

    /// Matches `LPAREN NL* (functionValueParameter (NL* COMMA NL* functionValueParameter)* (NL* COMMA)?)? NL* RPAREN`
    #[denote(FUNCTION_VALUE_PARAMETERS)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $LParen
        (function_value_parameter: FunctionValueParameter+{$Comma})?
        $RParen
    )]
    FunctionValueParameters {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        function_value_parameter: Vec<NodeRef>,
    },

    /// Matches `parameterModifiers? parameter (NL* ASSIGNMENT NL* expression)?`
    #[denote(FUNCTION_VALUE_PARAMETER)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(parameter_modifiers: ParameterModifiers? parameter: Parameter ($Assignment expression: Expression)?)]
    FunctionValueParameter {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        parameter_modifiers: NodeRef,
        #[child]
        parameter: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `modifiers? 
    /// FUN NL* (typeParameters)? (receiverType NL* DOT)? NL* simpleIdentifier 
    /// NL* functionValueParameters 
    /// (NL* COLON NL* type)? 
    /// (NL* typeConstraints)? 
    /// (NL* functionBody)?`
    #[denote(FUNCTION_DECLARATION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        modifiers: Modifiers?
        $Fun (type_parameters: TypeParameters)? (receiver_type: Type $Dot)?
        identifier: SimpleIdentifier
        function_value_parameters: FunctionValueParameters
        ($Colon return_type: Type)?
        (type_constraints: TypeConstraints)?
        (function_body: FunctionBody)?
    )]
    FunctionDeclaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        #[child]
        type_parameters: NodeRef,
        #[child]
        receiver_type: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        function_value_parameters: NodeRef,
        #[child]
        return_type: NodeRef,
        #[child]
        type_constraints: NodeRef,
        #[child]
        function_body: NodeRef,
    },
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
    #[rule(identifier: SimpleIdentifier ($Colon type_ref: Type)?)]
    VariableDeclarationWithoutAnnotation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        type_ref: NodeRef,
    },

    /// Matches `annotation* NL* simpleIdentifier (NL* COLON NL* type)?`
    /// /// As in:
    /// ```kt
    /// variable: Int
    /// ```
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(annotation: Annotation* declaration: VariableDeclarationWithoutAnnotation)]
    #[denote(VARIABLE_DECLARATION)]
    VariableDeclaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        declaration: NodeRef,
        #[child]
        annotation: Vec<NodeRef>,
    },

    /// Matches `LPAREN NL* variableDeclaration (NL* COMMA NL* variableDeclaration)* (NL* COMMA)? NL* RPAREN`
    /// As in:
    /// ```kt
    /// (variable: Int, another: String)
    /// ```
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


    /// Matches `modifiers? (VAL | VAR)
    ///  NL* typeParameters? 
    ///  NL* receiverType? 
    ///  NL* (multiVariableDeclaration | variableDeclaration) 
    ///  NL* typeConstraints? 
    ///  NL* (ASSIGNMENT NL* expression | propertyDelegate)? 
    ///  NL* SEMICOLON? NL* (getter? (NL* semi? setter?) | setter? (NL* semi? getter)?)`
    #[denote(PROPERTY_DECLARATION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        modifiers: Modifiers?
        ($Val | $Var)
        (type_parameters: TypeParameters)?
        (receiver_type: Type $Dot)?
        ((multi_variable_declaration: MultiVariableDeclaration | variable_declaration: VariableDeclaration))
        (type_constraints: TypeConstraints)?
        (($Assignment expression: Expression | property_delegate: PropertyDelegate))?
        ($Semicolon)?
        ((getter: Getter (Semi? setter: Setter)? | setter: Setter (Semi? getter: Getter)?))?
    )]
    PropertyDeclaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        #[child]
        type_parameters: NodeRef,
        #[child]
        receiver_type: NodeRef,
        #[child]
        multi_variable_declaration: NodeRef,
        #[child]
        variable_declaration: NodeRef,
        #[child]
        type_constraints: NodeRef,
        #[child]
        expression: NodeRef,
        #[child]
        property_delegate: NodeRef,
        #[child]
        getter: NodeRef,
        #[child]
        setter: NodeRef,
    },
    /// Matches `BY NL* expression`
    #[rule($By $NL* expression: Expression)]
    #[denote(PROPERTY_DELEGATE)]
    PropertyDelegate {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `modifiers? GET (NL* LPAREN NL* RPAREN (NL* COLON NL* type)? NL* functionBody)?`
    #[denote(GETTER)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        modifiers: Modifiers? $Get
        ( $LParen $RParen ($Colon type_ref: Type)?  function_body: FunctionBody)?
    )]
    Getter {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        #[child]
        type_ref: NodeRef,
        #[child]
        function_body: NodeRef,
    },

    /// Matches `modifiers? SET (NL* LPAREN NL* functionValueParameterWithOptionalType (NL* COMMA)? NL* RPAREN (NL* COLON NL* type)? NL* functionBody)?`
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

    /// Matches `modifiers? OBJECT NL* simpleIdentifier (NL* COLON NL* delegationSpecifiers)? (NL* classBody)?`
    #[rule(modifiers: Modifiers? $Object $NL* identifier: SimpleIdentifier
        // (delegation_specifiers: DelegationSpecifiers)?
        // (class_body: ClassBody)?
    )]
    #[denote(OBJECT_DECLARATION)]
    ObjectDeclaration {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        #[child]
        identifier: NodeRef,
        // #[child]
        // delegation_specifiers: NodeRef,
        // #[child]
        // class_body: NodeRef,
    },

    /// Matches `modifiers? CONSTRUCTOR NL* functionValueParameters (NL* COLON NL* constructorDelegationCall)? NL* block?`
    #[denote(SECONDARY_CONSTRUCTOR)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        modifiers: Modifiers? $Constructor
        //parameters: FunctionValueParameters
        ( $Colon constructor_delegation_call: ConstructorDelegationCall)?
        block: Block?
    )]
    SecondaryConstructor {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: NodeRef,
        // #[child]
        // parameters: NodeRef,
        #[child]
        constructor_delegation_call: NodeRef,
        #[child]
        block: NodeRef,
    },

    /// Matches `(THIS | SUPER) NL* valueArguments`
    #[denote(CONSTRUCTOR_DELEGATION_CALL)]
    #[rule(this_or_super: ($This | $Super) $NL* value_arguments: ValueArguments)]
    ConstructorDelegationCall {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        this_or_super: TokenRef,
        #[child]
        value_arguments: NodeRef,
    },

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

    /// Matches `(label | annotation)* ( declaration | assignment | loopStatement | expression)`
    #[rule((label: Label | annotation: Annotation)* (inner: BaseStatement | while_statement: WhileStatement))]
    Statement {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        label: Vec<NodeRef>,
        #[child]
        annotation: Vec<NodeRef>,
        #[child]
        inner: NodeRef,
        #[child]
        while_statement: NodeRef,
    },

    /// Todo: complete the rule
    /// ( declaration | assignment | loopStatement | expression)
    #[rule(
        (for_statement: ForStatement | do_while_statement: DoWhileStatement | expression: Expression)
    )]
    #[denote(BASE_STATEMENT)]
    BaseStatement {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        // #[child]
        // declaration: NodeRef,
        // #[child]
        // assignment: NodeRef,
        #[child]
        for_statement: NodeRef,
        #[child]
        do_while_statement: NodeRef,
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

    /// Matches `block | statement`
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

    /// Control structure body for do-while statements i.e. do not contain while statement
    #[rule(block: Block | statement: BaseStatement)]
    ControlStructureBodyForDoWhile {
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

    /// Matches `forStatement | whileStatement | doWhileStatement`
    #[rule(
        for_statement: ForStatement
        | while_statement: WhileStatement
        | do_while_statement: DoWhileStatement
    )]
    #[denote(LOOP_STATEMENT)]
    LoopStatement {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        for_statement: NodeRef,
        #[child]
        while_statement: NodeRef,
        #[child]
        do_while_statement: NodeRef,
    },

    /// Matches `FOR NL* LPAREN annotation* (variableDeclaration | multiVariableDeclaration) IN expression RPAREN NL* controlStructureBody?`
    #[rule(
        $For $LParen
            annotations: Annotation*
            (variable_declaration: VariableDeclarationWithoutAnnotation | multi_variable_declaration: MultiVariableDeclaration)
            $In expression: Expression
        $RParen body: ControlStructureBody?
    )]
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(FOR_STATEMENT)]
    ForStatement {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotations: Vec<NodeRef>,
        #[child]
        variable_declaration: NodeRef,
        #[child]
        multi_variable_declaration: NodeRef,
        #[child]
        expression: NodeRef,
        #[child]
        body: NodeRef,
    },

    /// Matches `WHILE NL* LPAREN expression RPAREN NL* (controlStructureBody | SEMICOLON)`
    #[rule(
        $While $LParen expression: Expression $RParen (body: ControlStructureBody | $Semicolon)
    )]
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(WHILE_STATEMENT)]
    WhileStatement {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expression: NodeRef,
        #[child]
        body: NodeRef,
    },

    /// Matches `DO NL* controlStructureBody? NL* WHILE NL* LPAREN expression RPAREN`
    #[rule(
        $Do body: ControlStructureBodyForDoWhile? $While $LParen expression: Expression $RParen
    )]
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(DO_WHILE_STATEMENT)]
    DoWhileStatement {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        body: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `(directlyAssignableExpression ASSIGNMENT | assignableExpression assignmentAndOperator) NL* expression`
    #[denote(ASSIGNMENT)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        (directly_assignable_expression: DirectlyAssignableExpression $Assignment
        | assignable_expression: AssignableExpression assignment_and_operator: AssignmentAndOperator)
        $NL* expression: Expression
    )]
    Assignment {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        directly_assignable_expression: NodeRef,
        #[child]
        assignable_expression: NodeRef,
        #[child]
        assignment_and_operator: NodeRef,
        #[child]
        expression: NodeRef,
    },

    // SECTION: expressions

    /// Matches `disjunction`
    #[denote(EXPRESSION)]
    #[rule(disjunction: Disjunction)]
    #[describe("expression", "expr")]
    Expression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        disjunction: NodeRef,
    },

    /// Matches `conjunction (NL* DISJ NL* conjunction)*`
    #[denote(DISJUNCTION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(conjunctions: Conjunction+{$Disj})]
    Disjunction {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        conjunctions: Vec<NodeRef>,
    },

    /// Matches `equality (NL* CONJ NL* equality)*`
    #[denote(CONJUNCTION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(equalities: EqualityExpression+{$Conj})]
    Conjunction {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        equalities: Vec<NodeRef>
    },

    /// Matches `comparison (equalityOperator NL* comparison)*`
    #[denote(EQUALITY_EXPRESSION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(comparisons: ComparisonExpression+{equality_operators: EqualityOperator})]
    EqualityExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        comparisons: Vec<NodeRef>,
        #[child]
        equality_operators: Vec<NodeRef>,
    },

    /// Matches `genericCallLikeComparison (comparisonOperator NL* genericCallLikeComparison)*`
    #[denote(COMPARISON_EXPRESSION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(generic_call_like_comparisons: GenericCallLikeComparison+{comparison_operators: ComparisonOperator})]
    ComparisonExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        generic_call_like_comparisons: Vec<NodeRef>,
        #[child]
        comparison_operators: Vec<NodeRef>,
    },

    /// Matches `infixOperation callSuffix*`
    #[denote(GENERIC_CALL_LIKE_COMPARISON)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(infix_operation: InfixOperation call_suffix: CallSuffix*)]
    GenericCallLikeComparison {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        infix_operation: NodeRef,
        #[child]
        call_suffix: Vec<NodeRef>,
    },

    /// Matches `elvisExpression (inOperator NL* elvisExpression | isOperator NL* type)*`
    #[denote(INFIX_OPERATION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        elvis_expression: ElvisExpression
        (
            in_operator: InOperator elvis_expression_in: ElvisExpression
            | is_operator: IsOperator type_ref: Type
        )*
    )]
    InfixOperation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        elvis_expression: NodeRef,
        #[child]
        in_operator: Vec<NodeRef>,
        #[child]
        elvis_expression_in: Vec<NodeRef>,
        #[child]
        is_operator: Vec<NodeRef>,
        #[child]
        type_ref: Vec<NodeRef>,
    },

    /// Matches `infixFunctionCall (NL* elvis NL* infixFunctionCall)*`
    #[denote(ELVIS_EXPRESSION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(infix_function_calls: InfixFunctionCall+{elvis: Elvis})]
    ElvisExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        infix_function_calls: Vec<NodeRef>,
        #[child]
        elvis: Vec<NodeRef>,
    },
    /// Matches `QUEST_NO_WS COLON`
    #[rule($QuestNoWs $Colon)]
    #[denote(ELVIS)]
    Elvis {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `rangeExpression (simpleIdentifier NL* rangeExpression)*`
    #[denote(INFIX_FUNCTION_CALL)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(range_expressions: RangeExpression+{simple_identifiers: SimpleIdentifier})]
    InfixFunctionCall {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        range_expressions: Vec<NodeRef>,
        #[child]
        simple_identifiers: Vec<NodeRef>,
    },

    /// Matches `additiveExpression (($Range | $RangeUntil) additiveExpression)*`
    #[denote(RANGE_EXPRESSION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(additive_expressions: AdditiveExpression+{($Range | $RangeUntil)})]
    RangeExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        additive_expressions: Vec<NodeRef>,
    },

    /// Matches `multiplicativeExpression (additiveOperator multiplicativeExpression)*`
    #[denote(ADDITIVE_EXPRESSION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(multiplicative_expressions: MultiplicativeExpression+{additive_operators: AdditiveOperator})]
    AdditiveExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        multiplicative_expressions: Vec<NodeRef>,
        #[child]
        additive_operators: Vec<NodeRef>,
    },


    /// Matches `asExpression (multiplicativeOperator NL* asExpression)*`
    #[denote(MULTIPLICATIVE_EXPRESSION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(as_expressions: AsExpression+{multiplicative_operators: MultiplicativeOperator})]
    MultiplicativeExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        as_expressions: Vec<NodeRef>,
        #[child]
        multiplicative_operators: Vec<NodeRef>,
    },


    /// Matches `prefixUnaryExpression (asOperator NL* type)*`
    #[denote(AS_EXPRESSION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(prefix_unary_expression: PrefixUnaryExpression (as_operator: AsOperator type_ref: Type)*)]
    AsExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        prefix_unary_expression: NodeRef,
        #[child]
        as_operator: Vec<NodeRef>,
        #[child]
        type_ref: Vec<NodeRef>,
    },

    /// Matches `unaryPrefix* postfixUnaryExpression`
    #[rule(unary_prefix: UnaryPrefix* postfix_unary_expression: PostfixUnaryExpression)]
    #[denote(PREFIX_UNARY_EXPRESSION)]
    PrefixUnaryExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        unary_prefix: Vec<NodeRef>,
        #[child]
        postfix_unary_expression: NodeRef,
    },

    /// Matches `annotation| label | prefixUnaryOperator NL*`
    #[rule(annotation: Annotation | label: Label | (prefix_unary_operator: PrefixUnaryOperator $NL*))]
    #[denote(PREFIXUNARY_EXPRESSION)]
    UnaryPrefix {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotation: NodeRef,
        #[child]
        label: NodeRef,
        #[child]
        prefix_unary_operator: NodeRef,
    },

    /// Matches `primaryExpression postfixUnarySuffix*`
    #[rule(primary: PrimaryExpression (suffix: PostfixUnarySuffix)*)]
    #[denote(POSTFIX_UNARY_EXPRESSION)]
    PostfixUnaryExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        primary: NodeRef,
        #[child]
        suffix: Vec<NodeRef>,
    },


    /// Matches `postfixUnaryOperator | typeArguments | callSuffix | indexingSuffix | navigationSuffix`
    #[rule(
        postfix_unary_operator: PostfixUnaryOperator 
        | type_arguments: TypeArguments 
        | call_suffix: CallSuffix 
        | indexing_suffix: IndexingSuffix 
        | navigation_suffix: NavigationSuffix
    )]
    #[denote(POSTFIX_UNARY_SUFFIX)]
    PostfixUnarySuffix {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        postfix_unary_operator: Vec<NodeRef>,
        #[child]
        type_arguments: Vec<NodeRef>,
        #[child]
        call_suffix: Vec<NodeRef>,
        #[child]
        indexing_suffix: Vec<NodeRef>,
        #[child]
        navigation_suffix: Vec<NodeRef>,
    },

    /// Matches `postfixUnaryExpression assignableSuffix | simpleIdentifier | parenthesizedDirectlyAssignableExpression`
    #[rule(
        (postfix_unary_expression: Expression assignable_suffix: AssignableSuffix)
        | identifier: SimpleIdentifier
        | parenthesized_expression: ParenthesizedDirectlyAssignableExpression
    )]
    #[denote(DIRECTLY_ASSIGNABLE_EXPRESSION)]
    DirectlyAssignableExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        postfix_unary_expression: NodeRef,
        #[child]
        assignable_suffix: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        parenthesized_expression: NodeRef,
    },

    /// Matches `LPAREN NL* directlyAssignableExpression NL* RPAREN`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule($LParen expression: DirectlyAssignableExpression $RParen)]
    ParenthesizedDirectlyAssignableExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `prefixUnaryExpression | parenthesizedAssignableExpression`
    #[rule(prefix_unary_expression: Expression | parenthesized_assignable_expression: ParenthesizedAssignableExpression)]
    #[denote(ASSIGNABLE_EXPRESSION)]
    AssignableExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        prefix_unary_expression: NodeRef,
        #[child]
        parenthesized_assignable_expression: NodeRef,
    },

    /// Matches `LPAREN NL* assignableExpression NL* RPAREN`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule($LParen expression: AssignableExpression $RParen)]
    ParenthesizedAssignableExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `typeArguments | IndexingSuffix | NavigationSuffix`
    #[denote(ASSIGNABLE_SUFFIX)]
    #[rule(
        type_arguments: TypeArguments
        | indexing_suffix: IndexingSuffix
        | navigation_suffix: NavigationSuffix
    )]
    AssignableSuffix {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        type_arguments: NodeRef,
        #[child]
        indexing_suffix: NodeRef,
        #[child]
        navigation_suffix: NodeRef,
    },

    /// Matches `LSQUARE NL* expression (NL* COMMA NL* expression)* (NL* COMMA)? NL* RSQUARE`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $LSquare
        (expression: Expression+{$Comma})?
        $RSquare
    )]
    #[denote(INDEXING_SUFFIX)]
    IndexingSuffix {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expression: Vec<NodeRef>,
    },

    /// Matches `memberAccessOperator NL* (simpleIdentifier | parenthesizedExpression | CLASS)`
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(NAVIGATION_SUFFIX)]
    #[rule(
        member_access_operator: MemberAccessOperator
        (identifier: SimpleIdentifier | parenthesized_expression: ParenthesizedExpression | $Class)
    )]
    NavigationSuffix {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        member_access_operator: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        parenthesized_expression: NodeRef,
    },

    /// Matches `typeArguments? (valueArguments? annotatedLambda | valueArguments)`
    #[denote(CALL_SUFFIX)]
    #[rule(
        type_arguments: TypeArguments?
        (
            (value_arguments: ValueArguments? annotated_lambda: AnnotatedLambda)
            | (value_arguments: ValueArguments)
        )
    )]
    CallSuffix {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        type_arguments: NodeRef,
        #[child]
        value_arguments: NodeRef,
        #[child]
        annotated_lambda: NodeRef,
    },

    /// Matches `annotation* label? NL* lambdaLiteral`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        annotations: Annotation*
        (label: SimpleIdentifier)?
        lambda_literal: LambdaLiteral
    )]
    #[denote(ANNOTATED_LAMBDA)]
    AnnotatedLambda {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotations: Vec<NodeRef>,
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

    /// Matches `LPAREN NL* (valueArgument (NL* COMMA NL* valueArgument)* (NL* COMMA)? NL*)? RPAREN`
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

    /// Matches `parenthesizedExpression | simpleIdentifier | literalConstant | stringLiteral | callableReference | functionLiteral | objectLiteral | collectionLiteral
    /// | thisExpression | superExpression | ifExpression | whenExpression | tryExpression | jumpExpression`
    #[denote(PRIMARY_EXPRESSION)]
    #[rule(
        parenthesized_expression: ParenthesizedExpression
        | identifier: SimpleIdentifier
        | literal_constant: LiteralConstant
        | string_literal: StringLiteral
        // | callable_reference: CallableReference
        // | function_literal: FunctionLiteral
        // | object_literal: ObjectLiteral
        | collection_literal: CollectionLiteral
        // | this_expression: ThisExpression
        // | super_expression: SuperExpression
        // | if_expression: IfExpression
        // | when_expression: WhenExpression
        // | try_expression: TryExpression
        // | jump_expression: JumpExpression
    )]
    PrimaryExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        parenthesized_expression: NodeRef,
        #[child]
        identifier: NodeRef,
        #[child]
        literal_constant: NodeRef,
        #[child]
        string_literal: NodeRef,
        // #[child]
        // callable_reference: NodeRef,
        // #[child]
        // function_literal: NodeRef,
        // #[child]
        // object_literal: NodeRef,
        #[child]
        collection_literal: NodeRef,
        // #[child]
        // this_expression: NodeRef,
        // #[child]
        // super_expression: NodeRef,
        // #[child]
        // if_expression: NodeRef,
        // #[child]
        // when_expression: NodeRef,
        // #[child]
        // try_expression: NodeRef,
        // #[child]
        // jump_expression: NodeRef,
    },

    /// Matches `LPAREN NL* expression NL* RPAREN`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule($LParen expression: Expression $RParen)]
    #[denote(PARENTHESIZED_EXPRESSION)]
    ParenthesizedExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expression: NodeRef,
    },
    /// Matches `LSQUARE NL* (expression (NL* COMMA NL* expression)* (NL* COMMA)? NL*)? RSQUARE`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $LSquare
        (expression: Expression+{$Comma})?
        $RSquare
    )]
    #[denote(COLLECTION_LITERAL)]
    CollectionLiteral {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expression: Vec<NodeRef>,
    },

    /// Matches `BooleanLiteral | IntegerLiteral | HexLiteral | BinLiteral | CharacterLiteral | RealLiteral | NullLiteral | LongLiteral | UnsignedLiteral`
    #[denote(LITERAL_CONSTANT)]
    #[rule(
        boolean_literal: $BooleanLiteral
        | integer_literal: $IntegerLiteral
        | hex_literal: $HexLiteral
        | bin_literal: $BinLiteral
        | character_literal: $CharacterLiteral
        | real_literal: ($DoubleLiteral | $FloatLiteral)
        | null_literal: $NullLiteral
        | long_literal: $LongLiteral
        | unsigned_literal: $UnsignedLiteral
    )]
    LiteralConstant {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        boolean_literal: TokenRef,
        #[child]
        integer_literal: TokenRef,
        #[child]
        hex_literal: TokenRef,
        #[child]
        bin_literal: TokenRef,
        #[child]
        character_literal: TokenRef,
        #[child]
        real_literal: TokenRef,
        #[child]
        null_literal: TokenRef,
        #[child]
        long_literal: TokenRef,
        #[child]
        unsigned_literal: TokenRef,
    },
    /// Matches `lineStringLiteral | multiLineStringLiteral`
    #[denote(STRING_LITERAL)]
    #[rule(
        line_string_literal: LineStringLiteral
        | multi_line_string_literal: MultiLineStringLiteral
    )]
    StringLiteral {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        line_string_literal: NodeRef,
        #[child]
        multi_line_string_literal: NodeRef,
    },

    /// Matches `QUOTE_OPEN (lineStringContent | lineStringExpression)* QUOTE_CLOSE`
    #[denote(LINE_STRING_LITERAL)]
    #[trivia]
    #[rule(
        QuoteOpen
        (
            line_string_content: LineStringContent
            | line_string_expression: LineStringExpression
        )*
        QuoteClose
    )]
    LineStringLiteral {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        line_string_content: Vec<NodeRef>,
        #[child]
        line_string_expression: Vec<NodeRef>,
    },

    /// Matches `QUOTE_OPEN (MultiLineStringContent | MultiLineStringExpression)* QUOTE_CLOSE`
    /// Note that when extracting contents, that the contents in TripleQuoteClose should be extracted.
    #[denote(MULTI_LINE_STRING_LITERAL)]
    #[trivia]
    #[rule(
        TripleQuoteOpen
        (
            multi_line_string_content: MultiLineStringContent
            | multi_line_string_expression: MultiLineStringExpression

        )*
        TripleQuoteClose
    )]
    MultiLineStringLiteral {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        multi_line_string_content: Vec<NodeRef>,
        #[child]
        multi_line_string_expression: Vec<NodeRef>,
    },

    /// Matches `LineStrText | LineStrEscapedChar | LineStrRef`
    #[denote(LINE_STRING_CONTENT)]
    #[trivia]
    #[rule(
        line_str_text: LineStrText
        | line_str_escaped_char: $LineStrEscapedChar
        | line_str_ref: LineStrRef
    )]
    LineStringContent {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        line_str_text: NodeRef,
        #[child]
        line_str_escaped_char: TokenRef,
        #[child]
        line_str_ref: NodeRef,
    },

    /// Matches `~('\\' | '"' | '$')+ | '$'` inside a line string
    #[rule((^[$Escape | $Dollar | $StrExprStart | $LineStrEscapedChar | $SingleQuote])+)]
    #[trivia]
    #[denote(LINE_STR_TEXT)]
    LineStrText {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },
    /// Matches `LineStrExprStart NL* expression NL* RCURL`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        line_str_expr_start: LineStrExprStart
        expression: Expression
        $RCurl
    )]
    #[denote(LINE_STRING_EXPRESSION)]
    LineStringExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        line_str_expr_start: NodeRef,
        #[child]
        expression: NodeRef,
    },

    /// Matches `MultiLineStrText | MultiLineStringQuote | MultiLineStrRef`
    /// Note that MultiLineStringQuote is already captured in MultiLineStrText
    #[denote(MULTI_LINE_STRING_CONTENT)]
    #[trivia]
    #[rule(
        multi_line_str_text: MultiLineStrText
        | multi_line_str_ref: MultiLineStrRef
    )]
    MultiLineStringContent {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        multi_line_str_text: NodeRef,
        #[child]
        multi_line_str_ref: NodeRef,
    },

    /// Matches `~('"' | '$')+ | '$'` inside a line string
    /// Note that we do not negate single qoute here as in the spec.
    /// This is because we removed MultiLineStringQuote from MultiLineStringContent
    /// to make the grammar work with ll1.
    #[rule((^[$Dollar | $StrExprStart | $TripleQuote])+)]
    #[trivia]
    #[denote(MULTI_LINE_STR_TEXT)]
    MultiLineStrText {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `MultiLineStrExprStart NL* expression NL* RCURL`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        multi_line_str_expr_start: MultiLineStrExprStart
        expression: Expression
        $RCurl
    )]
    #[denote(MULTI_LINE_STRING_EXPRESSION)]
    MultiLineStringExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        multi_line_str_expr_start: NodeRef,
        #[child]
        expression: NodeRef,
    },

    #[rule($StrExprStart)]
    MultiLineStrExprStart {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    #[rule($StrExprStart)]
    LineStrExprStart {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `FieldIdentifier` inside a line string
    #[rule(field_identifier: FieldIdentifier)]
    LineStrRef {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        field_identifier: NodeRef,
    },

    /// Matches `FieldIdentifier` inside a multiline string
    #[rule(field_identifier: FieldIdentifier)]
    MultiLineStrRef {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        field_identifier: NodeRef,
    },

    /// Matches `'$' IdentifierOrSoftKey` inside a line string
    /// But IdentifierOrSoftKey is made optional here so this rule can capture single $,
    /// which was supposed to be in [LineStrText] but conflicted with this
    #[rule($Dollar SimpleIdentifier?)]
    #[denote(FIELD_IDENTIFIER)]
    FieldIdentifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    #[rule($SingleQuote)]
    QuoteOpen {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    #[rule($SingleQuote)]
    QuoteClose {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    #[rule($TripleQuote)]
    TripleQuoteOpen {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches MultiLineStringQuote? '"""'
    /// But note that this is the inverted form
    #[rule($TripleQuote+ $SingleQuote*)]
    TripleQuoteClose {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `LCURL NL* (lambdaParameters? NL* ARROW NL*)? statements NL* RCURL`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $LCurl
        (lambda_parameters: /* Todo: LambdaParameters */  $Arrow)?
        statements: Statements
        $RCurl
    )]
    #[denote(LAMBDA_LITERAL)]
    LambdaLiteral {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        lambda_parameters: TokenRef, // Todo: Change to LambdaParameters
        #[child]
        statements: NodeRef,
    },

    /// Matches `lambdaParameter (NL* COMMA NL* lambdaParameter)* (NL* COMMA)?`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        lambda_parameter: LambdaParameter+{$Comma}
    )]
    #[denote(LAMBDA_PARAMETERS)]
    LambdaParameters {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        lambda_parameter: Vec<NodeRef>,
    },

    /// Matches `variableDeclaration | multiVariableDeclaration (NL* COLON NL* type)?`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(variable_declaration: VariableDeclaration | multi_variable_declaration: MultiVariableDeclaration ($Colon type_ref: Type)?)]
    #[denote(LAMBDA_PARAMETER)]
    LambdaParameter {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        variable_declaration: NodeRef,
        #[child]
        multi_variable_declaration: NodeRef,
        #[child]
        type_ref: NodeRef,
    },

    /// Matches `SUSPEND? FUN (type DOT)? ParametersWithOptionalType (COLON type)? (TypeConstraints)? (FunctionBody)?`
    #[denote(ANONYMOUS_FUNCTION)]
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        suspend: $Suspend? $Fun
        (receiver_type: Type $Dot)?
        // TODO:  parameters_with_optional_type: Type
        ($Colon  return_type: Type)?
        (type_constraints: TypeConstraints)?
        (function_body: FunctionBody)?
    )]
    AnonymousFunction {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        suspend: TokenRef,
        #[child]
        receiver_type: NodeRef,
        #[child]
        return_type: NodeRef,
        // #[child]
        // /// Can only be [ParametersWithOptionalType]
        // parameters_with_optional_type: NodeRef,
        #[child]
        type_constraints: NodeRef,
        #[child]
        function_body: NodeRef,
    },

    /// Matches `lambdaLiteral | anonymousFunction`
    #[denote(FUNCTION_LITERAL)]
    #[rule(
        lambda_literal: LambdaLiteral
        | anonymous_function: AnonymousFunction
    )]
    FunctionLiteral {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        lambda_literal: NodeRef,
        #[child]
        anonymous_function: NodeRef,
    },

    /// Matches `DATA? NL* OBJECT (NL* COLON NL* delegationSpecifiers NL*)? (NL* classBody)?`
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(OBJECT_LITERAL)]
    #[rule($Data? $Object ( $Colon /* delegation_specifiers: DelegationSpecifiers */)? /* (class_body: ClassBody)? */)]
    ObjectLiteral {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        // #[child]
        // delegation_specifiers: NodeRef,
        // #[child]
        // class_body: NodeRef,
    },

    /// Matches `THIS | THIS_AT`
    #[rule(this_token: $This | this_token: $ThisAt)]
    #[denote(THIS_EXPRESSION)]
    ThisExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        this_token: TokenRef,
    },

    /// Matches `SUPER (LANGLE NL* type NL* RANGLE)? (AT_NO_WS simpleIdentifier)? | SUPER_AT`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        super_token: $Super
        ( $LAngle type_ref: Type $RAngle )?
        ( $AtNoWs identifier: SimpleIdentifier )?
        | super_token: $SuperAt
    )]
    #[denote(SUPER_EXPRESSION)]
    SuperExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        super_token: TokenRef,
        #[child]
        type_ref: NodeRef,
        #[child]
        identifier: NodeRef,
    },

    /// Matches `IF NL* LPAREN NL* expression NL* RPAREN NL* (controlStructureBody | controlStructureBody? NL* SEMICOLON? NL* ELSE NL* (controlStructureBody | SEMICOLON) | SEMICOLON)`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule(
        $If $LParen expression: Expression $RParen
        (
            (body: ControlStructureBody)
            | (body: ControlStructureBody? $Semicolon? $Else (else_body: ControlStructureBody | $Semicolon))
            | ($Semicolon)
        )
    )]
    #[denote(IF_EXPRESSION)]
    IfExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expression: NodeRef,
        #[child]
        body: NodeRef,
        #[child]
        else_body: NodeRef,
    },

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

    /// Matches `receiverType? COLONCOLON NL* (simpleIdentifier | CLASS)`
    #[denote(CALLABLE_REFERENCE)]
    #[rule(
        receiver_type: Type? $ColonColon $NL*
        (identifier: SimpleIdentifier | $Class)
    )]
    CallableReference {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        receiver_type: NodeRef,
        #[child]
        identifier: NodeRef,
    },

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

    #[rule(type_parameter_modifiers: TypeParameterModifier+)]
    #[denote(TYPE_PARAMETER_MODIFIERS)]
    TypeParameterModifiers {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        type_parameter_modifiers: Vec<NodeRef>,
    },

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
