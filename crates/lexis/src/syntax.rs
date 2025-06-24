//! Notes:
//! 1 - $NL should not be used at the start of any rule. If it is needed, apply it at the site of usage

use lady_deirdre::{
    lexis::TokenRef,
    syntax::{Node, NodeRef},
};

#[cfg(test)]
mod tests;

use crate::tokens::KotlinToken;

// TODO: implement strings

#[derive(Node)]
#[token(KotlinToken)]
#[define(HIDDEN = $Whitespace | LineComment | DelimitedComment)]
#[define(HIDDEN_WITH_NL = $Whitespace | $NL | LineComment | DelimitedComment)]
#[trivia(HIDDEN)]
#[define(HiddenBefore =  $Whitespace | DelimitedComment)]
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

    #[rule($NL* shebang_line: ShebangLine? type_test:Type+ $NL*)]
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
        type_test: Vec<NodeRef>,
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

    /// Matches `LANGLE NL* typeProjection (NL* COMMA NL* typeProjection)* (NL* COMMA)? NL* RANGLE`
    #[rule($LAngle args: Type+{$Comma} $RAngle)]
    #[denote(TYPE_ARGUMENTS)]
    TypeArguments {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        args: Vec<NodeRef>,
    },

    /// Matches `Identifier | SOFT_KEYWORDS`
    /// Because we parse non-ascii identifier, we changed this from the original source
    #[rule(inner:($Identifier | SOFT_KEYWORDS_SANS_SUSPEND))]
    #[describe("simple identifier")]
    #[denote(SIMPLE_IDENTIFIER)]
    SimpleIdentifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        inner: TokenRef,
    },

    /// Matches `simpleIdentifier (NL* DOT simpleIdentifier)*`
    #[rule(idents: SimpleIdentifier ($NL* $Dot idents: SimpleIdentifier)*)]
    #[denote(IDENTIFIER)]
    // #[recovery($Whitespace)]
    #[describe("identifier")]
    Identifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        idents: Vec<NodeRef>,
    },

    // SECTION: types
    // Section Notes : Quest is always $QuestNoWs
    /// Matches typeModifiers? (functionType | parenthesizedType | nullableType | typeReference | definitelyNonNullableType)
    #[rule(modifiers: Annotation* (((suspend: $Suspend $NL*)? complex: ComplexType) | simple: SimpleType) )]
    #[denote(TYPE)]
    Type {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        modifiers: Vec<NodeRef>,
        #[child]
        suspend: TokenRef,
        #[child]
        complex: NodeRef,
        #[child]
        simple: NodeRef,
    },

    #[rule(NonSuspendTypeModifier* DefinitelyNonNullableType)]
    #[denote(T16)]
    Type2 {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    #[denote(COMPLEX_TYPE)]
    #[rule(
        ($LParen bracketed:Type+{$Comma} $RParen) // Comma-separated list: (foo.x, bar -> baz, fuz)
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

    #[rule(inner: GenericTypeReference ($NL* $Dot inner: GenericTypeReference)*)]
    #[denote(SIMPLE_TYPE)]
    SimpleType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        inner: Vec<NodeRef>,
    },

    #[rule(reference: TypeReference args:(TypeArguments)?)]
    #[denote(G_TYPE_REFERENCE)]
    GenericTypeReference {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        reference: NodeRef,
        #[child]
        args: NodeRef,
    },

    /// Matches userType | DYNAMIC.
    /// But note that this is unneccessarily repititive as `
    /// DYNAMIC` is already covered by UserType -> SimpleUserType -> SimpleIdentifier
    /// So it is not included in the rule
    #[rule(identifier: Identifier)]
    #[denote(TYPE_REFERENCE)]
    TypeReference {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
    },

    /// Matches (typeReference | parenthesizedType) NL* quest+
    #[rule((TypeReference  | ParenthesizedType) $NL* $QuestNoWs+)]
    #[denote(TEMP10)]
    NullableType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `simpleUserType (NL* DOT NL* simpleUserType)*`
    #[rule(main_type: SimpleUserType ($NL* $Dot $NL* other_types: SimpleUserType)*)]
    #[denote(TEMP9)]
    UserType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        main_type: NodeRef,
        #[child]
        other_types: Vec<NodeRef>,
    },

    /// Matches simpleIdentifier (NL* typeArguments)?
    #[rule(ident: SimpleIdentifier ($NL* type_args: TypeArguments)?)]
    #[denote(SIMPLE_USER_TYPE)]
    SimpleUserType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        ident: NodeRef,
        #[child]
        type_args: NodeRef,
    },

    /// Matches `typeProjectionModifiers? type | MULT`
    #[rule((modifiers: TypeProjectionModifier* /*Type2 (remove Div)*/ $Div) | $Mult)]
    #[denote(TEMP7)]
    TypeProjection {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        /// Could only be non-empty when TypeProjection is Type variant
        modifiers: Vec<NodeRef>,
    },

    #[rule((VarianceModifier $NL*) | Annotation)]
    TypeProjectionModifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    // Matches (receiverType NL* DOT NL*)? functionTypeParameters NL* ARROW NL* type
    // #[trivia(HIDDEN_WITH_NL)]
    // #[rule((ReceiverType $Dot)? FunctionTypeParameters $Arrow Type2 )]
    // #[denote(T14)]
    // FunctionType {
    //     #[node]
    //     node: NodeRef,
    //     #[parent]
    //     parent: NodeRef,
    // },
    /// FunctionType using baseReciever Type2 for making other nodes
    /// Matches (baseReceiverType NL* DOT NL*)? functionTypeParameters NL* ARROW NL* type
    // #[trivia(HIDDEN_WITH_NL)]
    // #[rule((BaseReceiverType $Dot)? FunctionTypeParameters $Arrow Type2)]
    // #[denote(T25)]
    // BaseFunctionType {
    //     #[node]
    //     node: NodeRef,
    //     #[parent]
    //     parent: NodeRef,
    // },

    /// Matches ` LPAREN NL* (parameter | type)? (NL* COMMA NL* (parameter | type))* (NL* COMMA)? NL* RPAREN`
    #[trivia(HIDDEN_WITH_NL)]
    #[rule($LParen (/* Parameter | */ Type2)? ($Comma (/*Parameter|*/  Type2))* ($Comma)? $RParen)]
    #[denote(T13)]
    FunctionTypeParameters {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `LPAREN NL* type NL* RPAREN`
    #[rule($LParen Type2 $RParen)]
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(T22)]
    ParenthesizedType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `typeModifiers? (parenthesizedType | nullableType | typeReference)`
    // #[rule(type_modifiers: TypeModifier* base: BaseReceiverType)]
    // #[denote(T23)]
    // ReceiverType {
    //     #[node]
    //     node: NodeRef,
    //     #[parent]
    //     parent: NodeRef,
    //     #[child]
    //     type_modifiers: Vec<NodeRef>,
    //     #[child]
    //     base: NodeRef,
    // },

    /// Matches `(parenthesizedType | nullableType | typeReference)`
    // #[rule(ParenthesizedType | NullableType | TypeReference)]
    // #[denote(T24)]
    // BaseReceiverType {
    //     #[node]
    //     node: NodeRef,
    //     #[parent]
    //     parent: NodeRef,
    // },

    /// Matches LPAREN NL* (userType | parenthesizedUserType) NL* RPAREN
    #[rule($LParen inner: (UserType | ParenthesizedUserType) $RParen)]
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(TEMP11)]
    ParenthesizedUserType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        inner: NodeRef,
    },

    /// Matches `typeModifiers? (userType | parenthesizedUserType) NL* AMP NL* typeModifiers? (userType | parenthesizedUserType)`
    /// TODO: reintroduce typeModifiers?
    #[rule(first_type: (UserType | ParenthesizedUserType) $NL* $Amp $NL* modifiers: NonSuspendTypeModifier* second_type: (UserType | ParenthesizedUserType))]
    DefinitelyNonNullableType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        first_type: NodeRef,
        #[child]
        modifiers: Vec<NodeRef>,
        #[child]
        second_type: NodeRef,
    },

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

    // functionBody
    //     : block
    //     | ASSIGNMENT NL* expression
    //     ;

    // variableDeclaration
    //     : annotation* NL* simpleIdentifier (NL* COLON NL* type)?
    //     ;

    // multiVariableDeclaration
    //     : LPAREN NL* variableDeclaration (NL* COMMA NL* variableDeclaration)* (NL* COMMA)? NL* RPAREN
    //     ;

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

    // parametersWithOptionalType
    //     : LPAREN NL* (functionValueParameterWithOptionalType (NL* COMMA NL* functionValueParameterWithOptionalType)* (NL* COMMA)?)? NL* RPAREN
    //     ;

    // functionValueParameterWithOptionalType
    //     : parameterModifiers? parameterWithOptionalType (NL* ASSIGNMENT NL* expression)?
    //     ;

    // parameterWithOptionalType
    //     : simpleIdentifier NL* (COLON NL* type)?
    //     ;
    /// Matches `simpleIdentifier NL* COLON NL* type`
    #[rule(identifier: SimpleIdentifier $Colon /* Type2 */)]
    #[trivia(HIDDEN_WITH_NL)]
    #[denote(PARAMETER)]
    Parameter {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
    },

    // objectDeclaration
    //     : modifiers? OBJECT
    //       NL* simpleIdentifier
    //       (NL* COLON NL* delegationSpecifiers)?
    //       (NL* classBody)?
    //     ;

    // secondaryConstructor
    //     : modifiers? CONSTRUCTOR NL* functionValueParameters (NL* COLON NL* constructorDelegationCall)? NL* block?
    //     ;

    // constructorDelegationCall
    //     : (THIS | SUPER) NL* valueArguments
    //     ;

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
    #[rule($ExclNoWs $Is $NL?)]
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
    #[rule($ExclNoWs $In $NL?)]
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
