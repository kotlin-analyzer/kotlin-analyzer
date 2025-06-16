use lady_deirdre::{
    lexis::TokenRef,
    syntax::{Node, NodeRef},
};

#[cfg(test)]
mod tests;

use crate::parser::{
    parse_delimited_comment, parse_identifier_token, parse_line_comment, parse_shebang_line,
};
use crate::tokens::KotlinToken;

// TODO: implement strings

#[derive(Node)]
#[token(KotlinToken)]
#[define(HIDDEN = $Whitespace | LineComment | DelimitedComment)]
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
)]
#[define(ANNOTATION_TYPE = ($Field | $Property | $Get | $Set | $Receiver | $Param | $Setparam | $Delegate))]
#[define(SOFT_KEYWORDS = (
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
    | $Suspend
    | $Value
))]
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

    #[rule(shebang_line: ShebangLine? $NL* file_annotations: FileAnnotation* ident_test: IdentifierToken+ $NL*)]
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
        ident_test: Vec<NodeRef>,
    },

    // #[rule(shebang_line: ShebangLine? $NL* ident_test: IdentifierToken+ $NL*)]
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
    #[rule($Hash $ExclNoWs)]
    #[parser(parse_shebang_line(session))]
    #[denote(SHEBANG_LINE)]
    #[describe("shebangline", "#! ...")]
    ShebangLine {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `(AT_NO_WS | AT_PRE_WS) FILE NL* COLON NL* (LSQUARE unescapedAnnotation+ RSQUARE | unescapedAnnotation) NL*`
    // Becuase this comes always before $NL* in Kotlin file we don't need to handle AT_PRE_WS
    #[rule($AtNoWs $File $NL* $Colon $NL* ($LSquare /* unescapedAnnotation+ */ $RSquare /* | unescapedAnnotation */ ) $NL*)]
    #[describe("file annotation", "file")]
    FileAnnotation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    #[rule($LAngle TypeProjection)]
    #[denote(TEMP8)]
    TypeArguments {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `IdentifierToken | SOFT_KEYWORDS`
    #[rule(SOFT_KEYWORDS | IdentifierToken)]
    #[denote(TEMP)]
    #[describe("simple identifier")]
    SimpleIdenitifer {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `simpleIdentifier (NL* DOT simpleIdentifier)*`
    #[rule(SimpleIdenitifer ($NL* $Dot SimpleIdenitifer)*)]
    #[denote(TEMP2)]
    #[describe("identifier")]
    Identifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    // SECTION: types
    // Section Notes : Quest is always $QuestNoWs
    /// Matches typeModifiers? (functionType | parenthesizedType | nullableType | typeReference | definitelyNonNullableType)
    #[rule(TypeModifier* (FunctionType | ParenthesizedType /*| NullableType */ | TypeReference | DefinitelyNonNullableType))]
    #[denote(T15)]
    Type {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches userType | DYNAMIC.
    /// But note that this is unneccessarily repititive as `
    /// DYNAMIC` is already covered by UserType -> SimpleUserType -> SimpleIdentifier
    /// So it is not included in the rule
    #[rule(UserType)]
    TypeReference {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
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
    #[rule(ident: SimpleIdenitifer ($NL* type_args: TypeArguments)?)]
    #[denote(TEMP5)]
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
    #[rule((modifiers: TypeProjectionModifier* /*Type (remove Div)*/ $Div) | $Mult)]
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
    #[rule((ReceiverType $NL* $Dot $NL*)? FunctionTypeParameters $NL* $Arrow $NL* Type )]
    #[denote(T14)]
    FunctionType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches ` LPAREN NL* (parameter | type)? (NL* COMMA NL* (parameter | type))* (NL* COMMA)? NL* RPAREN`
    #[rule($LParen $NL* (Parameter | Type)? ($NL* $Comma $NL* (Parameter | Type))* ($NL* $Comma)? $NL* $RParen)]
    #[denote(T13)]
    FunctionTypeParameters {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `LPAREN NL* type NL* RPAREN`
    #[rule($LParen $NL* Type $NL* $RParen)]
    #[denote(T22)]
    ParenthesizedType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `typeModifiers? (parenthesizedType | nullableType | typeReference)`
    #[rule(type_modifiers: TypeModifier* kt_type: (ParenthesizedType | NullableType | TypeReference))]
    #[denote(T23)]
    ReceiverType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        type_modifiers: Vec<NodeRef>,
        #[child]
        kt_type: NodeRef,
    },

    /// Matches LPAREN NL* (userType | parenthesizedUserType) NL* RPAREN
    #[rule($LParen $NL* inner: (UserType | ParenthesizedUserType) $NL* $RParen)]
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
    #[rule(first_modifiers: TypeModifier* first_type: (UserType | ParenthesizedUserType)$ NL* $Amp $NL* second_modifiers: TypeModifier* second_type: (UserType | ParenthesizedUserType))]
    DefinitelyNonNullableType {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        first_modifiers: Vec<NodeRef>,
        #[child]
        first_type: NodeRef,
        #[child]
        second_modifiers: Vec<NodeRef>,
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
    #[rule(identifier: SimpleIdenitifer $NL* $Colon $NL* /* Type */)]
    #[denote(TEMP12)]
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
    #[denote(T16)]
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
    #[rule(($NL* $AtNoWs annotation_type: ANNOTATION_TYPE $NL* $Colon $NL*  | $AtNoWs) (($LSquare /* UnescapedAnnotation+ */ $RSquare) /* | UnescapedAnnotation+ */) $NL*)  ]
    #[denote(TEMP6)]
    Annotation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        annotation_type: TokenRef,
    },

    // unescapedAnnotation
    //     : constructorInvocation
    //     | userType
    //     ;
    ///  constructorInvocation| userType
    // #[rule()]
    // #[describe("Unescaped Annotation")]
    UnescapedAnnotation {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /* Leaf Nodes - Mostly complex tokens that could not be handled by the lexer */
    /// Matches `(Letter | '_') (Letter | '_' | UnicodeDigit)*`
    /// | '`' ~([\r\n] | '`')+ '`'
    #[rule($Tick | $AsciiIdentifier | $MisMatch )]
    #[denote(IDENTIFIER_TOKEN)]
    #[parser(parse_identifier_token(session))]
    #[secondary]
    #[describe("identifier", "ident")]
    IdentifierToken {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    // Keywords with Identifier
    /// Matches `"return@" Identifier`
    #[rule($Return $AtNoWs identifier: IdentifierToken)]
    #[denote(RETURN_AT)]
    #[secondary]
    #[describe("return@<label>", "return@")]
    ReturnAt {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
    },

    /// Matches `"continue@" Identifier`
    #[rule($Continue $AtNoWs identifier: IdentifierToken)]
    #[denote(SOFT_KEYWORDS)]
    #[secondary]
    #[describe("continue@<label>", "continue@")]
    ContinueAt {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
    },

    /// Matches `"break@" Identifier`
    #[rule($Break $AtNoWs identifier: IdentifierToken)]
    #[denote(BREAK_AT)]
    #[secondary]
    #[describe("break@<label>", "break@")]
    BreakAt {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
    },

    /// Matches `"this@" Identifier`
    #[rule($This $AtNoWs identifier: IdentifierToken)]
    #[denote(THIS_AT)]
    #[secondary]
    #[describe("this@<label>", "this@")]
    ThisAt {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
    },

    /// Matches `"super@" Identifier`
    #[rule($Super $AtNoWs identifier: IdentifierToken)]
    #[denote(SUPER_AT)]
    #[secondary]
    #[describe("super@<label>", "super@")]
    SuperAt {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        identifier: NodeRef,
    },

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
    #[rule($LineCommentStart)]
    #[denote(LINE_COMMENT)]
    #[parser(parse_line_comment(session))]
    #[describe("line comment")]
    #[secondary]
    LineComment {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `_*'/*' ( DelimitedComment | . )*? '*/'`
    #[rule($DelimitedCommentStart)]
    #[denote(DELIMITED_COMMENT)]
    #[parser(parse_delimited_comment(session))]
    #[describe("delimited comment")]
    #[secondary]
    DelimitedComment {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },
}
