#![allow(nonstandard_style)]
// Generated from KotlinParser.g4 by ANTLR 4.8
use antlr_rust::tree::{ParseTreeVisitor};
use super::kotlinparser::*;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link KotlinParser}.
 */
pub trait KotlinParserVisitor<'input>: ParseTreeVisitor<'input,KotlinParserContextType>{
	/**
	 * Visit a parse tree produced by {@link KotlinParser#kotlinFile}.
	 * @param ctx the parse tree
	 */
	fn visit_kotlinFile(&mut self, ctx: &KotlinFileContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#script}.
	 * @param ctx the parse tree
	 */
	fn visit_script(&mut self, ctx: &ScriptContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#shebangLine}.
	 * @param ctx the parse tree
	 */
	fn visit_shebangLine(&mut self, ctx: &ShebangLineContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#fileAnnotation}.
	 * @param ctx the parse tree
	 */
	fn visit_fileAnnotation(&mut self, ctx: &FileAnnotationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#packageHeader}.
	 * @param ctx the parse tree
	 */
	fn visit_packageHeader(&mut self, ctx: &PackageHeaderContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#importList}.
	 * @param ctx the parse tree
	 */
	fn visit_importList(&mut self, ctx: &ImportListContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#importHeader}.
	 * @param ctx the parse tree
	 */
	fn visit_importHeader(&mut self, ctx: &ImportHeaderContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#importAlias}.
	 * @param ctx the parse tree
	 */
	fn visit_importAlias(&mut self, ctx: &ImportAliasContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#topLevelObject}.
	 * @param ctx the parse tree
	 */
	fn visit_topLevelObject(&mut self, ctx: &TopLevelObjectContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeAlias}.
	 * @param ctx the parse tree
	 */
	fn visit_typeAlias(&mut self, ctx: &TypeAliasContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#declaration}.
	 * @param ctx the parse tree
	 */
	fn visit_declaration(&mut self, ctx: &DeclarationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#classDeclaration}.
	 * @param ctx the parse tree
	 */
	fn visit_classDeclaration(&mut self, ctx: &ClassDeclarationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#primaryConstructor}.
	 * @param ctx the parse tree
	 */
	fn visit_primaryConstructor(&mut self, ctx: &PrimaryConstructorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#classBody}.
	 * @param ctx the parse tree
	 */
	fn visit_classBody(&mut self, ctx: &ClassBodyContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#classParameters}.
	 * @param ctx the parse tree
	 */
	fn visit_classParameters(&mut self, ctx: &ClassParametersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#classParameter}.
	 * @param ctx the parse tree
	 */
	fn visit_classParameter(&mut self, ctx: &ClassParameterContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#delegationSpecifiers}.
	 * @param ctx the parse tree
	 */
	fn visit_delegationSpecifiers(&mut self, ctx: &DelegationSpecifiersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#delegationSpecifier}.
	 * @param ctx the parse tree
	 */
	fn visit_delegationSpecifier(&mut self, ctx: &DelegationSpecifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#constructorInvocation}.
	 * @param ctx the parse tree
	 */
	fn visit_constructorInvocation(&mut self, ctx: &ConstructorInvocationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#annotatedDelegationSpecifier}.
	 * @param ctx the parse tree
	 */
	fn visit_annotatedDelegationSpecifier(&mut self, ctx: &AnnotatedDelegationSpecifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#explicitDelegation}.
	 * @param ctx the parse tree
	 */
	fn visit_explicitDelegation(&mut self, ctx: &ExplicitDelegationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeParameters}.
	 * @param ctx the parse tree
	 */
	fn visit_typeParameters(&mut self, ctx: &TypeParametersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeParameter}.
	 * @param ctx the parse tree
	 */
	fn visit_typeParameter(&mut self, ctx: &TypeParameterContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeConstraints}.
	 * @param ctx the parse tree
	 */
	fn visit_typeConstraints(&mut self, ctx: &TypeConstraintsContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeConstraint}.
	 * @param ctx the parse tree
	 */
	fn visit_typeConstraint(&mut self, ctx: &TypeConstraintContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#classMemberDeclarations}.
	 * @param ctx the parse tree
	 */
	fn visit_classMemberDeclarations(&mut self, ctx: &ClassMemberDeclarationsContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#classMemberDeclaration}.
	 * @param ctx the parse tree
	 */
	fn visit_classMemberDeclaration(&mut self, ctx: &ClassMemberDeclarationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#anonymousInitializer}.
	 * @param ctx the parse tree
	 */
	fn visit_anonymousInitializer(&mut self, ctx: &AnonymousInitializerContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#companionObject}.
	 * @param ctx the parse tree
	 */
	fn visit_companionObject(&mut self, ctx: &CompanionObjectContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#functionValueParameters}.
	 * @param ctx the parse tree
	 */
	fn visit_functionValueParameters(&mut self, ctx: &FunctionValueParametersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#functionValueParameter}.
	 * @param ctx the parse tree
	 */
	fn visit_functionValueParameter(&mut self, ctx: &FunctionValueParameterContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#functionDeclaration}.
	 * @param ctx the parse tree
	 */
	fn visit_functionDeclaration(&mut self, ctx: &FunctionDeclarationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#functionBody}.
	 * @param ctx the parse tree
	 */
	fn visit_functionBody(&mut self, ctx: &FunctionBodyContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#variableDeclaration}.
	 * @param ctx the parse tree
	 */
	fn visit_variableDeclaration(&mut self, ctx: &VariableDeclarationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#multiVariableDeclaration}.
	 * @param ctx the parse tree
	 */
	fn visit_multiVariableDeclaration(&mut self, ctx: &MultiVariableDeclarationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#propertyDeclaration}.
	 * @param ctx the parse tree
	 */
	fn visit_propertyDeclaration(&mut self, ctx: &PropertyDeclarationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#propertyDelegate}.
	 * @param ctx the parse tree
	 */
	fn visit_propertyDelegate(&mut self, ctx: &PropertyDelegateContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#getter}.
	 * @param ctx the parse tree
	 */
	fn visit_getter(&mut self, ctx: &GetterContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#setter}.
	 * @param ctx the parse tree
	 */
	fn visit_setter(&mut self, ctx: &SetterContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parametersWithOptionalType}.
	 * @param ctx the parse tree
	 */
	fn visit_parametersWithOptionalType(&mut self, ctx: &ParametersWithOptionalTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#functionValueParameterWithOptionalType}.
	 * @param ctx the parse tree
	 */
	fn visit_functionValueParameterWithOptionalType(&mut self, ctx: &FunctionValueParameterWithOptionalTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parameterWithOptionalType}.
	 * @param ctx the parse tree
	 */
	fn visit_parameterWithOptionalType(&mut self, ctx: &ParameterWithOptionalTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parameter}.
	 * @param ctx the parse tree
	 */
	fn visit_parameter(&mut self, ctx: &ParameterContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#objectDeclaration}.
	 * @param ctx the parse tree
	 */
	fn visit_objectDeclaration(&mut self, ctx: &ObjectDeclarationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#secondaryConstructor}.
	 * @param ctx the parse tree
	 */
	fn visit_secondaryConstructor(&mut self, ctx: &SecondaryConstructorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#constructorDelegationCall}.
	 * @param ctx the parse tree
	 */
	fn visit_constructorDelegationCall(&mut self, ctx: &ConstructorDelegationCallContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#enumClassBody}.
	 * @param ctx the parse tree
	 */
	fn visit_enumClassBody(&mut self, ctx: &EnumClassBodyContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#enumEntries}.
	 * @param ctx the parse tree
	 */
	fn visit_enumEntries(&mut self, ctx: &EnumEntriesContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#enumEntry}.
	 * @param ctx the parse tree
	 */
	fn visit_enumEntry(&mut self, ctx: &EnumEntryContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#type}.
	 * @param ctx the parse tree
	 */
	fn visit_type(&mut self, ctx: &TypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeReference}.
	 * @param ctx the parse tree
	 */
	fn visit_typeReference(&mut self, ctx: &TypeReferenceContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#nullableType}.
	 * @param ctx the parse tree
	 */
	fn visit_nullableType(&mut self, ctx: &NullableTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#quest}.
	 * @param ctx the parse tree
	 */
	fn visit_quest(&mut self, ctx: &QuestContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#userType}.
	 * @param ctx the parse tree
	 */
	fn visit_userType(&mut self, ctx: &UserTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#simpleUserType}.
	 * @param ctx the parse tree
	 */
	fn visit_simpleUserType(&mut self, ctx: &SimpleUserTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeProjection}.
	 * @param ctx the parse tree
	 */
	fn visit_typeProjection(&mut self, ctx: &TypeProjectionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeProjectionModifiers}.
	 * @param ctx the parse tree
	 */
	fn visit_typeProjectionModifiers(&mut self, ctx: &TypeProjectionModifiersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeProjectionModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_typeProjectionModifier(&mut self, ctx: &TypeProjectionModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#functionType}.
	 * @param ctx the parse tree
	 */
	fn visit_functionType(&mut self, ctx: &FunctionTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#functionTypeParameters}.
	 * @param ctx the parse tree
	 */
	fn visit_functionTypeParameters(&mut self, ctx: &FunctionTypeParametersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parenthesizedType}.
	 * @param ctx the parse tree
	 */
	fn visit_parenthesizedType(&mut self, ctx: &ParenthesizedTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#receiverType}.
	 * @param ctx the parse tree
	 */
	fn visit_receiverType(&mut self, ctx: &ReceiverTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parenthesizedUserType}.
	 * @param ctx the parse tree
	 */
	fn visit_parenthesizedUserType(&mut self, ctx: &ParenthesizedUserTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#definitelyNonNullableType}.
	 * @param ctx the parse tree
	 */
	fn visit_definitelyNonNullableType(&mut self, ctx: &DefinitelyNonNullableTypeContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#statements}.
	 * @param ctx the parse tree
	 */
	fn visit_statements(&mut self, ctx: &StatementsContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#statement}.
	 * @param ctx the parse tree
	 */
	fn visit_statement(&mut self, ctx: &StatementContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#label}.
	 * @param ctx the parse tree
	 */
	fn visit_label(&mut self, ctx: &LabelContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#controlStructureBody}.
	 * @param ctx the parse tree
	 */
	fn visit_controlStructureBody(&mut self, ctx: &ControlStructureBodyContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#block}.
	 * @param ctx the parse tree
	 */
	fn visit_block(&mut self, ctx: &BlockContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#loopStatement}.
	 * @param ctx the parse tree
	 */
	fn visit_loopStatement(&mut self, ctx: &LoopStatementContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#forStatement}.
	 * @param ctx the parse tree
	 */
	fn visit_forStatement(&mut self, ctx: &ForStatementContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#whileStatement}.
	 * @param ctx the parse tree
	 */
	fn visit_whileStatement(&mut self, ctx: &WhileStatementContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#doWhileStatement}.
	 * @param ctx the parse tree
	 */
	fn visit_doWhileStatement(&mut self, ctx: &DoWhileStatementContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#assignment}.
	 * @param ctx the parse tree
	 */
	fn visit_assignment(&mut self, ctx: &AssignmentContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#semi}.
	 * @param ctx the parse tree
	 */
	fn visit_semi(&mut self, ctx: &SemiContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#semis}.
	 * @param ctx the parse tree
	 */
	fn visit_semis(&mut self, ctx: &SemisContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#expression}.
	 * @param ctx the parse tree
	 */
	fn visit_expression(&mut self, ctx: &ExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#disjunction}.
	 * @param ctx the parse tree
	 */
	fn visit_disjunction(&mut self, ctx: &DisjunctionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#conjunction}.
	 * @param ctx the parse tree
	 */
	fn visit_conjunction(&mut self, ctx: &ConjunctionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#equality}.
	 * @param ctx the parse tree
	 */
	fn visit_equality(&mut self, ctx: &EqualityContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#comparison}.
	 * @param ctx the parse tree
	 */
	fn visit_comparison(&mut self, ctx: &ComparisonContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#genericCallLikeComparison}.
	 * @param ctx the parse tree
	 */
	fn visit_genericCallLikeComparison(&mut self, ctx: &GenericCallLikeComparisonContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#infixOperation}.
	 * @param ctx the parse tree
	 */
	fn visit_infixOperation(&mut self, ctx: &InfixOperationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#elvisExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_elvisExpression(&mut self, ctx: &ElvisExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#elvis}.
	 * @param ctx the parse tree
	 */
	fn visit_elvis(&mut self, ctx: &ElvisContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#infixFunctionCall}.
	 * @param ctx the parse tree
	 */
	fn visit_infixFunctionCall(&mut self, ctx: &InfixFunctionCallContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#rangeExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_rangeExpression(&mut self, ctx: &RangeExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#additiveExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_additiveExpression(&mut self, ctx: &AdditiveExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#multiplicativeExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_multiplicativeExpression(&mut self, ctx: &MultiplicativeExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#asExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_asExpression(&mut self, ctx: &AsExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#prefixUnaryExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_prefixUnaryExpression(&mut self, ctx: &PrefixUnaryExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#unaryPrefix}.
	 * @param ctx the parse tree
	 */
	fn visit_unaryPrefix(&mut self, ctx: &UnaryPrefixContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#postfixUnaryExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_postfixUnaryExpression(&mut self, ctx: &PostfixUnaryExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#postfixUnarySuffix}.
	 * @param ctx the parse tree
	 */
	fn visit_postfixUnarySuffix(&mut self, ctx: &PostfixUnarySuffixContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#directlyAssignableExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_directlyAssignableExpression(&mut self, ctx: &DirectlyAssignableExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parenthesizedDirectlyAssignableExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_parenthesizedDirectlyAssignableExpression(&mut self, ctx: &ParenthesizedDirectlyAssignableExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#assignableExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_assignableExpression(&mut self, ctx: &AssignableExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parenthesizedAssignableExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_parenthesizedAssignableExpression(&mut self, ctx: &ParenthesizedAssignableExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#assignableSuffix}.
	 * @param ctx the parse tree
	 */
	fn visit_assignableSuffix(&mut self, ctx: &AssignableSuffixContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#indexingSuffix}.
	 * @param ctx the parse tree
	 */
	fn visit_indexingSuffix(&mut self, ctx: &IndexingSuffixContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#navigationSuffix}.
	 * @param ctx the parse tree
	 */
	fn visit_navigationSuffix(&mut self, ctx: &NavigationSuffixContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#callSuffix}.
	 * @param ctx the parse tree
	 */
	fn visit_callSuffix(&mut self, ctx: &CallSuffixContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#annotatedLambda}.
	 * @param ctx the parse tree
	 */
	fn visit_annotatedLambda(&mut self, ctx: &AnnotatedLambdaContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeArguments}.
	 * @param ctx the parse tree
	 */
	fn visit_typeArguments(&mut self, ctx: &TypeArgumentsContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#valueArguments}.
	 * @param ctx the parse tree
	 */
	fn visit_valueArguments(&mut self, ctx: &ValueArgumentsContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#valueArgument}.
	 * @param ctx the parse tree
	 */
	fn visit_valueArgument(&mut self, ctx: &ValueArgumentContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#primaryExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_primaryExpression(&mut self, ctx: &PrimaryExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parenthesizedExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_parenthesizedExpression(&mut self, ctx: &ParenthesizedExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#collectionLiteral}.
	 * @param ctx the parse tree
	 */
	fn visit_collectionLiteral(&mut self, ctx: &CollectionLiteralContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#literalConstant}.
	 * @param ctx the parse tree
	 */
	fn visit_literalConstant(&mut self, ctx: &LiteralConstantContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#stringLiteral}.
	 * @param ctx the parse tree
	 */
	fn visit_stringLiteral(&mut self, ctx: &StringLiteralContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#lineStringLiteral}.
	 * @param ctx the parse tree
	 */
	fn visit_lineStringLiteral(&mut self, ctx: &LineStringLiteralContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#multiLineStringLiteral}.
	 * @param ctx the parse tree
	 */
	fn visit_multiLineStringLiteral(&mut self, ctx: &MultiLineStringLiteralContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#lineStringContent}.
	 * @param ctx the parse tree
	 */
	fn visit_lineStringContent(&mut self, ctx: &LineStringContentContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#lineStringExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_lineStringExpression(&mut self, ctx: &LineStringExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#multiLineStringContent}.
	 * @param ctx the parse tree
	 */
	fn visit_multiLineStringContent(&mut self, ctx: &MultiLineStringContentContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#multiLineStringExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_multiLineStringExpression(&mut self, ctx: &MultiLineStringExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#lambdaLiteral}.
	 * @param ctx the parse tree
	 */
	fn visit_lambdaLiteral(&mut self, ctx: &LambdaLiteralContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#lambdaParameters}.
	 * @param ctx the parse tree
	 */
	fn visit_lambdaParameters(&mut self, ctx: &LambdaParametersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#lambdaParameter}.
	 * @param ctx the parse tree
	 */
	fn visit_lambdaParameter(&mut self, ctx: &LambdaParameterContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#anonymousFunction}.
	 * @param ctx the parse tree
	 */
	fn visit_anonymousFunction(&mut self, ctx: &AnonymousFunctionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#functionLiteral}.
	 * @param ctx the parse tree
	 */
	fn visit_functionLiteral(&mut self, ctx: &FunctionLiteralContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#objectLiteral}.
	 * @param ctx the parse tree
	 */
	fn visit_objectLiteral(&mut self, ctx: &ObjectLiteralContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#thisExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_thisExpression(&mut self, ctx: &ThisExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#superExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_superExpression(&mut self, ctx: &SuperExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#ifExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_ifExpression(&mut self, ctx: &IfExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#whenSubject}.
	 * @param ctx the parse tree
	 */
	fn visit_whenSubject(&mut self, ctx: &WhenSubjectContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#whenExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_whenExpression(&mut self, ctx: &WhenExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#whenEntry}.
	 * @param ctx the parse tree
	 */
	fn visit_whenEntry(&mut self, ctx: &WhenEntryContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#whenCondition}.
	 * @param ctx the parse tree
	 */
	fn visit_whenCondition(&mut self, ctx: &WhenConditionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#rangeTest}.
	 * @param ctx the parse tree
	 */
	fn visit_rangeTest(&mut self, ctx: &RangeTestContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeTest}.
	 * @param ctx the parse tree
	 */
	fn visit_typeTest(&mut self, ctx: &TypeTestContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#tryExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_tryExpression(&mut self, ctx: &TryExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#catchBlock}.
	 * @param ctx the parse tree
	 */
	fn visit_catchBlock(&mut self, ctx: &CatchBlockContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#finallyBlock}.
	 * @param ctx the parse tree
	 */
	fn visit_finallyBlock(&mut self, ctx: &FinallyBlockContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#jumpExpression}.
	 * @param ctx the parse tree
	 */
	fn visit_jumpExpression(&mut self, ctx: &JumpExpressionContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#callableReference}.
	 * @param ctx the parse tree
	 */
	fn visit_callableReference(&mut self, ctx: &CallableReferenceContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#assignmentAndOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_assignmentAndOperator(&mut self, ctx: &AssignmentAndOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#equalityOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_equalityOperator(&mut self, ctx: &EqualityOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#comparisonOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_comparisonOperator(&mut self, ctx: &ComparisonOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#inOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_inOperator(&mut self, ctx: &InOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#isOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_isOperator(&mut self, ctx: &IsOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#additiveOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_additiveOperator(&mut self, ctx: &AdditiveOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#multiplicativeOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_multiplicativeOperator(&mut self, ctx: &MultiplicativeOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#asOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_asOperator(&mut self, ctx: &AsOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#prefixUnaryOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_prefixUnaryOperator(&mut self, ctx: &PrefixUnaryOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#postfixUnaryOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_postfixUnaryOperator(&mut self, ctx: &PostfixUnaryOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#excl}.
	 * @param ctx the parse tree
	 */
	fn visit_excl(&mut self, ctx: &ExclContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#memberAccessOperator}.
	 * @param ctx the parse tree
	 */
	fn visit_memberAccessOperator(&mut self, ctx: &MemberAccessOperatorContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#safeNav}.
	 * @param ctx the parse tree
	 */
	fn visit_safeNav(&mut self, ctx: &SafeNavContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#modifiers}.
	 * @param ctx the parse tree
	 */
	fn visit_modifiers(&mut self, ctx: &ModifiersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parameterModifiers}.
	 * @param ctx the parse tree
	 */
	fn visit_parameterModifiers(&mut self, ctx: &ParameterModifiersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#modifier}.
	 * @param ctx the parse tree
	 */
	fn visit_modifier(&mut self, ctx: &ModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeModifiers}.
	 * @param ctx the parse tree
	 */
	fn visit_typeModifiers(&mut self, ctx: &TypeModifiersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_typeModifier(&mut self, ctx: &TypeModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#classModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_classModifier(&mut self, ctx: &ClassModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#memberModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_memberModifier(&mut self, ctx: &MemberModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#visibilityModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_visibilityModifier(&mut self, ctx: &VisibilityModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#varianceModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_varianceModifier(&mut self, ctx: &VarianceModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeParameterModifiers}.
	 * @param ctx the parse tree
	 */
	fn visit_typeParameterModifiers(&mut self, ctx: &TypeParameterModifiersContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#typeParameterModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_typeParameterModifier(&mut self, ctx: &TypeParameterModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#functionModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_functionModifier(&mut self, ctx: &FunctionModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#propertyModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_propertyModifier(&mut self, ctx: &PropertyModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#inheritanceModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_inheritanceModifier(&mut self, ctx: &InheritanceModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#parameterModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_parameterModifier(&mut self, ctx: &ParameterModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#reificationModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_reificationModifier(&mut self, ctx: &ReificationModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#platformModifier}.
	 * @param ctx the parse tree
	 */
	fn visit_platformModifier(&mut self, ctx: &PlatformModifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#annotation}.
	 * @param ctx the parse tree
	 */
	fn visit_annotation(&mut self, ctx: &AnnotationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#singleAnnotation}.
	 * @param ctx the parse tree
	 */
	fn visit_singleAnnotation(&mut self, ctx: &SingleAnnotationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#multiAnnotation}.
	 * @param ctx the parse tree
	 */
	fn visit_multiAnnotation(&mut self, ctx: &MultiAnnotationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#annotationUseSiteTarget}.
	 * @param ctx the parse tree
	 */
	fn visit_annotationUseSiteTarget(&mut self, ctx: &AnnotationUseSiteTargetContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#unescapedAnnotation}.
	 * @param ctx the parse tree
	 */
	fn visit_unescapedAnnotation(&mut self, ctx: &UnescapedAnnotationContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#simpleIdentifier}.
	 * @param ctx the parse tree
	 */
	fn visit_simpleIdentifier(&mut self, ctx: &SimpleIdentifierContext<'input>) { self.visit_children(ctx) }

	/**
	 * Visit a parse tree produced by {@link KotlinParser#identifier}.
	 * @param ctx the parse tree
	 */
	fn visit_identifier(&mut self, ctx: &IdentifierContext<'input>) { self.visit_children(ctx) }


}