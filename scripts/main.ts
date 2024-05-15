// used to pre format the texts from kotlin spec before using inside the code
// run deno run scripts/main.ts

function main() {
  let spec = content();
  // replace '"""' -> TRIPLE_QUOTE_OPEN
  // spec = spec.replace(/'"""'/g, "TRIPLE_QUOTE_OPEN");
  // // replace 'c' -> ~c~
  // spec = spec.replace(/'([^'])'/g, "~$1~");
  // // replace 'many' -> "many"
  // spec = spec.replace(/'([^']{2,})'/g, '"$1"');
  // // replace ~c~ -> 'c'
  // spec = spec.replace(/~([^~])~/g, "'$1'");
  // // replace type -> TYPE
  // spec = spec.replace(/\btype\b/g, "TYPE");
  // formatting
  let lines = spec.split("\n").map((l) => {
    let line = l.trim();
    // to every line without : at end add \t
    // but add a line above
    if (line.match(/\w+:/)) {
      return `\n  ${line}`;
    }
    return `    ${line}`;
  });
  spec = lines.join("\n");
  console.log(spec);
}

function content() {
  return `
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
        importList
        {statement semi}
        EOF
        shebangLine:
        ShebangLine (NL {NL})
        fileAnnotation:
        (AT_NO_WS | AT_PRE_WS)
        'file'
        {NL}
        ':'
        {NL}
        (('[' (unescapedAnnotation {unescapedAnnotation}) ']') | unescapedAnnotation)
        {NL}
        packageHeader:
        ['package' identifier [semi]]
        importList:
        {importHeader}
        importHeader:
        'import' identifier [('.' '*') | importAlias] [semi]
        importAlias:
        'as' simpleIdentifier
        topLevelObject:
        declaration [semis]
        typeAlias:
        [modifiers]
        'typealias'
        {NL}
        simpleIdentifier
        [{NL} typeParameters]
        {NL}
        '='
        {NL}
        type
        declaration:
        classDeclaration
        | objectDeclaration
        | functionDeclaration
        | propertyDeclaration
        | typeAlias
        classDeclaration:
        [modifiers]
        ('class' | (['fun' {NL}] 'interface'))
        {NL}
        simpleIdentifier
        [{NL} typeParameters]
        [{NL} primaryConstructor]
        [{NL} ':' {NL} delegationSpecifiers]
        [{NL} typeConstraints]
        [({NL} classBody) | ({NL} enumClassBody)]
        primaryConstructor:
        [[modifiers] 'constructor' {NL}] classParameters
        classBody:
        '{'
        {NL}
        classMemberDeclarations
        {NL}
        '}'
        classParameters:
        '('
        {NL}
        [classParameter {{NL} ',' {NL} classParameter} [{NL} ',']]
        {NL}
        ')'
        classParameter:
        [modifiers]
        ['val' | 'var']
        {NL}
        simpleIdentifier
        ':'
        {NL}
        type
        [{NL} '=' {NL} expression]
        delegationSpecifiers:
        annotatedDelegationSpecifier {{NL} ',' {NL} annotatedDelegationSpecifier}
        delegationSpecifier:
        constructorInvocation
        | explicitDelegation
        | userType
        | functionType
        | ('suspend' {NL} functionType)
        constructorInvocation:
        userType {NL} valueArguments
        annotatedDelegationSpecifier:
        {annotation} {NL} delegationSpecifier
        explicitDelegation:
        (userType | functionType)
        {NL}
        'by'
        {NL}
        expression
        typeParameters:
        '<'
        {NL}
        typeParameter
        {{NL} ',' {NL} typeParameter}
        [{NL} ',']
        {NL}
        '>'
        typeParameter:
        [typeParameterModifiers] {NL} simpleIdentifier [{NL} ':' {NL} type]
        typeConstraints:
        'where' {NL} typeConstraint {{NL} ',' {NL} typeConstraint}
        typeConstraint:
        {annotation}
        simpleIdentifier
        {NL}
        ':'
        {NL}
        type
        classMemberDeclarations:
        {classMemberDeclaration [semis]}
        classMemberDeclaration:
        declaration
        | companionObject
        | anonymousInitializer
        | secondaryConstructor
        anonymousInitializer:
        'init' {NL} block
        companionObject:
        [modifiers]
        'companion'
        {NL}
        ['data']
        {NL}
        'object'
        [{NL} simpleIdentifier]
        [{NL} ':' {NL} delegationSpecifiers]
        [{NL} classBody]
        functionValueParameters:
        '('
        {NL}
        [functionValueParameter {{NL} ',' {NL} functionValueParameter} [{NL} ',']]
        {NL}
        ')'
        functionValueParameter:
        [parameterModifiers] parameter [{NL} '=' {NL} expression]
        functionDeclaration:
        [modifiers]
        'fun'
        [{NL} typeParameters]
        [{NL} receiverType {NL} '.']
        {NL}
        simpleIdentifier
        {NL}
        functionValueParameters
        [{NL} ':' {NL} type]
        [{NL} typeConstraints]
        [{NL} functionBody]
        functionBody:
        block
        | ('=' {NL} expression)
        variableDeclaration:
        {annotation} {NL} simpleIdentifier [{NL} ':' {NL} type]
        multiVariableDeclaration:
        '('
        {NL}
        variableDeclaration
        {{NL} ',' {NL} variableDeclaration}
        [{NL} ',']
        {NL}
        ')'
        propertyDeclaration:
        [modifiers]
        ('val' | 'var')
        [{NL} typeParameters]
        [{NL} receiverType {NL} '.']
        ({NL} (multiVariableDeclaration | variableDeclaration))
        [{NL} typeConstraints]
        [{NL} (('=' {NL} expression) | propertyDelegate)]
        [{NL} ';']
        {NL}
        (([getter] [{NL} [semi] setter]) | ([setter] [{NL} [semi] getter]))
        propertyDelegate:
        'by' {NL} expression
        getter:
        [modifiers] 'get' [{NL} '(' {NL} ')' [{NL} ':' {NL} type] {NL} functionBody]
        setter:
        [modifiers] 'set' [{NL} '(' {NL} functionValueParameterWithOptionalType [{NL} ','] {NL} ')' [{NL} ':' {NL} type] {NL} functionBody]
        parametersWithOptionalType:
        '('
        {NL}
        [functionValueParameterWithOptionalType {{NL} ',' {NL} functionValueParameterWithOptionalType} [{NL} ',']]
        {NL}
        ')'
        functionValueParameterWithOptionalType:
        [parameterModifiers] parameterWithOptionalType [{NL} '=' {NL} expression]
        parameterWithOptionalType:
        simpleIdentifier {NL} [':' {NL} type]
        parameter:
        simpleIdentifier
        {NL}
        ':'
        {NL}
        type
        objectDeclaration:
        [modifiers]
        'object'
        {NL}
        simpleIdentifier
        [{NL} ':' {NL} delegationSpecifiers]
        [{NL} classBody]
        secondaryConstructor:
        [modifiers]
        'constructor'
        {NL}
        functionValueParameters
        [{NL} ':' {NL} constructorDelegationCall]
        {NL}
        [block]
        constructorDelegationCall:
        ('this' | 'super') {NL} valueArguments
        enumClassBody:
        '{'
        {NL}
        [enumEntries]
        [{NL} ';' {NL} classMemberDeclarations]
        {NL}
        '}'
        enumEntries:
        enumEntry {{NL} ',' {NL} enumEntry} {NL} [',']
        enumEntry:
        [modifiers {NL}] simpleIdentifier [{NL} valueArguments] [{NL} classBody]
        type:
        [typeModifiers] (functionType | parenthesizedType | nullableType | typeReference | definitelyNonNullableType)
        typeReference:
        userType
        | 'dynamic'
        nullableType:
        (typeReference | parenthesizedType) {NL} (quest {quest})
        quest:
        QUEST_NO_WS
        | QUEST_WS
        userType:
        simpleUserType {{NL} '.' {NL} simpleUserType}
        simpleUserType:
        simpleIdentifier [{NL} typeArguments]
        typeProjection:
        ([typeProjectionModifiers] type)
        | '*'
        typeProjectionModifiers:
        typeProjectionModifier {typeProjectionModifier}
        typeProjectionModifier:
        (varianceModifier {NL})
        | annotation
        functionType:
        [receiverType {NL} '.' {NL}]
        functionTypeParameters
        {NL}
        '->'
        {NL}
        type
        functionTypeParameters:
        '('
        {NL}
        [parameter | type]
        {{NL} ',' {NL} (parameter | type)}
        [{NL} ',']
        {NL}
        ')'
        parenthesizedType:
        '('
        {NL}
        type
        {NL}
        ')'
        receiverType:
        [typeModifiers] (parenthesizedType | nullableType | typeReference)
        parenthesizedUserType:
        '('
        {NL}
        (userType | parenthesizedUserType)
        {NL}
        ')'
        definitelyNonNullableType:
        [typeModifiers]
        (userType | parenthesizedUserType)
        {NL}
        '&'
        {NL}
        [typeModifiers]
        (userType | parenthesizedUserType)
        statements:
        [statement {semis statement}] [semis]
        statement:
        {label | annotation} (declaration | assignment | loopStatement | expression)
        label:
        simpleIdentifier (AT_NO_WS | AT_POST_WS) {NL}
        controlStructureBody:
        block
        | statement
        block:
        '{'
        {NL}
        statements
        {NL}
        '}'
        loopStatement:
        forStatement
        | whileStatement
        | doWhileStatement
        forStatement:
        'for'
        {NL}
        '('
        {annotation}
        (variableDeclaration | multiVariableDeclaration)
        'in'
        expression
        ')'
        {NL}
        [controlStructureBody]
        whileStatement:
        'while'
        {NL}
        '('
        expression
        ')'
        {NL}
        (controlStructureBody | ';')
        doWhileStatement:
        'do'
        {NL}
        [controlStructureBody]
        {NL}
        'while'
        {NL}
        '('
        expression
        ')'
        assignment:
        ((directlyAssignableExpression '=') | (assignableExpression assignmentAndOperator)) {NL} expression
        semi:
        (';' | NL) {NL}
        semis:
        ';' | NL {';' | NL}
        expression:
        disjunction
        disjunction:
        conjunction {{NL} '||' {NL} conjunction}
        conjunction:
        equality {{NL} '&&' {NL} equality}
        equality:
        comparison {equalityOperator {NL} comparison}
        comparison:
        genericCallLikeComparison {comparisonOperator {NL} genericCallLikeComparison}
        genericCallLikeComparison:
        infixOperation {callSuffix}
        infixOperation:
        elvisExpression {(inOperator {NL} elvisExpression) | (isOperator {NL} type)}
        elvisExpression:
        infixFunctionCall {{NL} elvis {NL} infixFunctionCall}
        elvis:
        QUEST_NO_WS ':'
        infixFunctionCall:
        rangeExpression {simpleIdentifier {NL} rangeExpression}
        rangeExpression:
        additiveExpression {('..' | '..<') {NL} additiveExpression}
        additiveExpression:
        multiplicativeExpression {additiveOperator {NL} multiplicativeExpression}
        multiplicativeExpression:
        asExpression {multiplicativeOperator {NL} asExpression}
        asExpression:
        prefixUnaryExpression {{NL} asOperator {NL} type}
        prefixUnaryExpression:
        {unaryPrefix} postfixUnaryExpression
        unaryPrefix:
        annotation
        | label
        | (prefixUnaryOperator {NL})
        postfixUnaryExpression:
        primaryExpression {postfixUnarySuffix}
        postfixUnarySuffix:
        postfixUnaryOperator
        | typeArguments
        | callSuffix
        | indexingSuffix
        | navigationSuffix
        directlyAssignableExpression:
        (postfixUnaryExpression assignableSuffix)
        | simpleIdentifier
        | parenthesizedDirectlyAssignableExpression
        parenthesizedDirectlyAssignableExpression:
        '('
        {NL}
        directlyAssignableExpression
        {NL}
        ')'
        assignableExpression:
        prefixUnaryExpression
        | parenthesizedAssignableExpression
        parenthesizedAssignableExpression:
        '('
        {NL}
        assignableExpression
        {NL}
        ')'
        assignableSuffix:
        typeArguments
        | indexingSuffix
        | navigationSuffix
        indexingSuffix:
        '['
        {NL}
        expression
        {{NL} ',' {NL} expression}
        [{NL} ',']
        {NL}
        ']'
        navigationSuffix:
        memberAccessOperator {NL} (simpleIdentifier | parenthesizedExpression | 'class')
        callSuffix:
        [typeArguments] (([valueArguments] annotatedLambda) | valueArguments)
        annotatedLambda:
        {annotation} [label] {NL} lambdaLiteral
        typeArguments:
        '<'
        {NL}
        typeProjection
        {{NL} ',' {NL} typeProjection}
        [{NL} ',']
        {NL}
        '>'
        valueArguments:
        '(' {NL} [valueArgument {{NL} ',' {NL} valueArgument} [{NL} ','] {NL}] ')'
        valueArgument:
        [annotation]
        {NL}
        [simpleIdentifier {NL} '=' {NL}]
        ['*']
        {NL}
        expression
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
        parenthesizedExpression:
        '('
        {NL}
        expression
        {NL}
        ')'
        collectionLiteral:
        '[' {NL} [expression {{NL} ',' {NL} expression} [{NL} ','] {NL}] ']'
        literalConstant:
        BooleanLiteral
        | IntegerLiteral
        | HexLiteral
        | BinLiteral
        | CharacterLiteral
        | RealLiteral
        | 'null'
        | LongLiteral
        | UnsignedLiteral
        stringLiteral:
        lineStringLiteral
        | multiLineStringLiteral
        lineStringLiteral:
        '"' {lineStringContent | lineStringExpression} '"'
        multiLineStringLiteral:
        '"""' {multiLineStringContent | multiLineStringExpression | '"'} TRIPLE_QUOTE_CLOSE
        lineStringContent:
        LineStrText
        | LineStrEscapedChar
        | LineStrRef
        lineStringExpression:
        '\${'
        {NL}
        expression
        {NL}
        '}'
        multiLineStringContent:
        MultiLineStrText
        | '"'
        | MultiLineStrRef
        multiLineStringExpression:
        '\${'
        {NL}
        expression
        {NL}
        '}'
        lambdaLiteral:
        '{'
        {NL}
        [[lambdaParameters] {NL} '->' {NL}]
        statements
        {NL}
        '}'
        lambdaParameters:
        lambdaParameter {{NL} ',' {NL} lambdaParameter} [{NL} ',']
        lambdaParameter:
        variableDeclaration
        | (multiVariableDeclaration [{NL} ':' {NL} type])
        anonymousFunction:
        ['suspend']
        {NL}
        'fun'
        [{NL} type {NL} '.']
        {NL}
        parametersWithOptionalType
        [{NL} ':' {NL} type]
        [{NL} typeConstraints]
        [{NL} functionBody]
        functionLiteral:
        lambdaLiteral
        | anonymousFunction
        objectLiteral:
        ['data']
        {NL}
        'object'
        [{NL} ':' {NL} delegationSpecifiers {NL}]
        [{NL} classBody]
        thisExpression:
        'this'
        | THIS_AT
        superExpression:
        ('super' ['<' {NL} type {NL} '>'] [AT_NO_WS simpleIdentifier])
        | SUPER_AT
        ifExpression:
        'if'
        {NL}
        '('
        {NL}
        expression
        {NL}
        ')'
        {NL}
        (controlStructureBody | ([controlStructureBody] {NL} [';'] {NL} 'else' {NL} (controlStructureBody | ';')) | ';')
        whenSubject:
        '(' [{annotation} {NL} 'val' {NL} variableDeclaration {NL} '=' {NL}] expression ')'
        whenExpression:
        'when'
        {NL}
        [whenSubject]
        {NL}
        '{'
        {NL}
        {whenEntry {NL}}
        {NL}
        '}'
        whenEntry:
        (whenCondition {{NL} ',' {NL} whenCondition} [{NL} ','] {NL} '->' {NL} controlStructureBody [semi])
        | ('else' {NL} '->' {NL} controlStructureBody [semi])
        whenCondition:
        expression
        | rangeTest
        | typeTest
        rangeTest:
        inOperator {NL} expression
        typeTest:
        isOperator {NL} type
        tryExpression:
        'try' {NL} block ((({NL} catchBlock {{NL} catchBlock}) [{NL} finallyBlock]) | ({NL} finallyBlock))
        catchBlock:
        'catch'
        {NL}
        '('
        {annotation}
        simpleIdentifier
        ':'
        type
        [{NL} ',']
        ')'
        {NL}
        block
        finallyBlock:
        'finally' {NL} block
        jumpExpression:
        ('throw' {NL} expression)
        | (('return' | RETURN_AT) [expression])
        | 'continue'
        | CONTINUE_AT
        | 'break'
        | BREAK_AT
        callableReference:
        [receiverType] '::' {NL} (simpleIdentifier | 'class')
        assignmentAndOperator:
        '+='
        | '-='
        | '*='
        | '/='
        | '%='
        equalityOperator:
        '!='
        | '!=='
        | '=='
        | '==='
        comparisonOperator:
        '<'
        | '>'
        | '<='
        | '>='
        inOperator:
        'in'
        | NOT_IN
        isOperator:
        'is'
        | NOT_IS
        additiveOperator:
        '+'
        | '-'
        multiplicativeOperator:
        '*'
        | '/'
        | '%'
        asOperator:
        'as'
        | 'as?'
        prefixUnaryOperator:
        '++'
        | '--'
        | '-'
        | '+'
        | excl
        postfixUnaryOperator:
        '++'
        | '--'
        | ('!' excl)
        excl:
        '!'
        | EXCL_WS
        memberAccessOperator:
        ({NL} '.')
        | ({NL} safeNav)
        | '::'
        safeNav:
        QUEST_NO_WS '.'
        modifiers:
        annotation | modifier {annotation | modifier}
        parameterModifiers:
        annotation | parameterModifier {annotation | parameterModifier}
        modifier:
        (classModifier | memberModifier | visibilityModifier | functionModifier | propertyModifier | inheritanceModifier | parameterModifier | platformModifier) {NL}
        typeModifiers:
        typeModifier {typeModifier}
        typeModifier:
        annotation
        | ('suspend' {NL})
        classModifier:
        'enum'
        | 'sealed'
        | 'annotation'
        | 'data'
        | 'inner'
        | 'value'
        memberModifier:
        'override'
        | 'lateinit'
        visibilityModifier:
        'public'
        | 'private'
        | 'internal'
        | 'protected'
        varianceModifier:
        'in'
        | 'out'
        typeParameterModifiers:
        typeParameterModifier {typeParameterModifier}
        typeParameterModifier:
        (reificationModifier {NL})
        | (varianceModifier {NL})
        | annotation
        functionModifier:
        'tailrec'
        | 'operator'
        | 'infix'
        | 'inline'
        | 'external'
        | 'suspend'
        propertyModifier:
        'const'
        inheritanceModifier:
        'abstract'
        | 'final'
        | 'open'
        parameterModifier:
        'vararg'
        | 'noinline'
        | 'crossinline'
        reificationModifier:
        'reified'
        platformModifier:
        'expect'
        | 'actual'
        annotation:
        (singleAnnotation | multiAnnotation) {NL}
        singleAnnotation:
        ((annotationUseSiteTarget {NL}) | AT_NO_WS | AT_PRE_WS) unescapedAnnotation
        multiAnnotation:
        ((annotationUseSiteTarget {NL}) | AT_NO_WS | AT_PRE_WS) '[' (unescapedAnnotation {unescapedAnnotation}) ']'
        annotationUseSiteTarget:
        (AT_NO_WS | AT_PRE_WS) ('field' | 'property' | 'get' | 'set' | 'receiver' | 'param' | 'setparam' | 'delegate') {NL} ':'
        unescapedAnnotation:
        constructorInvocation
        | userType
        simpleIdentifier:
        Identifier
        | 'abstract'
        | 'annotation'
        | 'by'
        | 'catch'
        | 'companion'
        | 'constructor'
        | 'crossinline'
        | 'data'
        | 'dynamic'
        | 'enum'
        | 'external'
        | 'final'
        | 'finally'
        | 'get'
        | 'import'
        | 'infix'
        | 'init'
        | 'inline'
        | 'inner'
        | 'internal'
        | 'lateinit'
        | 'noinline'
        | 'open'
        | 'operator'
        | 'out'
        | 'override'
        | 'private'
        | 'protected'
        | 'public'
        | 'reified'
        | 'sealed'
        | 'tailrec'
        | 'set'
        | 'vararg'
        | 'where'
        | 'field'
        | 'property'
        | 'receiver'
        | 'param'
        | 'setparam'
        | 'delegate'
        | 'file'
        | 'expect'
        | 'actual'
        | 'const'
        | 'suspend'
        | 'value'
        identifier:
        simpleIdentifier {{NL} '.' simpleIdentifier}
    `;
}

main();
