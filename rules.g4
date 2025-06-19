parenthesizedUserType: LPAREN (userType | parenthesizedUserType) RPAREN
parenthesizedType: LPAREN type RPAREN
functionTypeParameters: LPAREN (parameter | type)? (COMMA (parameter | type))* (COMMA)? RPAREN


definitelyNonNullableType: typeModifiers? (userType | parenthesizedUserType) NL* AMP NL* typeModifiers? (userType | parenthesizedUserType)
type: typeModifiers? (functionType | parenthesizedType | nullableType | typeReference | definitelyNonNullableType)
receiverType: typeModifiers? (parenthesizedType | nullableType | typeReference)

definitelyNonNullableTypeNoModifier: (userType | parenthesizedUserType) NL* AMP NL* typeModifiers? (userType | parenthesizedUserType)
typeNoModifier:                      (functionType | parenthesizedType | nullableType | typeReference | definitelyNonNullableType)
receiverTypeNoModifier:                             (parenthesizedType | nullableType | typeReference)


typeReference: userType | DYNAMIC
nullableType: (typeReference | parenthesizedType) NL* quest+
functionType: (receiverType DOT)? functionTypeParameters ARROW type

// startsWith AT_NO_WS
typeModifier: annotation | SUSPEND NL*

// startsWith SimpleIdentifier
userType: ...
parameter: simpleIdentifier NL* COLON NL* type

type2: typeModifiers? (((receiverTypeNoModifier)(DOT functionTypeParameters ARROW type)?) | definitelyNonNullableTypeNoModifier)

------------------------------

Assert that the following is true
(userType | parenthesizedUserType) is in receiverTypeNoModifier because
    1. typeReference contains userType
    2. parenthesizedType can resolve parenthesizedUserType
    3. Both typeReference and parenthesizedType is in receiverTypeNoModifier
functionTypeParameters contains both parenthesizedType & parenthesizedUserType
    1. when it has zero childs, then it is itself
    2. When it has only one direct child, and that child is `type` then it is parenthesizedType
    2. When it has only one direct child, and that child is `userType | parenthesizedUserType` then it is parenthesizedType
Also we can reduce 
receiverTypeNoModifier:                             (parenthesizedType | nullableType | typeReference)
to
receiverTypeNoModifier2:                             (typeReference | parenthesizedType) (NL* quest+)?
Then:
type3: typeModifiers? (
        |   (functionTypeParameters ARROW type3)
        |   (receiverTypeNoModifier2 (
                    | (DOT functionTypeParameters ARROW type3) 
                    | (NL* AMP NL* typeModifiers? (userType | parenthesizedUserType))
                )
            )
)?
------------------------------

type4: typeModifiers? (
        |   (functionTypeParameters ARROW type4)
        |   (((typeReference | functionTypeParameters) (NL* quest+)?) 
              (
                | (DOT functionTypeParameters ARROW type4) 
                | (NL* AMP NL* typeModifiers? (userType | parenthesizedUserType))
              )
            )
)?
-------------------------------