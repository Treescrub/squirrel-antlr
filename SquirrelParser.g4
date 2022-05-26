parser grammar SquirrelParser;

options {
    tokenVocab=SquirrelLexer;
}

program
    :   statements? EOF;

statements
    :   statement (SEMICOLON | NEWLINE) statements?;

statement
    :   L_CURLY_BRACKET statements? R_CURLY_BRACKET
    |   IF L_PAREN expression R_PAREN statement (ELSE statement)?
    |   WHILE L_PAREN expression R_PAREN statement
    |   DO statement WHILE L_PAREN expression R_PAREN
    |   SWITCH L_PAREN expression R_PAREN L_CURLY_BRACKET (CASE literal COLON statements)* (DEFAULT COLON statements)? R_CURLY_BRACKET
    |   FOR L_PAREN expression SEMICOLON expression SEMICOLON expression R_PAREN statement
    |   FOREACH L_PAREN (Identifier COMMA)? Identifier IN expression R_PAREN statement
    |   BREAK
    |   CONTINUE
    |   RETURN expression?
    |   YIELD expression?
    |   LOCAL inits
    |   functionDeclareStart funcname functionDeclareEnd
    |   CLASS derefExpression (EXTENDS derefExpression)? L_CURLY_BRACKET memberdeclare* R_CURLY_BRACKET
    |   TRY statement CATCH L_PAREN Identifier R_PAREN statement
    |   THROW expression
    |   CONST Identifier EQUALS (IntegerLiteral | FloatLiteral | StringLiteral)
    |   ENUM Identifier L_CURLY_BRACKET enumerations* R_CURLY_BRACKET
    |   indexAssign
    |   expression;

indexAssign
    :   expression L_BRACKET expression R_BRACKET EQUALS expression;

enumerations
    :   Identifier EQUALS (IntegerLiteral | FloatLiteral | StringLiteral) COMMA?;

memberdeclare
    :   Identifier EQUALS expression SEMICOLON?
    |   L_BRACKET expression R_BRACKET EQUALS expression SEMICOLON?
    |   functionDeclareStart funcname functionDeclareEnd
    |   CONSTRUCTOR functionDeclareEnd;

inits
    :   Identifier (EQUALS expression)? (COMMA inits)?;

args
    :   Identifier (EQUALS expression)? (COMMA args)*;

funcname
    :   Identifier (SCOPE Identifier)*;

literal
    :
    |   IntegerLiteral
    |   FloatLiteral
    |   BooleanLiteral
    |   StringLiteral
    |   NULL
    |   THIS;

expression
    :   cloneExpression
    |   arrayConstruction
    |   deleteOperation
    |   functionCall
    |   functionDeclare
    |   anonymousFunction
    |   lambda
    |   assignment
    |   indexAccess
    |   LOCAL inits
    |   newslot
    |   ternaryOperation
    |   binaryOperation
    |   postfixOperation
    |   prefixOperation
    |   inOperation
    |   instanceofOperation
    |   typeofOperation
    |   tableConstruction
    |   expression L_PAREN args? R_PAREN
    |   IntegerLiteral
    |   FloatLiteral
    |   BooleanLiteral
    |   StringLiteral
    |   NULL
    |   THIS
    |   derefExpression;

indexAccess
    :   derefExpression L_BRACKET expression R_BRACKET;

cloneExpression
    :   CLONE expression;

arrayConstruction
    :   L_BRACKET expressionList? R_BRACKET;

deleteOperation
    :   DELETE derefExpression;

functionCall
    :   derefExpression L_PAREN expressionList? R_PAREN;

functionDeclare
    :   functionDeclareStart Identifier functionDeclareEnd;

anonymousFunction
    :   functionDeclareStart functionDeclareEnd;

functionDeclareStart
    :   FUNCTION;

functionDeclareEnd
    :   L_PAREN args? VARARGS? R_PAREN statement;

lambda
    :   AT L_PAREN args R_PAREN expression;

assignment
    :   derefExpression EQUALS expression;

newslot
    :   derefExpression NEWSLOT expression;

ternaryOperation
    :   derefExpression QUESTION expression COLON expression;

binaryOperation
    :   derefExpression operator expression;

postfixOperation
    :   derefExpression unaryPostfixOperator;

prefixOperation
    :   unaryPrefixOperator expression;

inOperation
    :   derefExpression IN expression;

instanceofOperation
    :   derefExpression INSTANCEOF expression;

typeofOperation
    :   TYPEOF expression;

tableConstruction
    :   L_CURLY_BRACKET (WS | NEWLINE)* tableSlot* R_CURLY_BRACKET;

tableSlot
    :   (basicTableSlot | arrayTableSlot | jsonTableSlot | (functionDeclareStart funcname functionDeclareEnd)) (COMMA | '\n')*;

basicTableSlot
    :   Identifier EQUALS expression;

arrayTableSlot
    :   L_BRACKET expression R_BRACKET EQUALS expression;

jsonTableSlot
    :   DOUBLE_QUOTE Identifier DOUBLE_QUOTE COLON expression;

unaryPrefixOperator
    :   INCREMENT
    |   DECREMENT
    |   TILDE
    |   EXCLAMATION
    |   PLUS
    |   MINUS;

unaryPostfixOperator
    :   INCREMENT
    |   DECREMENT;

operator
    :   PLUS
    |   MINUS
    |   STAR
    |   FORWARD_SLASH
    |   PERCENT
    |   PLUS EQUALS
    |   MINUS EQUALS
    |   STAR EQUALS
    |   FORWARD_SLASH EQUALS
    |   PERCENT EQUALS
    |   EQUALS EQUALS
    |   L_ARROW
    |   L_ARROW EQUALS
    |   R_ARROW
    |   R_ARROW EQUALS
    |   EXCLAMATION EQUALS
    |   L_ARROW EQUALS R_ARROW
    |   COMMA
    |   AND
    |   PIPE
    |   CARET
    |   L_ARROW L_ARROW
    |   R_ARROW R_ARROW
    |   R_ARROW R_ARROW R_ARROW
    |   AND AND
    |   PIPE PIPE;

expressionList
    :   expression (COMMA expression)*;

derefExpression
    :   Identifier
    |   L_PAREN expression R_PAREN
    |   derefExpression DOT Identifier
    |   derefExpression L_PAREN expressionList? R_PAREN
    |   derefExpression L_BRACKET expression R_BRACKET
    |   SCOPE Identifier
    |   IntegerLiteral
    |   FloatLiteral
    |   BooleanLiteral
    |   StringLiteral
    |   NULL
    |   THIS;