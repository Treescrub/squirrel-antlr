parser grammar SquirrelParser;

options {
    tokenVocab=SquirrelLexer;
}

program
    :   statements EOF;

statements
    :   statement*;

statement
    :   statementBody SEMICOLON?;

statementBody
    :   statementBlock
    |   ifStatement
    |   whileStatement
    |   doWhileStatement
    |   switchStatement
    |   forStatement
    |   foreachStatement
    |   BREAK
    |   CONTINUE
    |   RETURN expression?
    |   YIELD expression?
    |   localDeclare
    |   FUNCTION funcname functionDeclareEnd
    |   classDeclare
    |   tryCatch
    |   THROW expression
    |   constStatement
    |   enumStatement
    |   indexAssign
    |   expression;

statementBlock
    :   L_CURLY_BRACKET statements R_CURLY_BRACKET;

ifStatement
    :   IF L_PAREN expression R_PAREN statement (ELSE statement)?;

whileStatement
    :   WHILE L_PAREN expression R_PAREN statement;

doWhileStatement
    :   DO statement WHILE L_PAREN expression R_PAREN;

switchStatement
    :   SWITCH L_PAREN expression R_PAREN L_CURLY_BRACKET switchCase* defaultCase? R_CURLY_BRACKET;

switchCase
    :   CASE literal COLON statements;

defaultCase
    :   DEFAULT COLON statements;

forStatement
    :   FOR L_PAREN expression SEMICOLON expression SEMICOLON expression R_PAREN statement;

foreachStatement
    :   FOREACH L_PAREN foreachVar IN expression R_PAREN statement;

foreachVar
    :   (Identifier COMMA)? Identifier;

localDeclare
    :   LOCAL inits;

classDeclare
    :   CLASS derefExpression (EXTENDS derefExpression)? L_CURLY_BRACKET memberdeclare* R_CURLY_BRACKET;

tryCatch
    :   TRY statement CATCH L_PAREN Identifier R_PAREN statement;

constStatement
    :   CONST Identifier EQUALS constValue;

constValue
    :   IntegerLiteral
    |   FloatLiteral
    |   StringLiteral;

indexAssign
    :   expression L_BRACKET expression R_BRACKET EQUALS expression;

enumStatement
    :   ENUM Identifier L_CURLY_BRACKET enumerations* R_CURLY_BRACKET;

enumerations
    :   Identifier EQUALS constValue COMMA?;

memberdeclare
    :   Identifier EQUALS expression SEMICOLON?
    |   L_BRACKET expression R_BRACKET EQUALS expression SEMICOLON?
    |   FUNCTION funcname functionDeclareEnd
    |   CONSTRUCTOR functionDeclareEnd;

inits
    :   Identifier (EQUALS expression)? (COMMA inits)?;

args
    :   arg (COMMA arg)* (COMMA VARARGS)?
    |   VARARGS;

arg
    :   Identifier (EQUALS expression)?;

funcname
    :   Identifier (SCOPE Identifier)*;

literal
    :   IntegerLiteral
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
    |   L_PAREN expression R_PAREN
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
    :   FUNCTION Identifier functionDeclareEnd;

anonymousFunction
    :   FUNCTION functionDeclareEnd;

functionDeclareEnd
    :   L_PAREN args? R_PAREN statement;

lambda
    :   AT L_PAREN args? R_PAREN expression;

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
    :   L_CURLY_BRACKET tableSlot* R_CURLY_BRACKET;

tableSlot
    :   (basicTableSlot | arrayTableSlot | jsonTableSlot | (FUNCTION funcname functionDeclareEnd)) COMMA?;

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
    |   derefExpression DOT Identifier
    |   derefExpression L_PAREN expressionList? R_PAREN
    |   derefExpression L_BRACKET expression R_BRACKET
    |   SCOPE Identifier
    |   literal;