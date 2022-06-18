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
    :   CONST Identifier ASSIGN constValue;

constValue
    :   IntegerLiteral
    |   FloatLiteral
    |   StringLiteral;

indexAssign
    :   expression L_BRACKET expression R_BRACKET ASSIGN expression;

enumStatement
    :   ENUM Identifier L_CURLY_BRACKET enumerations* R_CURLY_BRACKET;

enumerations
    :   Identifier ASSIGN constValue COMMA?;

memberdeclare
    :   Identifier ASSIGN expression SEMICOLON?
    |   L_BRACKET expression R_BRACKET ASSIGN expression SEMICOLON?
    |   FUNCTION funcname functionDeclareEnd
    |   CONSTRUCTOR functionDeclareEnd;

inits
    :   Identifier (ASSIGN expression)? (COMMA inits)?;

args
    :   arg (COMMA arg)* (COMMA VARARGS)?
    |   VARARGS;

arg
    :   Identifier (ASSIGN expression)?;

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
    |   tableConstruction
    |   L_PAREN expression R_PAREN
    |   derefExpression
    |   operations;

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
    :   derefExpression ASSIGN expression;

newslot
    :   derefExpression NEWSLOT expression;

tableConstruction
    :   L_CURLY_BRACKET tableSlot* R_CURLY_BRACKET;

tableSlot
    :   (basicTableSlot | arrayTableSlot | jsonTableSlot | (FUNCTION funcname functionDeclareEnd)) COMMA?;

basicTableSlot
    :   Identifier ASSIGN expression;

arrayTableSlot
    :   L_BRACKET expression R_BRACKET ASSIGN expression;

jsonTableSlot
    :   DOUBLE_QUOTE Identifier DOUBLE_QUOTE COLON expression;

operations
    :   unaryMinusOp
    |   bitwiseNotOp
    |   logicalNotOp
    |   typeofOp
    |   preincrementOp
    |   postincrementOp
    |   predecrementOp
    |   postdecrementOp

    |   divideOp
    |   multiplyOp
    |   moduloOp

    |   addOp
    |   subOp

    |   leftShiftOp
    |   rightShiftOp
    |   rightUnsignedShiftOp

    |   lessOp
    |   lessEqualOp
    |   greaterOp
    |   greaterEqualOp

    |   equalOp
    |   notEqualOp
    |   compareOp

    |   bitwiseAndOp

    |   bitwiseXOROp

    |   bitwiseOrOp

    |   logicalAndOp
    |   inOp

    |   logicalOrOp

    |   ternaryOp

    |   addEqualOp
    |   assignOp
    |   subEqualOp

    |   commaOp;

unaryMinusOp
    :   MINUS expression;

bitwiseNotOp
    :   BITWISE_NOT expression;

logicalNotOp
    :   LOGICAL_NOT expression;

typeofOp
    :   TYPEOF expression;

preincrementOp
    :   INCREMENT expression;

postincrementOp
    :   derefExpression INCREMENT;

predecrementOp
    :   DECREMENT expression;

postdecrementOp
    :   derefExpression DECREMENT;

divideOp
    :   derefExpression DIVIDE expression;

multiplyOp
    :   derefExpression MULTIPLY expression;

moduloOp
    :   derefExpression MODULO expression;

addOp
    :   derefExpression PLUS expression;

subOp
    :   derefExpression MINUS expression;

leftShiftOp
    :   derefExpression LEFT_SHIFT expression;

rightShiftOp
    :   derefExpression RIGHT_SHIFT expression;

rightUnsignedShiftOp
    :   derefExpression RIGHT_UNSIGNED_SHIFT expression;

lessOp
    :   derefExpression LESS_THAN expression;

lessEqualOp
    :   derefExpression LESS_OR_EQUAL expression;

greaterOp
    :   derefExpression GREATER_THAN expression;

greaterEqualOp
    :   derefExpression GREATER_OR_EQUAL expression;

equalOp
    :   derefExpression EQUAL expression;

notEqualOp
    :   derefExpression NOT_EQUAL expression;

compareOp
    :   derefExpression COMPARE expression;

bitwiseAndOp
    :   derefExpression BITWISE_AND expression;

bitwiseXOROp
    :   derefExpression BITWISE_XOR expression;

bitwiseOrOp
    :   derefExpression BITWISE_OR expression;

logicalAndOp
    :   derefExpression LOGICAL_AND expression;

inOp
    :   derefExpression IN expression;

logicalOrOp
    :   derefExpression LOGICAL_OR expression;

ternaryOp
    :   derefExpression QUESTION expression COLON expression;

addEqualOp
    :   derefExpression ASSIGN_ADD expression;

assignOp
    :   derefExpression ASSIGN expression;

subEqualOp
    :   derefExpression ASSIGN_SUB expression;

commaOp
    :   derefExpression COMMA expression;


expressionList
    :   expression (COMMA expression)*;

derefExpression
    :   Identifier
    |   derefExpression DOT Identifier
    |   derefExpression L_PAREN expressionList? R_PAREN
    |   derefExpression L_BRACKET expression R_BRACKET
    |   SCOPE Identifier
    |   literal;