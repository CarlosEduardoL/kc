# Parsing Rules

This document outlines the parsing rules for the Kc programming language.

## Program Structure

```
program         → statement* ;
```

### Statements

```
statement       → declaration | assignment | expression | ifStmt | whileStmt | forStmt | returnStmt | breakStmt | continueStmt | block ;
declaration     → varDecl | funcDecl | structDecl ;
block           → "{" statement* "}" ;
```

#### Declarations

```
varDecl         → "var" IDENTIFIER ( ":" type )? "=" expression ;
funcDecl        → "func" IDENTIFIER "(" paramList ")" ":" type block ;
structDecl      → "struct" IDENTIFIER "{" structFieldList "}" ;
paramList       → IDENTIFIER ":" type ( "," IDENTIFIER ":" type )* ;
structFieldList → structField ( "," structField )* ;
structField     → IDENTIFIER ":" type ;
```

#### Assignment

```
assignment      → IDENTIFIER "=" expression ;
```

#### Control Structures

```
ifStmt          → "if" expression block ( "else" block )? ;
whileStmt       → "while" expression block ;
forStmt         → "for" IDENTIFIER "in" expression block ;
returnStmt      → "return" expression ;
breakStmt       → "break" ;
continueStmt    → "continue" ;
```

### Expressions

```
expression      → logicalOr ;
logicalOr       → logicalAnd ( "||" logicalAnd )* ;
logicalAnd      → equality ( "&&" equality )* ;
equality        → comparison ( ( "!=" | "==" ) comparison )* ;
comparison      → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term            → factor ( ( "-" | "+" ) factor )* ;
factor          → unary ( ( "/" | "*" | "%" ) unary )* ;
unary           → ( "!" | "-" ) unary | primary ;
primary         → literal | IDENTIFIER | "(" expression ")" | memberAccess | callExpr ;

```

#### Member Access and Function Calls

```
memberAccess    → primary "." IDENTIFIER ;
callExpr        → IDENTIFIER "(" ( expression ( "," expression )* )? ")" ;
```

#### Literals

```
literal            → NUMBER | BOOLEAN | STRING | arrayLiteral | structLiteral ;
arrayLiteral       → "[" ( expression ( "," expression )* )? "]" ;
structLiteral      → "{" ( structFieldLiteral ( "," structFieldLiteral )* )? "}" ;
structFieldLiteral → IDENTIFIER ":" expression ;

```

## Types

```
type            → "int" | "float" | "bool" | "char" | "string" | arrayType | structType | "none" ;
arrayType       → "array" "[" INTEGER ";" type "]" ;
structType      → "struct" IDENTIFIER ;
```

## Lexical Rules

```
IDENTIFIER      → letter ( letter | digit | "_" )* ;
NUMBER          → digit+ ( "." digit+ )? ;
BOOLEAN         → "true" | "false" ;
STRING          → "\"" character* "\"" ;
char            → "'" character "'" ;
character       → letter | digit | "_" | " " | "\n" | "\t" | "\r" ;
```