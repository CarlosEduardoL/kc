# Execution Phases

THis first implementation of Kc is a toy language, so it does not have a compiler or virtual machine. 
However, the following is a high-level overview of the execution phases:

## **Lexer**

The lexer is responsible for breaking the source code into tokens.
It reads the source code character by character and produces a stream of tokens.

## **Parser**

The parser is responsible for analyzing the tokens and constructing an abstract syntax tree (AST).
It reads the tokens produced by the lexer and produces an AST.

## **Semantic Analyzer**

The semantic analyzer is responsible for checking the AST for errors and type checking.
It reads the AST produced by the parser and produces a type-checked AST.

## **Compiler**

The compiler is responsible for generating byte code from the type-checked AST.
It reads the type-checked AST produced by the semantic analyzer and produces byte code.

## **Virtual Machine**

The virtual machine is responsible for executing the byte code.
It reads the byte code produced by the compiler and executes the code.


Ideally, each phase should be independent and reusable, allowing for example dumping the AST or the byte code to a file. And include future optimizations.
So they will be implemented as separate modules.

2 meta-modules will be created to handle the phases:

- Data Structures: THis metamodule will containd the tokens, AST, and byte code.
- Execution Phases: This metamodule will contain the lexer, parser, semantic analyzer, compiler, and virtual machine.