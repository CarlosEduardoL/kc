import ../data_structures/ast
import ../data_structures/token
import ../data_structures/position

import std/options

type 
  Parser* = object
    ## Parser is the main component of the Kc compiler pipeline.
    ## It takes a sequence of tokens and produces an abstract syntax tree (AST).
    ## The parser is responsible for recognizing and interpreting the structure of the source code.
    ## It uses a recursive descent parser to parse the tokens and construct the AST.
    tokens*: seq[Token]
    pos*: int
  ParserError* = object of CatchableError
    ## ParserError represents an error that occurred during parsing.
    ## It contains the position of the error in the source code.
    pos*: TokenPosition

# Forward declarations
proc parseExpression(parser: var Parser): Expression = discard
proc parseTerm(parser: var Parser): Expression = discard
proc parseFactor(parser: var Parser): Expression = discard
proc parseUnary(parser: var Parser): Expression = discard
proc parsePrimary(parser: var Parser): Expression

proc parseStatement(parser: var Parser): Statement = discard
proc parseDeclaration(parser: var Parser): Statement = discard
proc parseAssignment(parser: var Parser): Statement = discard
proc parseExpressionStatement(parser: var Parser): Statement = discard
proc parseIfStatement(parser: var Parser): Statement = discard
proc parseWhileStatement(parser: var Parser): Statement = discard
proc parseForStatement(parser: var Parser): Statement = discard
proc parseReturnStatement(parser: var Parser): Statement = discard
proc parseBreakStatement(parser: var Parser): Statement = discard
proc parseContinueStatement(parser: var Parser): Statement = discard
proc parseBlock(parser: var Parser): Statement = discard
proc parseVarDeclaration(parser: var Parser): Statement = discard
proc parseFuncDeclaration(parser: var Parser): Statement = discard
proc parseStructDeclaration(parser: var Parser): Statement = discard
proc parseParamList(parser: var Parser): seq[string] = discard
proc parseStructFieldList(parser: var Parser): seq[Field] = discard


proc parse*(tokens: seq[Token]): seq[Statement] =
  ## parse takes a sequence of tokens and returns a sequence of statements.
  ## It uses a recursive descent parser to parse the tokens and construct the AST.
  var parser = Parser(tokens: tokens)
  var statements: seq[Statement]
  while parser.pos < parser.tokens.len:
    let statement = parseStatement(parser)
    statements.add(statement)
  return statements

proc parsePrimary(parser: var Parser): Expression = 
  ## parsePrimary parses the primary expression of the Kc language.
  ## It handles the parsing of literals, identifiers, parentheses, and member access expressions.
  discard
  # case parser.tokens[parser.pos].kind:
