import ../data_structures/ast
import ../data_structures/token
import ../data_structures/position

import ../utils/iterutils

import ../error

import std/tables
import std/strformat

type
  Parser* = object
    ## Parser is the main component of the Kc compiler pipeline.
    ## It takes a sequence of tokens and produces an abstract syntax tree (AST).
    ## The parser is responsible for recognizing and interpreting the structure of the source code.
    ## It uses a recursive descent parser to parse the tokens and construct the AST.
    tokens*: seq[Token]
    pos*: int
    reporter*: ErrorReporter

  ParserError* = object of CatchableError
    ## Parser Error Just wors as a smoke signal to return to the statement caller

proc newParser*(reporter: ErrorReporter): Parser =
  ## newParser creates a new Parser instance with the specified reporter.
  Parser(tokens: @[], pos: 0, reporter: reporter)

# Forward declarations
proc parseExpression(parser: var Parser): Expression

proc parseStatement(parser: var Parser): Statement

# Utility functions
proc atEnd(parser: var Parser): bool {.inline.} =
  ## atEnd returns true if the parser has reached the end of the token stream.
  ## It is used to check if the parser has reached the end of the input.
  parser.pos >= parser.tokens.len

proc peek(parser: var Parser, offset: int = 0): Token =
  ## peek returns the token at the specified offset ahead of the current position.
  ## It is used to look ahead in the token stream without consuming the token.
  if parser.pos + offset >= parser.tokens.len:
    return Token(kind: EOF)
  parser.tokens[parser.pos]

proc reportError(parser: var Parser, message: string) {.noReturn.} =
  parser.reporter.reportError(SyntaxError, message, parser.peek().pos)
  raise (ref ParserError)()

proc advance(parser: var Parser) =
  ## advance moves the parser's position forward by one token.
  ## It is used to consume tokens and move the parser's position forward.
  parser.pos += 1

proc check(parser: var Parser, kinds: varargs[TK]): bool {.inline.} =
  ## check checks if the current token is of the specified kind.
  ## It returns true and advances the parser's position if the token is of the specified kind.
  ## Otherwise, it returns false.
  for kind in kinds:
    if parser.peek().kind == kind:
      parser.advance()
      return true
  return false

proc expect(parser: var Parser, kind: TK, message: string) {.inline.} =
  ## expect checks if the current token is of the specified kind.
  ## If it is not, it reports an error with the given message.
  ## Otherwise, it advances the parser's position.
  if parser.peek().kind != kind:
    parser.reportError(message)
  parser.advance()

proc skipNewLines(parser: var Parser) =
  ## NewLines are usefull on few places so this function skips them.
  while parser.peek().kind == TK.NewLine:
    parser.advance()

# Parser functions
proc parse*(parser: var Parser, tokens: seq[Token]): seq[Statement] =
  ## parse takes a sequence of tokens and returns a sequence of statements.
  ## It uses a recursive descent parser to parse the tokens and construct the AST.
  # Reset the parser's state
  parser.tokens = tokens
  parser.pos = 0

  var statements: seq[Statement]
  while not parser.atEnd():
    try:
      let statement = parseStatement(parser)
      statements.add(statement)
    except ParserError:
      while not parser.atEnd() and parser.peek().kind != TK.EOF and parser.peek().kind != TK.NewLine:
        # Skip tokens until we reach the end of the file or a new line
        parser.advance()
  return statements

proc parseIdentifier(parser: var Parser, name: string, pos: FilePosition): Expression =
  ## parseIdentifier parses an identifier expression.
  parser.advance()
  return newIdentifier(name, pos)

proc parseLiteral[T](parser: var Parser, value: T, pos: FilePosition): Expression =
  ## parseLiteral parses a literal expression.
  parser.advance()
  return newLiteral(value, pos)

proc parseGroup(parser: var Parser): Expression =
  ## parseGroup parses a group expression.
  parser.advance()
  let expression = parseExpression(parser)
  if parser.peek().kind != TK.RightParen:
    parser.reportError("Expected ')' after expression")
  parser.advance()
  return expression

proc parseStructLiteral(parser: var Parser, startPos: FilePosition): Expression =
  ## parseStructLiteral parses a struct literal expression.
  parser.advance()
  var fields: Table[Identifier, Expression]
  if parser.peek().kind == TK.RightCurly:
    parser.advance()
    return newStructLiteral(fields, startPos)
  doWhile check(parser, TK.Comma):
    let current = parser.peek()
    let fieldName: string =
      case current.kind
      of TK.Identifier:
        current.ident
      else:
        parser.reportError("Expected field name")
    let ident = Identifier(name: fieldName, pos: current.pos)
    parser.advance()
    expect(parser, TK.Colon, "Expected ':' after field name")
    fields[ident] = parseExpression(parser)

  if parser.peek().kind != TK.RightCurly:
    parser.reporter.reportError(
      SyntaxError, "Expected '}' after struct fields", parser.peek().pos
    )

  parser.advance()
  return newStructLiteral(fields, startPos)

proc parseArrayLiteral(parser: var Parser, startPos: FilePosition): Expression =
  ## parseArrayLiteral parses an array literal expression.
  parser.advance()
  var elements: seq[Expression]
  if parser.peek().kind == TK.RightBracket:
    parser.advance()
    return newArrayLiteral(elements, startPos)
  doWhile check(parser, TK.Comma):
    let element = parseExpression(parser)
    elements.add(element)
  if parser.peek().kind != TK.RightBracket:
    parser.reportError("Expected ']' after array elements but found " & parser.peek().lexeme)
  parser.advance()
  return newArrayLiteral(elements, startPos)

proc parseMemberAccess(parser: var Parser, objectExpr: Expression): Expression =
  ## parseMemberAccess parses a member access expression.
  parser.advance()
  let ident = parser.peek()
  let member =
    case ident.kind
    of TK.Identifier:
      ident.ident
    else:
      parser.reportError("Expected member name")
  parser.advance()
  return newMemberAccess(objectExpr, member, parser.peek().pos)

proc parseCall(parser: var Parser, callee: Expression): Expression =
  ## parseCall parses a call expression.
  parser.advance()
  var arguments: seq[Expression]
  if parser.peek().kind == TK.RightParen:
    parser.advance()
    return newCall(callee, arguments, parser.peek().pos)
  doWhile check(parser, TK.Comma):
    let argument = parseExpression(parser)
    arguments.add(argument)
  if parser.peek().kind != TK.RightParen:
    parser.reportError("Expected ')' after arguments")
  parser.advance()
  return newCall(callee, arguments, parser.peek().pos)

proc parseArrayAccess(parser: var Parser, objectExpr: Expression): Expression =
  ## parseArrayAccess parses an array access expression.
  parser.advance()
  let index = parseExpression(parser)
  expect(parser, TK.RightBracket, "Expected ']' after array index")
  result = newArrayAccess(objectExpr, index, parser.peek().pos)

proc parsePrimary(parser: var Parser): Expression =
  ## parsePrimary parses the primary expression of the Kc language.
  ## It handles the parsing of literals, identifiers, parentheses, and member access expressions.

  let token = parser.peek()
  case token.kind
  of TK.Identifier:
    result = parseIdentifier(parser, token.ident, token.pos)
  of TK.Integer:
    result = parseLiteral(parser, token.intVal, token.pos)
  of TK.Float:
    result = parseLiteral(parser, token.floatVal, token.pos)
  of TK.True:
    result = parseLiteral(parser, true, token.pos)
  of TK.False:
    result = parseLiteral(parser, false, token.pos)
  of TK.String:
    result = parseLiteral(parser, token.strVal, token.pos)
  of TK.LeftBracket:
    result = parseArrayLiteral(parser, token.pos)
  of TK.LeftCurly:
    result = parseStructLiteral(parser, token.pos)
  of TK.LeftParen:
    result = parseGroup(parser)
  else:
    reportError(parser, &"Unexpected token {token.lexeme} on primary expression")
  while {TK.Dot, TK.LeftParen, TK.LeftBracket}.contains(parser.peek().kind):
    case parser.peek().kind
    of TK.Dot:
      result = parseMemberAccess(parser, result)
    of TK.LeftParen:
      result = parseCall(parser, result)
    of TK.LeftBracket:
      result = parseArrayAccess(parser, result)
    else:
      discard

proc parseUnary(parser: var Parser): Expression =
  ## parseUnary parses the unary expression of the Kc language.
  ## It handles the parsing of unary expressions, such as negation and logical NOT.
  let token = parser.peek()
  case token.kind
  of TK.Bang:
    parser.advance()
    return newUnaryExpr(Not, parseUnary(parser), token.pos)
  of TK.Minus:
    parser.advance()
    return newUnaryExpr(Minus, parseUnary(parser), token.pos)
  else:
    return parsePrimary(parser)

proc parseFactor(parser: var Parser): Expression =
  ## parseFactor parses the factor expression of the Kc language.
  ## It handles the parsing of factor expressions, such as multiplication, modulus, and division.
  result = parseUnary(parser)
  var token = parser.peek()
  while token.kind == TK.Star or token.kind == TK.Slash or token.kind == TK.Percent:
    parser.advance()
    case token.kind
    of TK.Star:
      result = newBinaryExpr(Multiply, result, parseUnary(parser), token.pos)
    of TK.Slash:
      result = newBinaryExpr(Divide, result, parseUnary(parser), token.pos)
    of TK.Percent:
      result = newBinaryExpr(Modulus, result, parseUnary(parser), token.pos)
    else: discard
    token = parser.peek()

proc parseTerm(parser: var Parser): Expression =
  ## parseTerm parses the term expression of the Kc language.
  ## It handles the parsing of terms, such as addition and subtraction.
  result = parseFactor(parser)
  var token = parser.peek()
  while token.kind == TK.Plus or token.kind == TK.Minus:
    parser.advance()
    case token.kind
    of TK.Plus:
      result = newBinaryExpr(Plus, result, parseFactor(parser), token.pos)
    of TK.Minus:
      result = newBinaryExpr(Minus, result, parseFactor(parser), token.pos)
    else: discard
    token = parser.peek()

proc parseComparison(parser: var Parser): Expression =
  ## parseComparison parses the comparison expression of the Kc language.
  ## It handles the parsing of comparison expressions, such as less than/greater than.
  result = parseTerm(parser)
  var token = parser.peek()
  while token.kind == TK.Less or token.kind == TK.LessEqual or token.kind == TK.Greater or token.kind == TK.GreaterEqual:
    parser.advance()
    case token.kind
    of TK.Less:
      result = newBinaryExpr(Less, result, parseTerm(parser), token.pos)
    of TK.LessEqual:
      result = newBinaryExpr(LessEqual, result, parseTerm(parser), token.pos)
    of TK.Greater:
      result = newBinaryExpr(Greater, result, parseTerm(parser), token.pos)
    of TK.GreaterEqual:
      result = newBinaryExpr(GreaterEqual, result, parseTerm(parser), token.pos)
    else: discard
    token = parser.peek()

proc parseEquality(parser: var Parser): Expression =
  ## parseEquality parses the equality expression of the Kc language.
  ## It handles the parsing of equality expressions, such as equal to/not equal to.
  result = parseComparison(parser)
  var token = parser.peek()
  while token.kind == TK.EqualEqual or token.kind == TK.NotEqual:
    parser.advance()
    case token.kind
    of TK.EqualEqual:
      result = newBinaryExpr(EqualEqual, result, parseComparison(parser), token.pos)
    of TK.NotEqual:
      result = newBinaryExpr(NotEqual, result, parseComparison(parser), token.pos)
    else: discard
    token = parser.peek()

proc parseAnd(parser: var Parser): Expression =
  ## parseAnd parses the and expression of the Kc language.
  ## It handles the parsing of and expressions, such as logical and.
  result = parseEquality(parser)
  var token = parser.peek()
  while token.kind == TK.And:
    parser.advance()
    result = newBinaryExpr(And, result, parseEquality(parser), token.pos)
    token = parser.peek()

proc parseOr(parser: var Parser): Expression =
  ## parseOr parses the or expression of the Kc language.
  ## It handles the parsing of or expressions, such as logical or.
  result = parseAnd(parser)
  var token = parser.peek()
  while token.kind == TK.Or:
    parser.advance()
    result = newBinaryExpr(Or, result, parseAnd(parser), token.pos)
    token = parser.peek()

proc parseExpression(parser: var Parser): Expression =
  ## parseExpression parses the expression of the Kc language.
  result = parseOr(parser)

proc parseBlock(parser: var Parser): Statement =
  ## parseBlock parses a block statement.
  var statements: seq[Statement]
  parser.skipNewLines()
  while parser.peek().kind != TK.RightCurly:
    try:
      let statement = parseStatement(parser)
      statements.add(statement)
      parser.skipNewLines()
    except ParserError:
      while not parser.atEnd() and parser.peek().kind != TK.EOF and parser.peek().kind != TK.NewLine:
        # Skip tokens until we reach the end of the file or a new line
        parser.advance()
  parser.advance()
  return newBlock(statements, parser.peek().pos)

proc parseType(parser: var Parser): Type =
  # parseType parses the type of a variable or function parameter.
  let token = parser.peek()
  case token.kind
  of TK.Identifier:
    result = newType(token.ident, token.pos)
    parser.advance()
  of TK.IntType:
    result = newType(TypeKind.IntType, token.pos)
    parser.advance()
  of TK.FloatType:
    result = newType(TypeKind.FloatType, token.pos)
    parser.advance()
  of TK.BoolType:
    result = newType(TypeKind.BoolType, token.pos)
    parser.advance()
  of TK.String:
    result = newType(TypeKind.StringType, token.pos)
    parser.advance()
  of TK.Char:
    result = newType(TypeKind.CharType, token.pos)
    parser.advance()
  of TK.Array:
    parser.advance()
    expect(parser, TK.LeftBracket, "Expected '[' before array size")
    let tk = parser.peek()
    let size =
      case tk.kind
      of TK.Integer:
        tk.intVal
      else:
        parser.reportError("Expected integer literal")
    parser.advance()
    expect(parser, TK.Semicolon, "Expected ';' after array size")
    let elementType = parseType(parser)
    expect(parser, TK.RightBracket, "Expected ']' after array type")
    result = newArrayType(size, elementType, token.pos)
  of TK.Func:
    parser.advance()
    expect(parser, TK.LeftParen, "Expected '(' before function parameter list")
    var paramTypes: seq[Type]
    while parser.peek().kind != TK.RightParen:
      let paramType = parseType(parser)
      paramTypes.add(paramType)
      if parser.peek().kind == TK.Comma:
        parser.advance()
    expect(parser, TK.RightParen, "Expected ')' after function parameter list")
    expect(parser, TK.Colon, "Expected ':' after function parameter list")
    let returnType = parseType(parser)
    result = newFuctionType(paramTypes, returnType, token.pos)
  of NoneKeyword:
    result = newType(TypeKind.None, token.pos)
    parser.advance()
  else:
    parser.reportError("Expected type")

proc varDeclaration(parser: var Parser): Statement =
  ## varDeclaration parses a variable declaration statement.
  parser.advance()
  let ident = parser.peek()
  let name =
    case ident.kind
    of TK.Identifier:
      ident.ident
    else:
      parser.reportError("Expected variable name")
  parser.advance()
  var varType: Type = Type(kind: TypeKind.Infered, pos: ident.pos)
  if parser.peek().kind == TK.Colon:
    parser.advance()
    varType = parseType(parser)
  if varType.kind == TypeKind.None:
    parser.reportError("Variable type cannot be 'none'")
  expect(parser, TK.Equal, "Expected '=' after variable type")
  let initialValue = parseExpression(parser)
  result = newVarDecl(name, varType, initialValue, ident.pos)

proc funcDeclaration(parser: var Parser): Statement =
  ## funcDeclaration parses a function declaration statement.
  parser.advance()
  let ident = parser.peek()
  let name =
    case ident.kind
    of TK.Identifier:
      Identifier(name: ident.ident, pos: ident.pos)
    else:
      parser.reportError("Expected function name")
  parser.advance()
  expect(parser, TK.LeftParen, "Expected '(' after function name")

  var params: seq[(Identifier, Type)]
  var returnType: Type = Type(kind: TypeKind.None, pos: ident.pos)
  if parser.peek().kind == TK.RightParen:
    parser.advance()
  else:
    doWhile check(parser, TK.Comma):
      let current = parser.peek()
      let paramName =
        case current.kind
        of TK.Identifier:
          Identifier(name: current.ident, pos: current.pos)
        else:
          parser.reportError("Expected parameter name")
      parser.advance()
      expect(parser, TK.Colon, "Expected ':' after parameter name")
      let paramType = parseType(parser)
      params.add((paramName, paramType))
    expect(parser, TK.RightParen, "Expected ')' after function parameter list")
  if parser.peek().kind == TK.Colon:
    parser.advance()
    returnType = parseType(parser)
  expect(parser, TK.LeftCurly, "Expected '{' before function body")
  let body = parseBlock(parser)
  result = newFuncDecl(name, params, returnType, body, ident.pos)

proc parseStructDeclaration(parser: var Parser): Statement =
  ## parseStructDeclaration parses a struct declaration statement.
  parser.advance()
  let ident = parser.peek()
  let name =
    case ident.kind
    of TK.Identifier:
      Identifier(name: ident.ident, pos: ident.pos)
    else:
      parser.reportError("Expected struct name")
  parser.advance()
  expect(parser, TK.LeftCurly, "Expected '{' before struct field list")
  var fields: seq[Field]
  if parser.peek().kind == TK.RightCurly:
    parser.advance()
  else:
    skipNewLines(parser)
    doWhile check(parser, TK.Comma):
      let current = parser.peek()
      let fieldName =
        case current.kind
        of TK.Identifier:
          current.ident
        else:
          parser.reportError("Expected field name")
      parser.advance()
      expect(parser, TK.Colon, "Expected ':' after field name")
      let fieldType = parseType(parser)
      fields.add(Field(name: fieldName, typ: fieldType, pos: current.pos))
      skipNewLines(parser)
    expect(parser, TK.RightCurly, "Expected '}' after struct field list")
  result = newStructDecl(name, fields, ident.pos)


proc parseReturnStmt(parser: var Parser): Statement =
  ## parseReturnStmt parses a return statement.
  let pos = parser.peek().pos
  parser.advance()
  let value =
    if parser.peek().kind == TK.Semicolon or parser.peek().kind == TK.NewLine:
      noneExpression()
    else:
      parseExpression(parser)
  result = newReturnStmt(value, pos)

proc parseIfStmt(parser: var Parser): Statement =
  ## parseIfStmt parses an if statement.
  let pos = parser.peek().pos
  parser.advance()
  let condition = parseExpression(parser)
  expect(parser, TK.LeftCurly, "Expected '{' before if body")
  parser.skipNewLines()
  let thenBranch = parseBlock(parser)
  var elifBranches: seq[ElifStmt]
  parser.skipNewLines()
  while parser.peek().kind == TK.Elif:
    let pos = parser.peek().pos
    parser.advance()
    let elifCondition = parseExpression(parser)
    expect(parser, TK.LeftCurly, "Expected '{' before elif body")
    parser.skipNewLines()
    let elifBranch = parseBlock(parser)
    elifBranches.add(newElifStmt(elifCondition, elifBranch, pos))
    skipNewLines(parser)
  var elseBranch: ref Statement
  if parser.peek().kind == TK.Else:
    parser.advance()
    expect(parser, TK.LeftCurly, "Expected '{' before else body")
    elseBranch = parseBlock(parser).asRef()
  result = newIfStmt(condition, thenBranch, elifBranches, elseBranch, pos)

proc parseWhileStmt(parser: var Parser): Statement =
  raise newException(ValueError, "While statements are not supported yet")

proc parseForStmt(parser: var Parser): Statement =
  raise newException(ValueError, "For statements are not supported yet")

proc parseBreakStmt(parser: var Parser): Statement =
  ## parseBreakStmt parses a break statement.
  parser.advance()
  result = newBreakStmt(parser.peek().pos)

proc parseContinueStmt(parser: var Parser): Statement =
  ## parseContinueStmt parses a continue statement.
  parser.advance()
  result = newContinueStmt(parser.peek().pos)

proc parseStatement(parser: var Parser): Statement =
  ## parseStatement parses a statement.
  skipNewLines(parser) # Skip new lines
  let token = parser.peek()
  case token.kind
  of TK.Var:
    result = varDeclaration(parser)
  of TK.Func:
    result = funcDeclaration(parser)
  of TK.Struct:
    result = parseStructDeclaration(parser)
  of TK.If:
    result = parseIfStmt(parser)
  of TK.While:
    result = parseWhileStmt(parser)
  of TK.For:
    result = parseForStmt(parser)
  of TK.Break:
    result = parseBreakStmt(parser)
  of TK.Continue:
    result = parseContinueStmt(parser)
  of TK.Return:
    result = parseReturnStmt(parser)
  of TK.LeftCurly:
    parser.advance()
    result = parseBlock(parser)
  of TK.Else:
    parser.reportError("Unexpected 'else' keyword without a previous 'if' statement")
  else:
    # Handle assignments and expressions statements
    let expression = parseExpression(parser)
    if check(parser, TK.Equal):
      result = newAssign(expression, parseExpression(parser), parser.peek().pos)
    else:
      result = newExpressionStmt(expression, parser.peek().pos)
      if not check(parser, TK.Semicolon, TK.NewLine, TK.EOF):
        parser.reportError("Expected ';' or new line after expression")