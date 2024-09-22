import position
import std/tables

type
  TypeKind* {.pure.} = enum
    ## Enum for all types in the Kc Expressions.
    IntType, FloatType, BoolType, StringType, CharType # Primitive types
    StructType, FunctionType, ArrayType                # User-defined types
    # special types
    None           # None type for no return functions, is not valid for assignments
    Infered        # Temporal Type for variables wihtout type annotation
    Defined        # Temporal type that holds the identifier of a type, should be replaced by the real type
    UndifinedType  # Temporal Type for every expression before type inference and Semantic analysis

  Field* = object
    ## Struct for fields in user-defined types
    pos*: FilePosition
    name*: string
    typ*: Type

  StructDescriptor = object
    ## Struct for struct types
    structName*: string
    fields*: seq[Field]

  ArrayDescriptor = object
    ## Struct for array types
    elementType*: TypeRef
    size*: int

  FunctionDescriptor = object
    ## Struct for function types
    paramTypes*: seq[Type]
    returnType*: TypeRef

  Type* = object
    ## Type is a composable type that can be used to represent all types in the Kc AST.
    pos*: FilePosition
    case kind*: TypeKind
      of IntType, FloatType, BoolType, StringType, CharType, UndifinedType, Infered, None: discard
      of StructType:
        structInfo*: StructDescriptor
      of ArrayType:
        arrayInfo*: ArrayDescriptor
      of FunctionType:
        funcInfo*: FunctionDescriptor
      of Defined:
        name*: string
  
  TypeRef = ref Type
    ## TypeRef is a reference to a Type object.

  OperatorKind* = enum
    ## Enum for all operators in the Kc Expressions.
    ## Note: We can reuse the token enum for operators, but we need to define our own enum to isolate the compile phases.
    Plus, Minus, Multiply,
    Divide, Modulus, EqualEqual,
    NotEqual, Less, LessEqual, Greater, 
    GreaterEqual, And, Or, Not

  ExpressionKind* {.pure.} = enum
    ## Enum for all expressions in the Kc AST.
    IntLiteral, FloatLiteral, BoolLiteral, StringLiteral, CharLiteral, StructLiteral, ArrayLiteral, NoneLiteral
    Identifier, GroupExpression, CallExpression, MemberAccessExpression, ArrayAccessExpression

  ArrayAccess = object
    ## Struct for array access expressions
    objectExpr*: Expression
    index*: Expression

  CallExpr = object
    ## Struct for function call expressions
    callee*: Expression
    arguments*: seq[Expression]

  MemberAccess = object
    ## Struct for member access expressions
    objectExpr*: Expression
    member*: string

  Identifier* = object
    ## Struct for identifier expressions
    name*: string
    pos*: FilePosition

  Expression* = ref object
    ## Expression is the base type for all expressions in the Kc AST.
    pos*: FilePosition
    typeChecked*: bool
    nodeType*: Type
    case kind*: ExpressionKind
      of IntLiteral:
        intVal*: int
      of FloatLiteral:
        floatVal*: float
      of BoolLiteral:
        boolVal*: bool
      of StringLiteral:
        strVal*: string
      of CharLiteral:
        charVal*: char
      of StructLiteral:
        structVal*: Table[Identifier, Expression]
      of ArrayLiteral:
        arrayLiteral*: seq[Expression]
      of NoneLiteral: discard
      of Identifier:
        name: string
      of GroupExpression:
        expr*: Expression
      of CallExpression:
        callExpr*: CallExpr
      of MemberAccessExpression:
        memberAccess*: MemberAccess
      of ArrayAccessExpression:
        arrayAccess*: ArrayAccess

  StatementKind* = enum
    ## StatementKind represents the different types of statements in the Kc AST.
    VarDeclaration, Assignment, IfStatement, WhileStatement, ReturnStatement, BlockStatement, FunctionDeclaration
    StructDeclaration, ExpressionStatement, BreakStatement, ContinueStatement

  VarDecl = object
    ## Struct for variable declarations
    name*: string
    varType*: Type
    initialValue*: Expression

  Assign = object
    ## Struct for assignment statements
    target*: Expression
    value*: Expression

  ElifStmt* = object
    ## Struct for elif statements
    condition*: Expression
    thenBranch*: ref Statement

  IfStmt = object
    ## Struct for if statements
    condition*: Expression
    thenBranch*: ref Statement
    elifBranches*: seq[ElifStmt]
    elseBranch*: ref Statement

  WhileStmt = object
    ## Struct for while statements
    condition*: Expression
    body*: ref Statement

  ReturnStmt = object
    ## Struct for return statements
    returnValue*: Expression
  
  BlockStmt = object
    ## Struct for block statements
    body*: seq[Statement]

  FunctionDecl = object
    ## Struct for function declarations
    name*: Identifier
    params*: seq[(Identifier,Type)]
    returnType*: Type
    body*: ref Statement

  StructDecl = object
    ## Struct for struct declarations
    structName*: Identifier
    fields*: seq[Field]

  Statement* = object
    ## Statement is the base type for all statements in the Kc AST.
    pos*: FilePosition
    case kind*: StatementKind
      of VarDeclaration:
        varDecl*: VarDecl
      of Assignment:
        assign*: Assign
      of IfStatement:
        ifStmt*: IfStmt
      of WhileStatement:
        whileStmt*: WhileStmt
      of ReturnStatement:
        returnStmt*: ReturnStmt
      of BlockStatement:
        blockStmt*: BlockStmt
      of FunctionDeclaration:
        funcDecl*: FunctionDecl
      of StructDeclaration:
        structDecl*: StructDecl
      of ExpressionStatement:
        expression*: Expression
      of BreakStatement, ContinueStatement: discard

let
  UNDEFINED* = Type(kind: UndifinedType)
  NONE* = Type(kind: None)
  INT* = Type(kind: IntType)
  FLOAT* = Type(kind: FloatType)
  BOOL* = Type(kind: BoolType)
  STRING* = Type(kind: StringType)
  CHAR* = Type(kind: CharType)

proc newLiteral*[T](value: T, pos: FilePosition): Expression =
  ## Creates a new literal expression with the given value and position.
  when T is int:
    result = Expression(kind: IntLiteral, nodeType: INT, intVal: value, pos: pos, typeChecked: true)
  elif T is float:
    result = Expression(kind: FloatLiteral, nodeType: FLOAT, floatVal: value, pos: pos, typeChecked: true)
  elif T is bool:
    result = Expression(kind: BoolLiteral, nodeType: BOOL, boolVal: value, pos: pos, typeChecked: true)
  elif T is string:
    result = Expression(kind: StringLiteral, nodeType: STRING, strVal: value, pos: pos, typeChecked: true)
  elif T is char:
    result = Expression(kind: CharLiteral, nodeType: CHAR, charVal: value, pos: pos, typeChecked: true)
  else:
    {.error: "Unsupported literal type".}

proc newArrayLiteral*(elements: seq[Expression], pos: FilePosition): Expression =
  ## Creates a new array literal expression with the given elements and position.
  result = Expression(kind: ArrayLiteral, arrayLiteral: elements, pos: pos, typeChecked: false)
  result.nodeType = Type(kind: ArrayType, arrayInfo: ArrayDescriptor(elementType: (ref Type)(kind: UndifinedType), size: elements.len))

proc newStructLiteral*(fields: Table[Identifier, Expression], pos: FilePosition): Expression =
  ## Creates a new struct literal expression with the given fields and position.
  result = Expression(kind: StructLiteral, structVal: fields, pos: pos, typeChecked: false)
  result.nodeType = Type(kind: StructType, structInfo: StructDescriptor(structName: "__anonymous__", fields: @[]))

proc newIdentifier*(name: string, pos: FilePosition): Expression =
  ## Creates a new identifier expression with the given name and position.
  # We cannot type check identifiers at creation time
  result = Expression(kind: Identifier, name: name, pos: pos, typeChecked: false, nodeType: UNDEFINED) 

proc newCall*(callee: Expression, arguments: seq[Expression], pos: FilePosition): Expression =
  ## Creates a new call expression with the given callee, arguments, and position.
  result = Expression(kind: CallExpression, callexpr: CallExpr(callee: callee, arguments: arguments), pos: pos, typeChecked: false, nodeType: UNDEFINED)

proc newArrayAccess*(objectExpr: Expression, index: Expression, pos: FilePosition): Expression =
  ## Creates a new array access expression with the given object expression, index, and position.
  result = Expression(kind: ArrayAccessExpression, arrayAccess: ArrayAccess(objectExpr: objectExpr, index: index), pos: pos, typeChecked: false, nodeType: UNDEFINED)

proc newMemberAccess*(objectExpr: Expression, member: string, pos: FilePosition): Expression =
  ## Creates a new member access expression with the given object expression, member, and position.
  result = Expression(kind: MemberAccessExpression, memberAccess: MemberAccess(objectExpr: objectExpr, member: member), pos: pos, typeChecked: false, nodeType: UNDEFINED)

proc newGroup*(expr: Expression, pos: FilePosition): Expression =
  ## Creates a new group expression with the given expression and position.
  result = Expression(kind: GroupExpression, expr: expr, pos: pos, typeChecked: false, nodeType: UNDEFINED)

proc newAssign*(target: Expression, value: Expression, pos: FilePosition): Statement =
  ## Creates a new assignment statement with the given target, value, and position.
  result = Statement(kind: Assignment, assign: Assign(target: target, value: value), pos: pos)

proc newVarDecl*(name: string, varType: Type, initialValue: Expression, pos: FilePosition): Statement =
  ## Creates a new variable declaration statement with the given name, type, and position.
  result = Statement(kind: VarDeclaration, varDecl: VarDecl(name: name, varType: varType, initialValue: initialValue), pos: pos)

proc newBinaryExpr*(op: static[OperatorKind], left: Expression, right: Expression, pos: FilePosition): Expression =
  ## Creates a new function call expression with the given callee, arguments, and position.
  var operator: string
  when op == Plus:
    operator = "__plus"
  elif op == Minus:
    operator = "__minus"
  elif op == Multiply:
    operator = "__multiply"
  elif op == Divide:
    operator = "__divide"
  elif op == Modulus:
    operator = "__modulus"
  elif op == EqualEqual:
    operator = "__equal_equal"
  elif op == NotEqual:
    operator = "__not_equal"
  elif op == Less:
    operator = "__less"
  elif op == LessEqual:
    operator = "__less_equal"
  elif op == Greater:
    operator = "__greater"
  elif op == GreaterEqual:
    operator = "__greater_equal"
  elif op == And:
    operator = "__and"
  elif op == Or:
    operator = "__or"
  else:
    {.error: "Invalid operator".}
  let identifier = newIdentifier(operator, pos)
  result = newCall(identifier, @[left, right], pos)

proc newUnaryExpr*(op: static[OperatorKind], operand: Expression, pos: FilePosition): Expression =
  ## Creates a new unary expression with the given operator, operand, and position.
  var operator: string
  when op == Not:
    operator = "__not"
  elif op == Minus:
    operator = "__minus"
  else:
    {.error: "Invalid operator".}
  let identifier = newIdentifier(operator, pos)
  result = newCall(identifier, @[operand], pos)

proc newBlock*(body: seq[Statement], pos: FilePosition): Statement =
  ## Creates a new block statement with the given body and position.
  result = Statement(kind: BlockStatement, blockStmt: BlockStmt(body: body), pos: pos)

proc newType*(name: string | TypeKind, pos: FilePosition): Type =
  ## Creates a new type with the given name and position.
  when name is string:
    result = Type(kind: Defined, name: name, pos: pos)
  else:
    result = Type(kind: name, pos: pos)

proc asRef*[T](t: T): ref T =
  ## asRef returns a reference to the given type.
  var temp: ref T = new T
  temp[] = t
  return temp

proc newArrayType*(size: int, elementType: Type, pos: FilePosition): Type =
  ## Creates a new array type with the given size and element type.
  result = Type(kind: ArrayType, arrayInfo: ArrayDescriptor(size: size, elementType: elementType.asRef()), pos: pos)

proc newFuctionType*(paramTypes: seq[Type], returnType: Type, pos: FilePosition): Type =
  ## Creates a new function type with the given parameter types and return type.
  result = Type(kind: FunctionType, funcInfo: FunctionDescriptor(paramTypes: paramTypes, returnType: returnType.asRef()), pos: pos)

proc newFuncDecl*(name: Identifier, params: seq[(Identifier,Type)], returnType: Type, body: Statement, pos: FilePosition): Statement =
  ## Creates a new function declaration statement with the given name, parameters, return type, and body.
  result = Statement(kind: FunctionDeclaration, funcDecl: FunctionDecl(name: name, params: params, returnType: returnType, body: body.asRef()), pos: pos)

proc newStructDecl*(name: Identifier, fields: seq[Field], pos: FilePosition): Statement =
  ## Creates a new struct declaration statement with the given name and fields.
  result = Statement(kind: StructDeclaration, structDecl: StructDecl(structName: name, fields: fields), pos: pos)

proc newExpressionStmt*(expr: Expression, pos: FilePosition): Statement =
  ## Creates a new expression statement with the given expression.
  result = Statement(kind: ExpressionStatement, expression: expr, pos: pos)

proc newElifStmt*(condition: Expression, thenBranch: Statement, pos: FilePosition): ElifStmt =
  ## Creates a new elif statement with the given condition, then branch, and else branch.
  result = ElifStmt(condition: condition, thenBranch: thenBranch.asRef())

proc newIfStmt*(condition: Expression, thenBranch: Statement, elifBranches: seq[ElifStmt], elseBranch: ref Statement, pos: FilePosition): Statement =
  ## Creates a new if statement with the given condition, then branch, and else branch.
  result = Statement(kind: IfStatement, ifStmt: IfStmt(condition: condition, thenBranch: thenBranch.asRef(), elifBranches: elifBranches, elseBranch: elseBranch), pos: pos)

proc newWhileStmt*(condition: Expression, body: Statement, pos: FilePosition): Statement =
  ## Creates a new while statement with the given condition and body.
  result = Statement(kind: WhileStatement, whileStmt: WhileStmt(condition: condition, body: body.asRef()), pos: pos)

proc newBreakStmt*(pos: FilePosition): Statement =
  ## Creates a new break statement with the given position.
  result = Statement(kind: BreakStatement, pos: pos)

proc newContinueStmt*(pos: FilePosition): Statement =
  ## Creates a new continue statement with the given position.
  result = Statement(kind: ContinueStatement, pos: pos)

proc noneExpression*(): Expression =
  ## Creates a new none expression.
  result = Expression(kind: NoneLiteral, pos: FilePosition(line: 0, column: 0), typeChecked: true, nodeType: NONE)

proc newReturnStmt*(value: Expression, pos: FilePosition): Statement =
  ## Creates a new return statement with the given value.
  result = Statement(kind: ReturnStatement, returnStmt: ReturnStmt(returnValue: value), pos: pos)

import std/strutils
import std/strformat

proc stringify*(expr: Expression, indent: int): string =
  ## stringify returns a string representation of the given expression.
  let indentStr = repeat(" ", indent)
  case expr.kind
  of IntLiteral:
    result = &"{indentStr}intLiteral: {expr.intVal}"
  of FloatLiteral:
    result = &"{indentStr}floatLiteral: {expr.floatVal}"
  of BoolLiteral:
    result = &"{indentStr}boolLiteral: {expr.boolVal}"
  of StringLiteral:
    result = &"{indentStr}stringLiteral: {expr.strVal}"
  of CharLiteral:
    result = &"{indentStr}charLiteral: {expr.charVal}"
  of StructLiteral:
    result = &"{indentStr}structLiteral:\n"
    for key in expr.structVal.keys:
      result &= &"{indentStr}  {key.name}:\n"
      result &= expr.structVal[key].stringify(indent + 4) & "\n"
  of ArrayLiteral:
    result = &"{indentStr}arrayLiteral:\n"
    result &= &"{indentStr}  size: {expr.arrayLiteral.len}\n"
    result &= &"{indentStr}  elements:\n"
    for element in expr.arrayLiteral:
      result &= element.stringify(indent + 4) & "\n"
  of NoneLiteral:
    result = &"{indentStr}none"
  of Identifier:
    result = &"{indentStr}identifier: {expr.name}"
  of GroupExpression:
    result = &"{indentStr}group:\n{indentStr}{expr.expr.stringify(indent + 2)}"
  of CallExpression:
    result = &"{indentStr}call:\n"
    result &= &"{indentStr}  callee:\n"
    result &= expr.callExpr.callee.stringify(indent + 4) & "\n"
    result &= &"{indentStr}  arguments:\n"
    for argument in expr.callExpr.arguments:
      result &= argument.stringify(indent + 4) & "\n"
  of ArrayAccessExpression:
    result = &"{indentStr}arrayAccess:\n"
    result &= &"{indentStr}  objectExpr:\n"
    result &= expr.arrayAccess.objectExpr.stringify(indent + 4) & "\n"
    result &= &"{indentStr}  index:\n"
    result &= expr.arrayAccess.index.stringify(indent + 4) & "\n"
  of MemberAccessExpression:
    result = &"{indentStr}memberAccess:\n"
    result &= &"{indentStr}  objectExpr:\n"
    result &= expr.memberAccess.objectExpr.stringify(indent + 4) & "\n"
    result &= &"{indentStr}  member: {expr.memberAccess.member}"

proc stringify*(stmt: ref Statement, indent: int): string

proc stringify*(stmt: Statement, indent: int): string =
  ## stringify returns a string representation of the given statement.
  let indentStr = repeat(" ", indent)
  case stmt.kind
  of VarDeclaration:
    result = &"{indentStr}varDeclaration:\n"
    result &= &"{indentStr}  name:{stmt.varDecl.name}\n"
    result &= &"{indentStr}  varType:{stmt.varDecl.varType.kind}\n"
    result &= &"{indentStr}  initialValue:\n"
    result &= stmt.varDecl.initialValue.stringify(indent + 4)
  of Assignment:
    result = &"{indentStr}assignment:\n"
    result &= &"{indentStr}  target:\n"
    result &= stmt.assign.target.stringify(indent + 4)
    result &= &"{indentStr}  value:\n"
    result &= stmt.assign.value.stringify(indent + 4)
  of IfStatement:
    result = &"{indentStr}if:\n"
    result &= &"{indentStr}  condition:\n"
    result &= stmt.ifStmt.condition.stringify(indent + 4)
    result &= &"{indentStr}  thenBranch:\n"
    result &= stmt.ifStmt.thenBranch.stringify(indent + 4)
    for elifStmt in stmt.ifStmt.elifBranches:
      result &= &"{indentStr}elif:\n"
      result &= &"{indentStr}  condition:\n"
      result &= elifStmt.condition.stringify(indent + 4)
      result &= &"{indentStr}  thenBranch:\n"
      result &= elifStmt.thenBranch.stringify(indent + 4)
    if stmt.ifStmt.elseBranch != nil:
      result &= &"{indentStr}else:\n"
      result &= stmt.ifStmt.elseBranch.stringify(indent + 4)
  of WhileStatement:
    result = &"{indentStr}while:\n"
    result &= &"{indentStr}  condition:\n"
    result &= stmt.whileStmt.condition.stringify(indent + 4)
    result &= &"{indentStr}  body:\n"
    result &= stmt.whileStmt.body.stringify(indent + 4)
  of ReturnStatement:
    result = &"{indentStr}return:\n"
    result &= stmt.returnStmt.returnValue.stringify(indent + 2)
  of BlockStatement:
    result = &"{indentStr}block:\n"
    for statement in stmt.blockStmt.body:
      result &= statement.stringify(indent + 2) & "\n"
  of FunctionDeclaration:
    result = &"{indentStr}func:\n"
    result &= &"{indentStr}  name:{stmt.funcDecl.name.name}\n"
    result &= &"{indentStr}  returnType:{stmt.funcDecl.returnType.kind}\n"
    result &= &"{indentStr}  params:\n"
    for param in stmt.funcDecl.params:
      result &= &"{indentStr}    {param[0].name}:{param[1].kind}\n"
    result &= &"{indentStr}  body:\n"
    result &= stmt.funcDecl.body.stringify(indent + 4)
  of StructDeclaration:
    result = &"{indentStr}struct:\n"
    result &= &"{indentStr}  name:{stmt.structDecl.structName.name}\n"
    result &= &"{indentStr}  fields:\n"
    for field in stmt.structDecl.fields:
      result &= &"{indentStr}    {field.name}:{field.typ.kind}\n"
  of ExpressionStatement:
    result = &"{indentStr}expression:\n"
    result &= stmt.expression.stringify(indent + 2)
  of BreakStatement:
    result = &"{indentStr}break"
  of ContinueStatement:
    result = &"{indentStr}continue"

proc stringify*(stmt: ref Statement, indent: int): string =
  ## stringify returns a string representation of the given statement.
  result = stmt[].stringify(indent)

import re
proc normalizeNewlines(s: string): string =
  let rePattern = re"(\n)+"
  return s.replace(rePattern, "\n")

proc stringify*(program: seq[Statement]): string =
  ## stringify returns a string representation of the given program.
  result = ""
  for statement in program:
    result &= statement.stringify(0) & "\n"
  result = normalizeNewlines(result)