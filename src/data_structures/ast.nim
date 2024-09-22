import position
import std/tables

type
  TypeKind* {.pure.} = enum
    ## Enum for all types in the Kc Expressions.
    IntType, FloatType, BoolType, StringType, CharType # Primitive types
    StructType, FunctionType, ArrayType # User-defined types
    None # None type for no return functions, is not valid for assignments
    UndifinedType # Temporal Type for every expression before type inference and Semantic analysis

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
      of IntType, FloatType, BoolType, StringType, CharType, UndifinedType, None: discard
      of StructType:
        structInfo*: StructDescriptor
      of ArrayType:
        arrayInfo*: ArrayDescriptor
      of FunctionType:
        funcInfo*: FunctionDescriptor
  
  TypeRef = ref Type
    ## TypeRef is a reference to a Type object.

  OperatorKind* = enum
    ## Enum for all operators in the Kc Expressions.
    ## Note: We can reuse the token enum for operators, but we need to define our own enum to isolate the compile phases.
    Plus, Minus, Star, Slash, Percent, EqualEqual, NotEqual, Less, LessEqual, Greater, GreaterEqual, And, Or, Bang

  ExpressionKind* = enum
    ## Enum for all expressions in the Kc AST.
    IntLiteral, FloatLiteral, BoolLiteral, StringLiteral, CharLiteral, StructLiteral, Identifier, UnaryExpression, BinaryExpression, GroupExpression, CallExpression, MemberAccessExpression

  UnaryExpr = object
    ## Struct for unary expressions
    op*: OperatorKind
    operand*: Expression

  BinaryExpr = object
    ## Struct for binary expressions
    op*: OperatorKind
    left*: Expression
    right*: Expression

  CallExpr = object
    ## Struct for function call expressions
    callee*: Expression
    arguments*: seq[Expression]

  MemberAccess = object
    ## Struct for member access expressions
    objectExpr*: Expression
    member*: string

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
        structVal*: Table[string, Expression]
      of Identifier:
        name*: string
      of UnaryExpression:
        unaryExpr*: UnaryExpr
      of BinaryExpression:
        binaryExpr*: BinaryExpr
      of GroupExpression:
        expr*: Expression
      of CallExpression:
        callExpr*: CallExpr
      of MemberAccessExpression:
        memberAccess*: MemberAccess

  StatementKind* = enum
    ## StatementKind represents the different types of statements in the Kc AST.
    VarDeclaration, Assignment, IfStatement, WhileStatement, ReturnStatement, BlockStatement, FunctionDeclaration

  VarDecl = object
    ## Struct for variable declarations
    name*: string
    varType*: Type
    initialValue*: Expression

  Assign = object
    ## Struct for assignment statements
    name*: string
    value*: Expression

  IfStmt = object
    ## Struct for if statements
    condition*: Expression
    thenBranch*: seq[Statement]
    elseBranch*: seq[Statement]

  WhileStmt = object
    ## Struct for while statements
    condition*: Expression
    body*: seq[Statement]

  ReturnStmt = object
    ## Struct for return statements
    returnValue*: Expression
  
  BlockStmt = object
    ## Struct for block statements
    body*: seq[Statement]

  FunctionDecl = object
    ## Struct for function declarations
    name*: string
    paramTypes*: seq[Type]
    returnType*: Type
    body*: seq[Statement]

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

proc newIdentifier*(name: string, pos: FilePosition): Expression =
  ## Creates a new identifier expression with the given name and position.
  # We cannot type check identifiers at creation time
  result = Expression(kind: Identifier, name: name, pos: pos, typeChecked: false, nodeType: UNDEFINED) 

proc newCall*(callee: Expression, arguments: seq[Expression], pos: FilePosition): Expression =
  ## Creates a new call expression with the given callee, arguments, and position.
  result = Expression(kind: CallExpression, callexpr: CallExpr(callee: callee, arguments: arguments), pos: pos, typeChecked: false, nodeType: UNDEFINED)

proc newMemberAccess*(objectExpr: Expression, member: string, pos: FilePosition): Expression =
  ## Creates a new member access expression with the given object expression, member, and position.
  result = Expression(kind: MemberAccessExpression, memberAccess: MemberAccess(objectExpr: objectExpr, member: member), pos: pos, typeChecked: false, nodeType: UNDEFINED)

proc newBinary*(op: OperatorKind, left: Expression, right: Expression, pos: FilePosition): Expression =
  ## Creates a new binary expression with the given operator, left operand, right operand, and position.
  result = Expression(kind: BinaryExpression, binaryExpr: BinaryExpr(op: op, left: left, right: right), pos: pos, typeChecked: false, nodeType: UNDEFINED)

proc newUnary*(op: OperatorKind, operand: Expression, pos: FilePosition): Expression =
  ## Creates a new unary expression with the given operator, operand, and position.
  result = Expression(kind: UnaryExpression, unaryExpr: UnaryExpr(op: op, operand: operand), pos: pos, typeChecked: false, nodeType: UNDEFINED)

proc newGroup*(expr: Expression, pos: FilePosition): Expression =
  ## Creates a new group expression with the given expression and position.
  result = Expression(kind: GroupExpression, expr: expr, pos: pos, typeChecked: false, nodeType: UNDEFINED)

proc newAssign*(name: string, value: Expression, pos: FilePosition): Statement =
  ## Creates a new assignment statement with the given name, value, and position.
  result = Statement(kind: Assignment, assign: Assign(name: name, value: value), pos: pos)

proc newVarDecl*(name: string, varType: Type, initialValue: Expression, pos: FilePosition): Statement =
  ## Creates a new variable declaration statement with the given name, type, and position.
  result = Statement(kind: VarDeclaration, varDecl: VarDecl(name: name, varType: varType, initialValue: initialValue), pos: pos)
