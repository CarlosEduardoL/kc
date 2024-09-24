import std/tables
import std/strformat
import ../utils/refutils
import types
import position


type 
  ScopeFlag* {.pure.} = enum
    ## Enum for all scope flags.
    Global     # Only 1 global scope should exist
    Function   # Function scope, child of global scope (there is no closures)
    Block      # Block scope, child of global, function, loop or block scope
    Loop       # Loop scope, child of global, function, block scope, can break and continue

  ScopeFlags* = set[ScopeFlag]

  MetaData* = object
    ## MetaData is a data structure that holds the metadata for a symbol.
    ## Is used to help with type inference and semantic analysis.
    pos: FilePosition
    typ: Type

  Scope* = ref object
    ## Scope is a data structure that holds the block metadata.
    ## Is used to help with type inference and semantic analysis.
    flags*: ScopeFlags
    variableSymbols: Table[string, seq[MetaData]]
    functionSymbols: Table[string, seq[MetaData]]
    structSymbols: Table[string, MetaData]
    parent: Scope
    global: Scope # Reference to the global scope for convenience

  CannotOverrideError* = object of ValueError
    ## CannotOverrideError is raised when a symbol cannot be overridden.
    originalPos*: FilePosition

proc checkVariable*(scope: Scope, name: string): bool =
  ## checkVariable checks if a variable is already declared in the scope.
  result = scope.variableSymbols.hasKey(name) or (scope.parent != nil and scope.parent.checkVariable(name))

proc checkFunction*(scope: Scope, name: string): bool =
  ## checkFunction checks if a function is already declared in the scope.
  result = scope.functionSymbols.hasKey(name) or (scope.parent != nil and scope.parent.checkFunction(name))

proc checkStruct*(scope: Scope, name: string): bool =
  ## checkStruct checks if a struct is already declared in the scope.
  result = scope.structSymbols.hasKey(name) or (scope.parent != nil and scope.parent.checkStruct(name))

proc lookUpVariable*(scope: Scope, name: string, pos: FilePosition): MetaData =
  ## lookUpVariable looks up a variable in the scope.
  ## If the variable is not found error message is returned.
  if scope.variableSymbols.hasKey(name):
    for i in 0..<scope.variableSymbols[name].len:
      let variable = scope.variableSymbols[name][scope.variableSymbols[name].len - 1 - i]
      if variable.pos.line < pos.line or (variable.pos.line == pos.line and variable.pos.column <= pos.column):
        return variable
  if scope.parent != nil:
    return scope.parent.lookUpVariable(name, pos)
  else:
    raise ValueError.newException(&"Variable {name} not found")

proc lookUpFunction*(scope: Scope, name: string): seq[MetaData] =
  ## lookUpFunction looks up a function in the scope.
  ## If the function is not found error message is returned.
  result = @[]
  if scope.functionSymbols.hasKey(name):
    result &= scope.functionSymbols[name]
  if scope.parent != nil:
    result &= scope.parent.lookUpFunction(name)
  if result.len == 0:
    raise ValueError.newException(&"Function {name} not found")

proc lookUpStruct*(scope: Scope, name: string): MetaData =
  ## lookUpStruct looks up a struct in the scope.
  ## If the struct is not found error message is returned.
  if scope.structSymbols.hasKey(name):
    return scope.structSymbols[name]
  elif scope.parent != nil:
    return scope.parent.lookUpStruct(name)
  else:
    raise ValueError.newException(&"Struct {name} not found")

proc addVariable*(scope: Scope, name: string, pos: FilePosition, typ: Type) =
  ## addVariable adds a variable declaration to the scope.
  ## variable declaration always overrides a previous declaration
  if not scope.variableSymbols.hasKey(name):
    scope.variableSymbols[name] = @[MetaData(pos: pos, typ: typ)]
    return
  scope.variableSymbols[name].add(MetaData(pos: pos, typ: typ))

proc addStruct*(scope: Scope, name: string, pos: FilePosition, typ: Type) =
  ## addStruct adds a struct declaration to the scope.
  ## struct declaration cannot override a previous declaration
  ## if previous declaration is a struct, it should raise an error
  if scope.structSymbols.hasKey(name):
    raise (ref CannotOverrideError)(originalPos: scope.structSymbols[name].pos)
  scope.structSymbols[name] = MetaData(pos: pos, typ: typ)

proc addFunction*(scope: Scope, name: string, pos: FilePosition, typ: Type) =
  ## addFunction adds a function declaration to the scope.
  ## there will be several function declarations with the same name
  ## but if them have the same type, it will override the previous declaration
  if not scope.functionSymbols.hasKey(name):
    scope.functionSymbols[name] = @[MetaData(pos: pos, typ: typ)]
    return
  # check if function is already declared if so, replace it
  let functions = scope.functionSymbols[name]
  for i in 0..<functions.len:
    if typ == functions[i].typ: # Already have a function with the same name and type
      scope.functionSymbols[name][i] = MetaData(pos: pos, typ: typ)
      return
  scope.functionSymbols[name].add(MetaData(pos: pos, typ: typ))

proc newFunctionScope*(parent: Scope): Scope =
  ## newFunctionScope creates a new function scope.
  result = Scope(flags: {Function}, parent: parent.global)

proc newBlockScope*(parent: Scope): Scope =
  ## newBlockScope creates a new block scope.
  result = Scope(parent: parent)
  if not parent.flags.contains(Global):
    result.flags.incl(parent.flags)
  result.flags.incl(Block)

proc newLoopScope*(parent: Scope): Scope =
  ## newLoopScope creates a new loop scope.
  result = Scope(parent: parent)
  if not parent.flags.contains(Global):
    result.flags.incl(parent.flags)
  result.flags.incl(Loop)

template functionType(returnTy: Type, params: varargs[Type]): Type =
  Type(kind: TypeKind.FunctionType, funcInfo: FunctionDescriptor(paramTypes: @params, returnType: returnTy.asRef()))

proc initGlobalScope*(): Scope =
  ## initGlobalScope initializes a new global scope.
  result = Scope(flags: {Global})
  result.global = result
  
  # Introduce primitive operators on the global scope
  let binaryInt = MetaData(typ: functionType(Type(kind: IntType), Type(kind: IntType), Type(kind: IntType)))
  let binaryFloat = MetaData(typ: functionType(Type(kind: FloatType), Type(kind: FloatType), Type(kind: FloatType)))
  let binaryString = MetaData(typ: functionType(Type(kind: StringType), Type(kind: StringType), Type(kind: StringType)))
  let binaryChar = MetaData(typ: functionType(Type(kind: CharType), Type(kind: CharType), Type(kind: CharType)))
  let binaryBool = MetaData(typ: functionType(Type(kind: BoolType), Type(kind: BoolType), Type(kind: BoolType)))
  let unaryInt = MetaData(typ: functionType(Type(kind: IntType), Type(kind: IntType)))
  let unaryFloat = MetaData(typ: functionType(Type(kind: FloatType), Type(kind: FloatType)))
  let unaryBool = MetaData(typ: functionType(Type(kind: BoolType), Type(kind: BoolType)))

  result.functionSymbols["__plus"] = @[binaryInt, binaryFloat, binaryString, binaryChar]
  result.functionSymbols["__minus"] = @[binaryInt, binaryFloat, unaryInt, unaryFloat]
  result.functionSymbols["__multiply"] = @[binaryInt, binaryFloat, binaryString, binaryChar]
  result.functionSymbols["__divide"] = @[binaryInt, binaryFloat, binaryString, binaryChar]
  result.functionSymbols["__modulus"] = @[binaryInt, binaryFloat, binaryString, binaryChar]
  result.functionSymbols["__equal_equal"] = @[binaryInt, binaryFloat, binaryString, binaryChar, binaryBool]
  result.functionSymbols["__not_equal"] = @[binaryInt, binaryFloat, binaryString, binaryChar, binaryBool]
  result.functionSymbols["__less"] = @[binaryInt, binaryFloat, binaryString, binaryChar]
  result.functionSymbols["__less_equal"] = @[binaryInt, binaryFloat, binaryString, binaryChar]
  result.functionSymbols["__greater"] = @[binaryInt, binaryFloat, binaryString, binaryChar]
  result.functionSymbols["__greater_equal"] = @[binaryInt, binaryFloat, binaryString, binaryChar]
  result.functionSymbols["__and"] = @[binaryBool]
  result.functionSymbols["__or"] = @[binaryBool]
  result.functionSymbols["__not"] = @[unaryBool]

  # Support print function
  result.functionSymbols["print"] = @[
    MetaData(typ: functionType(Type(kind: None), Type(kind: IntType))),
    MetaData(typ: functionType(Type(kind: None), Type(kind: FloatType))),
    MetaData(typ: functionType(Type(kind: None), Type(kind: StringType))),
    MetaData(typ: functionType(Type(kind: None), Type(kind: CharType))),
    MetaData(typ: functionType(Type(kind: None), Type(kind: BoolType))),
  ]

  # Support for range function to implement for loops
  result.functionSymbols["range"] = @[
    MetaData(
      typ: functionType(
        Type(
          kind: StructType,
          structInfo: StructDescriptor(
            fields: @[
              Field(name: "current", typ: Type(kind: IntType), pos: FilePosition(line: 0, column: 0)),
              Field(name: "end", typ: Type(kind: IntType), pos: FilePosition(line: 0, column: 0))
            ]
          )
        ), 
        Type(kind: IntType), 
        Type(kind: IntType)
      )
    )
  ]
  result.functionSymbols["next"] = @[
    MetaData(
      typ: functionType(
        Type(kind: IntType),
        Type(
          kind: StructType,
          structInfo: StructDescriptor(
            structName: "range",
            fields: @[
              Field(name: "current", typ: Type(kind: IntType), pos: FilePosition(line: 0, column: 0)),
              Field(name: "end", typ: Type(kind: IntType), pos: FilePosition(line: 0, column: 0))
            ]
          )
        )
      )
    )
  ]
  result.functionSymbols["has_next"] = @[
    MetaData(
      typ: functionType(
        Type(kind: BoolType),
        Type(
          kind: StructType,
          structInfo: StructDescriptor(
            structName: "range",
            fields: @[
              Field(name: "current", typ: Type(kind: IntType), pos: FilePosition(line: 0, column: 0)),
              Field(name: "end", typ: Type(kind: IntType), pos: FilePosition(line: 0, column: 0))
            ]
          )
        )
      )
    )
  ]