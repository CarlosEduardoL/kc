import position

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

  StructDescriptor* = object
    ## Struct for struct types
    structName*: string
    fields*: seq[Field]

  ArrayDescriptor* = object
    ## Struct for array types
    elementType*: TypeRef
    size*: int

  FunctionDescriptor* = object
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

let
  UNDEFINED* = Type(kind: UndifinedType)

proc `==`*(a, b: Type): bool =
  ## `==` operator for types.
  if a.kind != b.kind: return false
  case a.kind
  of IntType, FloatType, BoolType, StringType, CharType, Defined, UndifinedType, Infered, None:
    return true
  of StructType:
    case b.kind
    of StructType:
      if a.structInfo.fields.len != b.structInfo.fields.len: return false
      for i in 0 ..< a.structInfo.fields.len:
        if a.structInfo.fields[i].name != b.structInfo.fields[i].name: return false
        if a.structInfo.fields[i].typ != b.structInfo.fields[i].typ: return false
      return true
    else:
      return false
  of ArrayType:
    case b.kind
    of ArrayType:
      if a.arrayInfo.size != b.arrayInfo.size: return false
      if a.arrayInfo.elementType != b.arrayInfo.elementType: return false
      return true
    else:
      return false
  of FunctionType:
    case b.kind
    of FunctionType:
      if a.funcInfo.paramTypes.len != b.funcInfo.paramTypes.len: return false
      if a.funcInfo.returnType != b.funcInfo.returnType: return false
      for i in 0 ..< a.funcInfo.paramTypes.len:
        if a.funcInfo.paramTypes[i] != b.funcInfo.paramTypes[i]: return false
      return true
    else:
      return false

proc isOverloable*(a, b: Type): bool =
  ## Checks if a can be overloaded with b.
  ## Two functions are overloads if they have diferen parameters
  if a.kind != b.kind: return false
  case a.kind
  of IntType, FloatType, BoolType, StringType, CharType, Defined, UndifinedType, Infered, None, StructType, ArrayType:
    return true
  of FunctionType:
    case b.kind
    of FunctionType:
      return a.funcInfo.paramTypes != b.funcInfo.paramTypes
    else:
      return false