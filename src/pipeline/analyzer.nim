import ../data_structures/ast
import ../data_structures/scope
import ../data_structures/position
import ../data_structures/types

import ../error

import std/strformat

type 
  Analyzer* = object
    ## Analyzer is the 3rd component of the Kc compiler pipeline.
    ## It takes the unchanged AST and produces a type-checked AST.
    ## This will happen in several phases:
    ## 1. Construction of the scope tree and symbol tables
    ##    - Goes for every function, struct and variable declaration
    ##    - In every scope and add them to the symbol table
    ## 2. Variable, function and struct declarations resolution
    ##    - Check that variables are declared before use
    ##    - Check that functions are declared before be called
    ##    - Check that structs are declared before use as type
    ## 3. Type resolution
    ##    - Check that every expression has a type
    ##    - Resolve the type of infered variables based on the type of the expression
    ##    - Resolve function return types based on there parameters (function overloading)
    ## 4. Semantic analysis
    ##    - Check that every expression is valid
    ##    - Check that there are no type errors
    reporter: ErrorReporter

proc newAnalyzer*(reporter: ErrorReporter): Analyzer =
  ## newAnalyzer creates a new Analyzer instance with the given reporter.
  Analyzer(reporter: reporter)

proc unwrapBlock(statement: Statement): BlockStmt =
  case statement.kind
    of BlockStatement: statement.blockStmt
    else: raise newException(Exception, "Expected a block statement")

proc phase1*(analyzer: Analyzer, program: var Program, globalScope: Scope) =
  ## phase1 takes a Program and returns a Program with the scope tree and symbol tables.
  var contexts: seq[BlockStmt]
  contexts.add(BlockStmt(scope: globalScope, body: program.statements))
  while contexts.len > 0:
    var context = contexts.pop()
    for statement in context.body:
      case statement.kind
        of VarDeclaration: 
          context.scope.addVariable(statement.varDecl.name, statement.pos, statement.varDecl.varType)
        of FunctionDeclaration: 
          context.scope.addFunction(statement.funcDecl.name.name, statement.pos, statement.funcDecl.returnType)
          var newContext = statement.funcDecl.body[].unwrapBlock()
          newContext.scope = context.scope.newFunctionScope()
          for param in statement.funcDecl.params:
            newContext.scope.addVariable(param[0].name, param[1].pos, param[1])
          contexts.add(newContext)
        of StructDeclaration: 
          try: context.scope.addStruct(statement.structDecl.structName.name, statement.pos, statement.structDecl.toType)
          except CannotOverrideError as e: analyzer.reporter.reportError(SemanticError, &"Trying to override struct {statement.structDecl.structName.name}, that struct is already declared at {e.originalPos}", statement.pos)
        of BlockStatement:
          var newContext = statement.blockStmt
          newContext.scope = context.scope.newBlockScope()
          contexts.add(newContext)
        of IfStatement: 
          var newContext = statement.ifStmt.thenBranch[].unwrapBlock()
          newContext.scope = context.scope.newBlockScope()
          contexts.add(newContext)
          for elifBranch in statement.ifStmt.elifBranches:
            newContext = elifBranch.thenBranch[].unwrapBlock()
            newContext.scope = context.scope.newBlockScope()
            contexts.add(newContext)
          if statement.ifStmt.elseBranch != nil:
            newContext = statement.ifStmt.elseBranch[].unwrapBlock()
            newContext.scope = context.scope.newBlockScope()
            contexts.add(newContext)
        of WhileStatement:
          var newContext = statement.whileStmt.body[].unwrapBlock()
          newContext.scope = context.scope.newLoopScope()
          contexts.add(newContext)
        else: discard

proc analyzeIdentifierAccess(analyzer: Analyzer, scope: Scope, expression: Expression, pos: FilePosition) =
  if expression == nil: return
  case expression.kind
    of ArrayLiteral:
      for element in expression.arrayLiteral:
        analyzer.analyzeIdentifierAccess(scope, element, pos)
    of StructLiteral:
      for (key, value) in expression.structVal:
        analyzer.analyzeIdentifierAccess(scope, value, pos)
    of ArrayAccessExpression:
      analyzer.analyzeIdentifierAccess(scope, expression.arrayAccess.objectExpr, pos)
      analyzer.analyzeIdentifierAccess(scope, expression.arrayAccess.index, pos)
    of GroupExpression:
      analyzer.analyzeIdentifierAccess(scope, expression.expr, pos)
    of CallExpression:
      for argument in expression.callExpr.arguments:
        analyzer.analyzeIdentifierAccess(scope, argument, pos)
      let callee = expression.callExpr.callee
      case callee.kind
      of Identifier: 
        try:discard scope.lookUpFunction(callee.name)
        except ValueError: analyzer.reporter.reportError(SemanticError, &"Function {callee.name} not found", callee.pos)
      else: analyzer.analyzeIdentifierAccess(scope, callee, callee.pos)
    of MemberAccessExpression:
      analyzer.analyzeIdentifierAccess(scope, expression.memberAccess.objectExpr, expression.pos)
    of Identifier:
      try: discard scope.lookUpVariable(expression.name, expression.pos)
      except ValueError: 
        try: discard scope.lookUpFunction(expression.name)
        except ValueError: analyzer.reporter.reportError(SemanticError, &"Variable or function {expression.name} not found", expression.pos)
    of IntLiteral, FloatLiteral, BoolLiteral, StringLiteral, CharLiteral, NoneLiteral: discard

proc analyzeType(analyzer: Analyzer, scope: Scope, typ: Type, pos: FilePosition) =
  case typ.kind
  of TypeKind.Defined:
    try: discard scope.lookUpStruct(typ.name)
    except ValueError: analyzer.reporter.reportError(SemanticError, &"Struct {typ.name} not found", pos)
  of TypeKind.ArrayType:
    analyzer.analyzeType(scope, typ.arrayInfo.elementType[], pos)
  of TypeKind.FunctionType:
    for paramType in typ.funcInfo.paramTypes:
      analyzer.analyzeType(scope, paramType, pos)
    analyzer.analyzeType(scope, typ.funcInfo.returnType[], pos)
  else: discard

proc phase2*(analyzer: Analyzer, program: var Program, globalScope: Scope) =
  ## phase2 takes a Program and returns a Program all variables access checked.
  var contexts: seq[BlockStmt]
  contexts.add(BlockStmt(scope: globalScope, body: program.statements))
  while contexts.len > 0:
    let context = contexts.pop()
    for statement in context.body:
      case statement.kind
      of Assignment:
        analyzer.analyzeIdentifierAccess(context.scope, statement.assign.target, statement.pos)
        analyzer.analyzeIdentifierAccess(context.scope, statement.assign.value, statement.pos)
      of IfStatement:
        analyzer.analyzeIdentifierAccess(context.scope, statement.ifStmt.condition, statement.pos)
        contexts.add(statement.ifStmt.thenBranch[].unwrapBlock())
        for elifBranch in statement.ifStmt.elifBranches:
          analyzer.analyzeIdentifierAccess(context.scope, elifBranch.condition, statement.pos)
          contexts.add(elifBranch.thenBranch[].unwrapBlock())
        if statement.ifStmt.elseBranch != nil:
          contexts.add(statement.ifStmt.elseBranch[].unwrapBlock())
      of WhileStatement:
        analyzer.analyzeIdentifierAccess(context.scope, statement.whileStmt.condition, statement.pos)
        contexts.add(statement.whileStmt.body[].unwrapBlock())
      of ReturnStatement:
        if statement.returnStmt.returnValue != nil:
          analyzer.analyzeIdentifierAccess(context.scope, statement.returnStmt.returnValue, statement.pos)
        if not context.scope.flags.contains(Function):
          analyzer.reporter.reportError(SemanticError, &"Cannot return from a non-function", statement.pos)
      of BreakStatement:
        if not context.scope.flags.contains(Loop):
          analyzer.reporter.reportError(SemanticError, &"Cannot break from a non-loop", statement.pos)
      of ContinueStatement:
        if not context.scope.flags.contains(Loop):
          analyzer.reporter.reportError(SemanticError, &"Cannot continue from a non-loop", statement.pos)
      of ExpressionStatement:
        analyzer.analyzeIdentifierAccess(context.scope, statement.expression, statement.pos)
      of VarDeclaration:
        analyzer.analyzeIdentifierAccess(context.scope, statement.varDecl.initialValue, statement.pos)
      of StructDeclaration:
        for field in statement.structDecl.fields:
          analyzer.analyzeType(context.scope, field.typ, statement.pos)
      of FunctionDeclaration:
        analyzer.analyzeType(context.scope, statement.funcDecl.returnType, statement.pos)
        for param in statement.funcDecl.params:
          analyzer.analyzeType(context.scope, param[1], statement.pos)
        contexts.add(statement.funcDecl.body[].unwrapBlock())
      of BlockStatement:
        contexts.add(statement.blockStmt)


proc analyze*(analyzer: Analyzer, program: var Program) =
  ## analyze takes a Program and returns a type-checked Program.
  var globalScope = initGlobalScope()
  analyzer.phase1(program, globalScope)
  analyzer.phase2(program, globalScope)