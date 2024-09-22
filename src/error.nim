import std/strformat
import data_structures/position

type
  CompilerErrorKind* = enum
    LexicalError  # Lexical errors occur when the lexer encounters invalid characters or constructs.
    SyntaxError   # Syntax errors occur when the parser encounters invalid syntax.
    SemanticError # Semantic errors occur when the compiler encounters errors during the compilation process.

  CompilerError* = object
    kind*: CompilerErrorKind
    message*: string
    pos*: FilePosition

  ErrorReporter* = ref object
    errors*: seq[CompilerError]

proc newErrorReporter*(): ErrorReporter {.inline.} =
  ErrorReporter(errors: @[])

proc reportError*(reporter: ErrorReporter, kind: CompilerErrorKind, msg: string, pos: FilePosition) =
  reporter.errors.add(CompilerError(kind: kind, message: msg, pos: pos))

proc hasErrors*(reporter: ErrorReporter): bool =
  return reporter.errors.len > 0

proc printErrors*(reporter: ErrorReporter) =
  for err in reporter.errors:
    echo &"{err.kind}: {err.message} at {err.pos}"