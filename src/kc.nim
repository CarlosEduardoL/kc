import pipeline/lexer
import pipeline/parser
import data_structures/ast
import error

import std/os
import std/strformat
import utils/unionutils

proc main() =
  if paramCount() < 1:
    stderr.writeLine "Usage: kc <filename>"
    quit(1)
  let filename = paramStr(1)
  let source = readFile(filename)
  var reporter = newErrorReporter()
  var tokens = newLexer(reporter).tokenize(source)
  var parser = newParser(reporter)
  let program = parser.parse(tokens)
  echo stringify(program)
  if reporter.hasErrors:
    reporter.printErrors()
    quit(1)

when isMainModule:
  main()
