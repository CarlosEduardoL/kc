import pipeline/lexer
import pipeline/parser
import pipeline/analyzer
import data_structures/ast
import error

import std/os

proc main() =
  if paramCount() < 1:
    stderr.writeLine "Usage: kc <filename>"
    quit(1)
  let filename = paramStr(1)
  let source = readFile(filename)
  var reporter = newErrorReporter()
  var tokens = newLexer(reporter).tokenize(source)
  var parser = newParser(reporter)
  var program = parser.parse(tokens)
  var analyzer = newAnalyzer(reporter)
  analyzer.analyze(program)
  echo stringify(program)
  if reporter.hasErrors:
    reporter.printErrors()
    quit(1)

when isMainModule:
  main()
