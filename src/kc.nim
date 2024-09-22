import pipeline/lexer
import data_structures/token

import std/os
import std/strformat
import utils/unionutils

proc main() =
  if paramCount() < 1:
    stderr.writeLine "Usage: kc <filename>"
    quit(1)
  let filename = paramStr(1)
  let source = readFile(filename)
  var hadError = false
  let my_lexer = newLexer(source)
  let tokens = my_lexer.tokenize(hadError)
  if hadError:
    for token in tokens:
      checked token.kind, TK.Error:
        stderr.writeLine &"Error: {token.errorMsg} at {token.pos}"
  else:
    var line = 1
    for token in tokens:
      if token.pos.line != line:
        line.inc
        echo ""
      stdout.write &"{token} "

when isMainModule:
  main()
