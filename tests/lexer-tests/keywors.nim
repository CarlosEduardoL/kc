import ../../src/pipeline/lexer
import ../../src/data_structures/token
import ../../src/data_structures/position
import ../../src/error
import strformat


let source_with_keywords = """var struct if else while for return break continue true false none func"""
var reporter = newErrorReporter()
let my_lexer = newLexer(reporter)
let tokens = my_lexer.tokenize(source_with_keywords)
let expexted = [
  Token(kind: TK.Var, pos: pos(1, 1)),
  Token(kind: TK.Struct, pos: pos(1, 4)),
  Token(kind: TK.If, pos: pos(1, 10)),
  Token(kind: TK.Else, pos: pos(1, 13)),
  Token(kind: TK.While, pos: pos(1, 18)),
  Token(kind: TK.For, pos: pos(1, 21)),
  Token(kind: TK.Return, pos: pos(1, 25)),
  Token(kind: TK.Break, pos: pos(1, 31)),
  Token(kind: TK.Continue, pos: pos(1, 38)),
  Token(kind: TK.True, pos: pos(1, 45)),
  Token(kind: TK.False, pos: pos(1, 50)),
  Token(kind: TK.NoneKeyword, pos: pos(1, 55)),
  Token(kind: TK.Func, pos: pos(1, 61))
]