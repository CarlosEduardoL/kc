import ../../src/pipeline/lexer
import ../../src/data_structures/token
import ../../src/data_structures/position
import ../../src/error
import strformat

let source = "= == ! != < > <= >= , ; : ( ) { } [ ] ^ || &&"
var reporter = newErrorReporter()
let my_lexer = newLexer(reporter)
let tokens = my_lexer.tokenize(source)
let expexted = [
  Token(kind: TK.Equal, pos: pos(1, 1)),
  Token(kind: TK.EqualEqual, pos: pos(1, 3)),
  Token(kind: TK.Bang, pos: pos(1, 6)),
  Token(kind: TK.NotEqual, pos: pos(1, 8)),
  Token(kind: TK.Less, pos: pos(1, 11)),
  Token(kind: TK.Greater, pos: pos(1, 13)),
  Token(kind: TK.LessEqual, pos: pos(1, 15)),
  Token(kind: TK.GreaterEqual, pos: pos(1, 18)),
  Token(kind: TK.Comma, pos: pos(1, 21)),
  Token(kind: TK.Semicolon, pos: pos(1, 23)),
  Token(kind: TK.Colon, pos: pos(1, 25)),
  Token(kind: TK.LeftParen, pos: pos(1, 27)),
  Token(kind: TK.RightParen, pos: pos(1, 29)),
  Token(kind: TK.LeftCurly, pos: pos(1, 31)),
  Token(kind: TK.RightCurly, pos: pos(1, 33)),
  Token(kind: TK.LeftBracket, pos: pos(1, 35)),
  Token(kind: TK.RightBracket, pos: pos(1, 37)),
  Token(kind: TK.Or, pos: pos(1, 41)),
  Token(kind: TK.And, pos: pos(1, 44)),
  Token(kind: TK.EOF, pos: pos(1, 46))
]
doAssert tokens.len == expexted.len, &"expexted {expexted.len} but got {tokens.len}"
for i in 0..<tokens.len:
  doAssert tokens[i] == expexted[i], &"expexted {expexted[i]} at {expexted[i].pos} but got {tokens[i]} at {tokens[i].pos}"
doAssert reporter.hasErrors == true, &"hadError is {reporter.hasErrors}"
doAssert reporter.errors[0].message == "Unexpected character '^'"