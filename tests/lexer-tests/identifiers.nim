import ../../src/pipeline/lexer
import ../../src/data_structures/token
import ../../src/data_structures/position
import ../../src/error

import strformat

proc int_ident_test() =
  ## Test that identifiers are recognized correctly.
  let source = "var a = 5"
  var reporter = newErrorReporter()
  let my_lexer = newLexer(reporter)
  let tokens = my_lexer.tokenize(source)
  let expexted = [
    Token(kind: TK.Var, pos: pos(1, 1)),
    Token(kind: TK.Identifier, ident: "a", pos: pos(1, 5)),
    Token(kind: TK.Equal, pos: pos(1, 7)),
    Token(kind: TK.Integer, intVal: 5, pos: pos(1, 9)),
    Token(kind: TK.EOF, pos: pos(1, 10))
  ]
  echo tokens
  doAssert tokens.len == expexted.len, &"expexted {expexted.len} but got {tokens.len}"
  for i in 0..<tokens.len:
    doAssert tokens[i] == expexted[i], &"expexted {expexted[i]} at {expexted[i].pos} but got {tokens[i]} at {tokens[i].pos}"

int_ident_test()

proc float_ident_test() =
  ## Test that identifiers are recognized correctly.
  let source = "var a = 5.5"
  var reporter = newErrorReporter()
  let my_lexer = newLexer(reporter)
  let tokens = my_lexer.tokenize(source)
  let expexted = [
    Token(kind: TK.Var, pos: pos(1, 1)),
    Token(kind: TK.Identifier, ident: "a", pos: pos(1, 5)),
    Token(kind: TK.Equal, pos: pos(1, 7)),
    Token(kind: TK.Float, floatVal: 5.5, pos: pos(1, 9)),
    Token(kind: TK.EOF, pos: pos(1, 12))
  ]
  echo tokens
  doAssert tokens.len == expexted.len, &"expexted {expexted.len} but got {tokens.len}"
  for i in 0..<tokens.len:
    doAssert tokens[i] == expexted[i], &"expexted {expexted[i]} at {expexted[i].pos} but got {tokens[i]} at {tokens[i].pos}"

float_ident_test()

proc string_ident_test() =
  ## Test that identifiers are recognized correctly.
  let source = "var a = \"hello\""
  var reporter = newErrorReporter()
  let my_lexer = newLexer(reporter)
  let tokens = my_lexer.tokenize(source)
  let expexted = [
    Token(kind: TK.Var, pos: pos(1, 1)),
    Token(kind: TK.Identifier, ident: "a", pos: pos(1, 5)),
    Token(kind: TK.Equal, pos: pos(1, 7)),
    Token(kind: TK.String, strVal: "hello", pos: pos(1, 9)),
    Token(kind: TK.EOF, pos: pos(1, 16))
  ]
  echo tokens
  doAssert tokens.len == expexted.len, &"expexted {expexted.len} but got {tokens.len}"
  for i in 0..<tokens.len:
    doAssert tokens[i] == expexted[i], &"expexted {expexted[i]} at {expexted[i].pos} but got {tokens[i]} at {tokens[i].pos}"

string_ident_test()