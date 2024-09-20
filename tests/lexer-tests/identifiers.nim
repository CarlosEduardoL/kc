import ../../src/pipeline/lexer
import ../../src/data_structures/token

import strformat

proc int_ident_test() =
  ## Test that identifiers are recognized correctly.
  let source = "var a = 5"
  var hadError = false
  let my_lexer = newLexer(source)
  let tokens = my_lexer.tokenize(hadError)
  doAssert tokens.len == 4, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Var, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Identifier, &"tokens[1].kind is {tokens[1].kind}"
  doAssert tokens[1].ident == "a", &"tokens[1].ident is {tokens[1].ident}"
  doAssert tokens[2].kind == TK.Equal, &"tokens[2].kind is {tokens[2].kind}"
  doAssert tokens[3].kind == TK.Integer, &"tokens[3].kind is {tokens[3].kind}"
  doAssert tokens[3].intVal == 5, &"tokens[3].intVal is {tokens[3].intVal}"

int_ident_test()

proc float_ident_test() =
  ## Test that identifiers are recognized correctly.
  let source = "var a = 5.5"
  var hadError = false
  let my_lexer = newLexer(source)
  let tokens = my_lexer.tokenize(hadError)
  doAssert tokens.len == 4, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Var, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Identifier, &"tokens[1].kind is {tokens[1].kind}"
  doAssert tokens[1].ident == "a", &"tokens[1].ident is {tokens[1].ident}"
  doAssert tokens[2].kind == TK.Equal, &"tokens[2].kind is {tokens[2].kind}"
  doAssert tokens[3].kind == TK.Float, &"tokens[3].kind is {tokens[3].kind}"
  doAssert tokens[3].floatVal == 5.5, &"tokens[3].floatVal is {tokens[3].floatVal}" 

float_ident_test()

proc string_ident_test() =
  ## Test that identifiers are recognized correctly.
  let source = "var a = \"hello\""
  var hadError = false
  let my_lexer = newLexer(source)
  let tokens = my_lexer.tokenize(hadError)
  doAssert tokens.len == 4, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Var, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Identifier, &"tokens[1].kind is {tokens[1].kind}"
  doAssert tokens[1].ident == "a", &"tokens[1].ident is {tokens[1].ident}"
  doAssert tokens[2].kind == TK.Equal, &"tokens[2].kind is {tokens[2].kind}"
  doAssert tokens[3].kind == TK.String, &"tokens[3].kind is {tokens[3].kind}" 
  doAssert tokens[3].strVal == "hello", &"tokens[3].strVal is {tokens[3].strVal}"

string_ident_test()