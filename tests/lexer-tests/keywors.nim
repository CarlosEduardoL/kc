import ../../src/pipeline/lexer
import ../../src/data_structures/token
import strformat

proc tokenize(source: string): tuple[tokens: seq[Token], hadError: bool] =
  ## Tokenizes the given source code and returns a sequence of tokens and a boolean indicating if an error occurred.
  var hadError = false
  let my_lexer = newLexer(source)
  let tokens = my_lexer.tokenize(hadError)
  return (tokens, hadError)

proc var_keyword_test() =
  ## Test that var keyword is recognized correctly.
  let (tokens, hadError) = tokenize("var\n")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Var, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.NewLine, &"tokens[1].kind is {tokens[1].kind}"

var_keyword_test()

proc func_keyword_test() =
  ## Test that func keyword is recognized correctly.
  let (tokens, hadError) = tokenize("func\n")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Func, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.NewLine, &"tokens[1].kind is {tokens[1].kind}"

func_keyword_test()

proc struct_keyword_test() =
  ## Test that struct keyword is recognized correctly.
  let (tokens, hadError) = tokenize("struct\n")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Struct, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.NewLine, &"tokens[1].kind is {tokens[1].kind}"

struct_keyword_test()

proc if_keyword_test() =
  ## Test that if keyword is recognized correctly.
  let (tokens, hadError) = tokenize("if\n")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.If, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.NewLine, &"tokens[1].kind is {tokens[1].kind}"

if_keyword_test()

proc else_keyword_test() =
  ## Test that else keyword is recognized correctly.
  let (tokens, hadError) = tokenize("else\n")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Else, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.NewLine, &"tokens[1].kind is {tokens[1].kind}"

else_keyword_test()

proc while_keyword_test() =
  ## Test that while keyword is recognized correctly.
  let (tokens, hadError) = tokenize("while;")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.While, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Semicolon, &"tokens[1].kind is {tokens[1].kind}"

while_keyword_test()

proc for_keyword_test() =
  ## Test that for keyword is recognized correctly.
  let (tokens, hadError) = tokenize("for;")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens[0].kind == TK.For, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Semicolon, &"tokens[1].kind is {tokens[1].kind}"

for_keyword_test()

proc return_keyword_test() =
  ## Test that return keyword is recognized correctly.
  let (tokens, hadError) = tokenize("return;")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Return, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Semicolon, &"tokens[1].kind is {tokens[1].kind}"

return_keyword_test()

proc break_keyword_test() =
  ## Test that break keyword is recognized correctly.
  let (tokens, hadError) = tokenize("break;")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Break, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Semicolon, &"tokens[1].kind is {tokens[1].kind}"

break_keyword_test()

proc continue_keyword_test() =
  ## Test that continue keyword is recognized correctly.
  let (tokens, hadError) = tokenize("continue;")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.Continue, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Semicolon, &"tokens[1].kind is {tokens[1].kind}"

continue_keyword_test()

proc true_keyword_test() =
  ## Test that true keyword is recognized correctly.
  let (tokens, hadError) = tokenize("true;")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.True, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Semicolon, &"tokens[1].kind is {tokens[1].kind}"

true_keyword_test()

proc false_keyword_test() =
  ## Test that false keyword is recognized correctly.
  let (tokens, hadError) = tokenize("false;")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.False, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Semicolon, &"tokens[1].kind is {tokens[1].kind}"

false_keyword_test()

proc none_keyword_test() =
  ## Test that none keyword is recognized correctly.
  let (tokens, hadError) = tokenize("none;")
  doAssert hadError == false, &"hadError is {hadError}"
  doAssert tokens.len == 2, &"tokens.len is {tokens.len}"
  doAssert tokens[0].kind == TK.NoneKeyword, &"tokens[0].kind is {tokens[0].kind}"
  doAssert tokens[1].kind == TK.Semicolon, &"tokens[1].kind is {tokens[1].kind}"

none_keyword_test()