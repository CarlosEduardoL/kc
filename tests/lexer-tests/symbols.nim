import ../../src/pipeline/lexer
import ../../src/data_structures/token
import strformat

let source = "= == ! != < > <= >= , ; : ( ) { } [ ] ^ || &&"
var hadError = false
let my_lexer = newLexer(source)
let tokens = my_lexer.tokenize(hadError)
echo tokens
doAssert hadError == true, &"hadError is {hadError}"
doAssert tokens.len == 20, &"tokens.len is {tokens.len}"
doAssert tokens[0].kind == TK.Equal, &"tokens[0].kind is {tokens[0].kind}"
doAssert tokens[1].kind == TK.EqualEqual, &"tokens[1].kind is {tokens[1].kind}"
doAssert tokens[2].kind == TK.Bang, &"tokens[2].kind is {tokens[2].kind}"
doAssert tokens[3].kind == TK.NotEqual, &"tokens[3].kind is {tokens[3].kind}"
doAssert tokens[4].kind == TK.Less, &"tokens[4].kind is {tokens[4].kind}"
doAssert tokens[5].kind == TK.Greater, &"tokens[5].kind is {tokens[5].kind}"
doAssert tokens[6].kind == TK.LessEqual, &"tokens[6].kind is {tokens[6].kind}"
doAssert tokens[7].kind == TK.GreaterEqual, &"tokens[7].kind is {tokens[7].kind}"
doAssert tokens[8].kind == TK.Comma, &"tokens[8].kind is {tokens[8].kind}"
doAssert tokens[9].kind == TK.Semicolon, &"tokens[9].kind is {tokens[9].kind}"
doAssert tokens[10].kind == TK.Colon, &"tokens[10].kind is {tokens[10].kind}"
doAssert tokens[11].kind == TK.LeftParen, &"tokens[11].kind is {tokens[11].kind}"
doAssert tokens[12].kind == TK.RightParen, &"tokens[12].kind is {tokens[12].kind}"
doAssert tokens[13].kind == TK.LeftCurly, &"tokens[13].kind is {tokens[13].kind}"
doAssert tokens[14].kind == TK.RightCurly, &"tokens[14].kind is {tokens[14].kind}"
doAssert tokens[15].kind == TK.LeftBracket, &"tokens[15].kind is {tokens[15].kind}"
doAssert tokens[16].kind == TK.RightBracket, &"tokens[16].kind is {tokens[16].kind}"
doAssert tokens[17].kind == TK.Error, &"tokens[17].kind is {tokens[17].kind}"
doAssert tokens[17].errorMsg == "Unexpected character '^'"
doAssert tokens[18].kind == TK.Or, &"tokens[18].kind is {tokens[18].kind}"
doAssert tokens[19].kind == TK.And, &"tokens[19].kind is {tokens[19].kind}"