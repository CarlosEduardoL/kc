import ../../src/pipeline/lexer
import ../../src/data_structures/token
import strformat

let source = """fibo(n: int): int {
  if n <= 1 {
    return n
  } else {
    return fibo(n - 1) + fibo(n - 2)
  }
}
fibo(10)
"""

var hadError = false
let my_lexer = newLexer(source)
let tokens = my_lexer.tokenize(hadError)
doAssert hadError == false, &"hadError is {hadError}"
let expectedTokens = @[
  Token(kind: TK.Identifier, ident: "fibo", pos: TokenPosition(line: 1, column: 1)),
  Token(kind: TK.LeftParen, pos: TokenPosition(line: 1, column: 5)),
  Token(kind: TK.Identifier, ident: "n", pos: TokenPosition(line: 1, column: 6)),
  Token(kind: TK.Colon, pos: TokenPosition(line: 1, column: 7)),
  Token(kind: TK.IntType, pos: TokenPosition(line: 1, column: 9)),
  Token(kind: TK.RightParen, pos: TokenPosition(line: 1, column: 12)),
  Token(kind: TK.Colon, pos: TokenPosition(line: 1, column: 13)),
  Token(kind: TK.IntType, pos: TokenPosition(line: 1, column: 15)),
  Token(kind: TK.LeftCurly, pos: TokenPosition(line: 1, column: 19)),
  Token(kind: TK.NewLine, pos: TokenPosition(line: 1, column: 20)),
  Token(kind: TK.If, pos: TokenPosition(line: 2, column: 3)),
  Token(kind: TK.Identifier, ident: "n", pos: TokenPosition(line: 2, column: 6)),
  Token(kind: TK.LessEqual, pos: TokenPosition(line: 2, column: 8)),
  Token(kind: TK.Integer, intVal: 1, pos: TokenPosition(line: 2, column: 11)),
  Token(kind: TK.LeftCurly, pos: TokenPosition(line: 2, column: 13)),
  Token(kind: TK.NewLine, pos: TokenPosition(line: 2, column: 14)),
  Token(kind: TK.Return, pos: TokenPosition(line: 3, column: 5)),
  Token(kind: TK.Identifier, ident: "n", pos: TokenPosition(line: 3, column: 12)),
  Token(kind: TK.NewLine, pos: TokenPosition(line: 3, column: 13)),
  Token(kind: TK.RightCurly, pos: TokenPosition(line: 4, column: 3)),
  Token(kind: TK.Else, pos: TokenPosition(line: 4, column: 5)),
  Token(kind: TK.LeftCurly, pos: TokenPosition(line: 4, column: 10)),
  Token(kind: TK.NewLine, pos: TokenPosition(line: 4, column: 11)),
  Token(kind: TK.Return, pos: TokenPosition(line: 5, column: 5)),
  Token(kind: TK.Identifier, ident: "fibo", pos: TokenPosition(line: 5, column: 12)),
  Token(kind: TK.LeftParen, pos: TokenPosition(line: 5, column: 16)),
  TOken(kind: TK.Identifier, ident: "n", pos: TokenPosition(line: 5, column: 17)),
  Token(kind: TK.Minus, pos: TokenPosition(line: 5, column: 19)),
  Token(kind: TK.Integer, intVal: 1, pos: TokenPosition(line: 5, column: 21)),
  Token(kind: TK.RightParen, pos: TokenPosition(line: 5, column: 22)),
  Token(kind: TK.Plus, pos: TokenPosition(line: 5, column: 24)),
  Token(kind: TK.Identifier, ident: "fibo", pos: TokenPosition(line: 5, column: 26)),
  Token(kind: TK.LeftParen, pos: TokenPosition(line: 5, column: 30)),
  Token(kind: TK.Identifier, ident: "n", pos: TokenPosition(line: 5, column: 31)),
  Token(kind: TK.Minus, pos: TokenPosition(line: 5, column: 33)),
  Token(kind: TK.Integer, intVal: 2, pos: TokenPosition(line: 5, column: 35)),
  Token(kind: TK.RightParen, pos: TokenPosition(line: 5, column: 36)),
  Token(kind: TK.NewLine, pos: TokenPosition(line: 5, column: 37)),
  Token(kind: TK.RightCurly, pos: TokenPosition(line: 6, column: 3)),
  Token(kind: TK.NewLine, pos: TokenPosition(line: 6, column: 4)),
  Token(kind: TK.RightCurly, pos: TokenPosition(line: 7, column: 1)),
  Token(kind: TK.NewLine, pos: TokenPosition(line: 7, column: 2)),
  Token(kind: TK.Identifier, ident: "fibo", pos: TokenPosition(line: 8, column: 1)),
  Token(kind: TK.LeftParen, pos: TokenPosition(line: 8, column: 5)),
  Token(kind: TK.Integer, intVal: 10, pos: TokenPosition(line: 8, column: 6)),
  Token(kind: TK.RightParen, pos: TokenPosition(line: 8, column: 8)),
  Token(kind: TK.NewLine, pos: TokenPosition(line: 8, column: 9))
]
doAssert tokens.len == expectedTokens.len, &"tokens.len is {tokens.len}, expected {expectedTokens.len}"
for i in 0 ..< expectedTokens.len:
  echo tokens[i]
  doAssert tokens[i] == expectedTokens[i], &"tokens[i] is '{tokens[i]}', expected '{expectedTokens[i]}'"