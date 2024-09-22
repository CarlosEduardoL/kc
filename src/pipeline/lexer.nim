import ../data_structures/token
import ../error
import tables
import strutils
import strformat

type 
  Lexer* = ref object
    ## Lexer is responsible for breaking the source code into tokens.
    ## It reads the source code character by character and produces a stream of tokens.
    source*: string
    index*: int
    pos*: TokenPosition
    current*: char
    reporter: ErrorReporter

const Keywords: Table[string, TK] = toTable([
  ("var", TK.Var),
  ("func", TK.Func),
  ("struct", TK.Struct),
  ("if", TK.If),
  ("elif", TK.Elif),
  ("else", TK.Else),
  ("while", TK.While),
  ("for", TK.For),
  ("return", TK.Return),
  ("break", TK.Break),
  ("continue", TK.Continue),
  ("true", TK.True),
  ("false", TK.False),
  ("none", TK.NoneKeyword),
  ("int", TK.IntType),
  ("float", TK.FloatType),
  ("bool", TK.BoolType),
  ("string", TK.StringType),
  ("array", TK.Array),
  ("char", TK.CharType)
])

proc newLexer*(reporter: ErrorReporter): Lexer {.inline.} =
  ## Creates a new Lexer object with the given error reporter.
  Lexer(reporter: reporter)

proc peek(lexer: Lexer): char {.inline.} =
  ## Returns the next character in the source code without advancing the position.
  lexer.current

proc peekNext(lexer: Lexer): char {.inline.} =
  ## Returns the next character in the source code without advancing the position.
  if lexer.index + 1 < lexer.source.len:
    lexer.source[lexer.index + 1]
  else:
    '\0'

proc prev(lexer: Lexer): char {.inline.} =
  ## Returns the previous character in the source code without advancing the position.
  if lexer.index > 0:
    lexer.source[lexer.index - 1]
  else:
    '\0'

proc isAtEnd(lexer: Lexer): bool =
  ## Returns true if the end of the source code has been reached.
  lexer.index >= lexer.source.len

proc advance(lexer: Lexer) =
  ## Advances the position in the source code by one character.
  if lexer.current == '\n':
    lexer.pos.line += 1
    lexer.pos.column = 1
  else:
    lexer.pos.column += 1
  lexer.index += 1
  if not lexer.isAtEnd():
    lexer.current = lexer.source[lexer.index]

proc skipWhitespace(lexer: Lexer) =
  ## Skips whitespace characters in the source code.
  while lexer.current in {' ', '\t', '\r', '\\'}:
    lexer.advance()

proc skipComment(lexer: Lexer) =
  ## Skips a comment in the source code.
  while lexer.current != '\n' and not lexer.isAtEnd():
    lexer.advance()

proc makeToken(pos: TokenPosition, kind: static[TK]): Token =
  ## Creates a new token with the given kind and value.
  result = Token(kind: kind, pos: pos)
  when kind == Identifier or kind == Integer or kind == Float or kind == String or kind == Char:
    {.error: "This function is not supposed to be used for these token kinds".}


template makeToken(kind: TK): Token =
  makeToken(lexer.pos, kind)

proc makeIdentifier(lexer: Lexer): Token =
  let start = lexer.index
  let pos = lexer.pos
  while not lexer.isAtEnd() and lexer.peek() in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
    lexer.advance()
  let identifier = lexer.source[start .. lexer.index - 1]
  if Keywords.hasKey(identifier):
    result = Token(kind: Keywords[identifier], pos: pos)
    return
  result = Token(kind: Identifier, ident: identifier, pos: pos)

proc makeNumber(lexer: Lexer): Token =
  let start = lexer.index
  let pos = lexer.pos
  while not lexer.isAtEnd() and lexer.peek() in {'0'..'9'}:
    lexer.advance()
  if not lexer.isAtEnd() and lexer.peek() == '.' and lexer.peekNext() in {'0'..'9'}:
    lexer.advance()
    while not lexer.isAtEnd() and lexer.peek() in {'0'..'9'}:
      lexer.advance()
    result = Token(kind: Float, floatVal: parseFloat(lexer.source[start .. lexer.index - 1]), pos: pos)
  else:
    result = Token(kind: Integer, intVal: parseInt(lexer.source[start .. lexer.index - 1]), pos: pos)

proc makeString(lexer: Lexer): Token =
  let start = lexer.index
  let pos = lexer.pos
  lexer.advance() # Skip the opening quote
  while not lexer.isAtEnd() and lexer.peek() != '"':
    lexer.advance()
  if lexer.isAtEnd() or lexer.peek() != '"':
    lexer.reporter.reportError(LexicalError, "Unterminated string literal", lexer.pos)
    return Token(kind: String, strVal: lexer.source[start + 1 .. lexer.index - 2], pos: pos)
  lexer.advance() # Skip the closing quote
  result = Token(kind: String, strVal: lexer.source[start + 1 .. lexer.index - 2], pos: pos)

proc tokenize*(lexer: Lexer, source: string): seq[Token] =
  ## Tokenizes the source code and returns a sequence of tokens.
  
  # Reset the lexer's state
  lexer.pos = TokenPosition(line: 1, column: 1)
  lexer.source = source
  lexer.current = source[0]
  lexer.index = 0

  result = @[] # Initialize the token sequence

  while not lexer.isAtEnd():
    case lexer.peek():
    of '#':
      lexer.skipComment()
    of ' ', '\t', '\r', '\\':
      lexer.skipWhitespace()
    of '\n':
      if lexer.prev() != '\\':
        # Implicit semicolon insertion
        result.add(makeToken(TK.NewLine)) 
      lexer.advance()
    of 'a'..'z', 'A'..'Z', '_':
      result.add(lexer.makeIdentifier())
    of '0'..'9':
      result.add(lexer.makeNumber())
    of '"':
      result.add(lexer.makeString())
    of ';':
      result.add(makeToken(TK.Semicolon))
      lexer.advance()
    of ':':
      result.add(makeToken(TK.Colon))
      lexer.advance()
    of ',':
      result.add(makeToken(TK.Comma))
      lexer.advance()
    of '.':
      result.add(makeToken(TK.Dot))
      lexer.advance()
    of '(':
      result.add(makeToken(TK.LeftParen))
      lexer.advance()
    of ')':
      result.add(makeToken(TK.RightParen))
      lexer.advance()
    of '[':
      result.add(makeToken(TK.LeftBracket))
      lexer.advance()
    of ']':
      result.add(makeToken(TK.RightBracket))
      lexer.advance()
    of '{':
      result.add(makeToken(TK.LeftCurly))
      lexer.advance()
    of '}':
      result.add(makeToken(TK.RightCurly))
      lexer.advance()
    of '+':
      result.add(makeToken(TK.Plus))
      lexer.advance()
    of '-':
      result.add(makeToken(TK.Minus))
      lexer.advance()
    of '*':
      result.add(makeToken(TK.Star,))
      lexer.advance()
    of '/':
      result.add(makeToken(TK.Slash))
      lexer.advance()
    of '%':
      result.add(makeToken(TK.Percent))
      lexer.advance()
    of '=':
      if not lexer.isAtEnd() and lexer.peekNext() == '=':
        result.add(makeToken(TK.EqualEqual))
        lexer.advance()
      else:
        result.add(makeToken(TK.Equal))
      lexer.advance() # Advance the '=' character
    of '!':
      if not lexer.isAtEnd() and lexer.peekNext() == '=':
        result.add(makeToken(TK.NotEqual))
        lexer.advance()
      else:
        result.add(makeToken(TK.Bang))
      lexer.advance() # Advance the '!' character
    of '<':
      if not lexer.isAtEnd() and lexer.peekNext() == '=':
        result.add(makeToken(TK.LessEqual))
        lexer.advance()
      else:
        result.add(makeToken(TK.Less))
      lexer.advance() # Advance the '<' character
    of '>':
      if not lexer.isAtEnd() and lexer.peekNext() == '=':
        result.add(makeToken(TK.GreaterEqual))
        lexer.advance()
      else:
        result.add(makeToken(TK.Greater))
      lexer.advance() # Advance the '>' character
    of '&':
      if not lexer.isAtEnd() and lexer.peekNext() == '&':
        result.add(makeToken(TK.And))
        lexer.advance()
      else:
        lexer.reporter.reportError(LexicalError, "A single '&' is not allowed", lexer.pos)
      lexer.advance() # Advance the '&' character
    of '|':
      if not lexer.isAtEnd() and lexer.peekNext() == '|':
        result.add(makeToken(TK.Or))
        lexer.advance()
      else:
        lexer.reporter.reportError(LexicalError, "A single '|' is not allowed", lexer.pos)
      lexer.advance() # Advance the '|' character
    else: 
      lexer.reporter.reportError(LexicalError, &"Unexpected character '{lexer.current}'", lexer.pos)
      lexer.advance()

  result.add(makeToken(TK.EOF))