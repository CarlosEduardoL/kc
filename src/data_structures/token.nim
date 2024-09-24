import position

type
  TK* {.pure.} = enum
    ## TokenKind enum represents all possible token types for Kc language.
    ## Used in lexical analysis to categorize tokens and guide parsing.
    # Special markers
    EOF

    # Literals
    Identifier # An identifier (variable/function name)
    Integer # An integer literal
    Float # A float literal
    String # A string literal
    Char # A character literal

    # Operators
    Plus # Addition operator (+)
    Minus # Subtraction operator (-)
    Star # Multiplication operator (*)
    Slash # Division operator (/)
    Percent # Modulus operator (%)
    EqualEqual # Equality comparison (==)
    NotEqual # Inequality comparison (!=)
    Less # Less than (<)
    LessEqual # Less than or equal to (<=)
    Greater # Greater than (>)
    GreaterEqual # Greater than or equal to (>=)
    And # Logical AND (&&)
    Or # Logical OR (||)
    Bang # Logical NOT (!)

    # Assignment operators
    Equal # Assignment operator (=)

    # Delimiters
    NewLine # New line (\n)
    Dot # Dot (.)
    Comma # Comma (,)
    Semicolon # Semicolon (;)
    Colon # Colon (:)
    LeftParen # Left parenthesis (() 
    RightParen # Right parenthesis ())
    LeftBracket # Left bracket ([)
    RightBracket # Right bracket (])
    LeftCurly # Left curly brace ({)
    RightCurly # Right curly brace (})

    # Keywords
    Var # 'var' keyword for variable declaration
    Func # 'func' keyword for function declaration
    Struct # 'struct' keyword for structure declaration
    If # 'if' keyword for conditional statement
    Elif # 'elif' keyword for alternate condition
    Else # 'else' keyword for alternate condition
    While # 'while' keyword for loop statement
    For # 'for' keyword for loop iteration
    Return # 'return' keyword for function return
    Break # 'break' keyword to exit a loop
    Continue # 'continue' keyword to skip to the next loop iteration
    True # Boolean literal 'true'
    False # Boolean literal 'false'
    NoneKeyword # 'none' keyword for no return type
    Array # 'array' keyword for array type
    In # 'in' keyword for range function

    # Primitive types
    IntType # 'int' type
    FloatType # 'float' type
    BoolType # 'bool' type
    StringType # 'string' type
    CharType # 'char' type

  TokenPosition* = FilePosition
    ## TokenPosition represents the position of a token in the source code.
    ## It contains the line number and column number of the token.

  Token* = object
    ## Token represents a lexical token in the Kc language.
    ## It contains the token type, value, and position information.
    case kind*: TK
    of Identifier:
      ident*: string
    of Integer:
      intVal*: int
    of Float:
      floatVal*: float
    of String:
      strVal*: string
    of Char:
      charVal*: char
    else:
      discard
    pos*: TokenPosition

import strformat

proc lexeme*(token: Token): string =
  ## Returns the lexeme of the token.
  case token.kind
  of Identifier:
    token.ident
  of Integer:
    $token.intVal
  of Float:
    $token.floatVal
  of String:
    &"\"{token.strVal}\""
  of Char:
    &"\'{token.charVal}\'"
  of Array:
    "array"
  of IntType:
    "int"
  of FloatType:
    "float"
  of BoolType:
    "bool"
  of StringType:
    "string"
  of CharType:
    "char"
  of In:
    "in"
  of NewLine:
    "'\\n'"
  of Plus:
    "'+'"
  of Minus:
    "'-'"
  of Star:
    "'*'"
  of Slash:
    "'/'"
  of Percent:
    "'%'"
  of EqualEqual:
    "'=='"
  of NotEqual:
    "'!='"
  of Less:
    "'<'"
  of LessEqual:
    "'<='"
  of Greater:
    "'>'"
  of GreaterEqual:
    "'>='"
  of And:
    "'&&'"
  of Or:
    "'||'"
  of Bang:
    "'!'"
  of Equal:
    "'='"
  of Comma:
    "','"
  of Dot:
    "'.'"
  of Semicolon:
    "';'"
  of Colon:
    "':'"
  of LeftParen:
    "'('"
  of RightParen:
    "')'"
  of LeftBracket:
    "'['"
  of RightBracket:
    "']'"
  of LeftCurly:
    "'{'"
  of RightCurly:
    "'}'"
  of Var:
    "var"
  of Func:
    "func"
  of Struct:
    "struct"
  of If:
    "if"
  of Elif:
    "elif"
  of Else:
    "else"
  of While:
    "while"
  of For:
    "for"
  of Return:
    "return"
  of Break:
    "break"
  of Continue:
    "continue"
  of True:
    "true"
  of False:
    "false"
  of NoneKeyword:
    "none"
  of EOF:
    "<eof>"

proc `$`*(pos: TokenPosition): string =
  ## Returns a string representation of the token position.
  &"{pos.line}:{pos.column}"

proc `$`*(token: Token): string =
  ## Returns a string representation of the token.
  &"{token.kind}({token.lexeme})"

proc `==`*(a, b: Token): bool =
  ## Returns true if the two tokens are equal.
  a.kind == b.kind and a.lexeme == b.lexeme and a.pos == b.pos
