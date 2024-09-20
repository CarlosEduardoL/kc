# **Grammar Design Notes for Kc**

## **Grammar Overview**

Kc is a simple statically typed programming language. It is strongly typed, meaning every variable, function, and expression must have an explicitly declared type. No implicit type conversions are allowed.

## **Types**

Kc supports the following types:

- **int**: integer type
- **float**: floating-point type
- **bool**: boolean type
- **string**: string type
- **array**: fixed-size array type
- **struct**: user-defined structure type
- **none**: represents no return value (used for functions without a return value)

### **Examples of Type Declarations**

```kc
var age: int = 25 
var name: string = "Carlos" 
var primeNumbers: array[10; int] = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
```

### **Struct Example**

```kc
struct Person = {   
  name: string,   
  age: int,   
  languages: array[10; string] 
}
```

### **Function Example**

```kc
func greet(person: Person): string {
  return "Hello, " + person.name 
}  
func log(message: string): none {
  # This function has no return value 
}
```

## **Literals**

Literals are written as:

- **Integers**: sequence of digits (e.g., `42`)
- **floats**: sequence of digits with a decimal point (e.g., `3.14`)
- **Booleans**: `true` or `false`
- **Characters**: single character surrounded by single quotes (e.g., `'a'`)
- **Strings**: sequence of characters surrounded by double quotes (e.g., `"hello"`)
- **Arrays**: sequence of values of the same type, enclosed in square brackets (e.g., `[1, 2, 3]`)
- **Structs**: key-type pairs surrounded by curly brackets (e.g., `{ name: "John", age: 30 }`)

## **Statements**

Each statement must be terminated by a newline. Multi-line statements are allowed using a backslash (`\`) to escape the newline.

## **Comments**

Comments begin with `#` and end at the next newline.

## **Identifiers**

Identifiers begin with a letter or underscore and can contain letters, digits, or underscores.

## **Keywords**

Kc has the following keywords:

- **var**: declare a variable
- **func**: declare a function
- **struct**: declare a structure
- **if**, **else**: conditional statements
- **while**: loop statement
- **for**: loop statement
- **return**: return a value from a function
- **break**: exit a loop
- **continue**: skip to the next iteration of a loop
- **true**, **false**: boolean literals
- **none**: for functions that do not return a value

Precedence and Associativity
In recursive descent parsing, the parser will respect operator precedence and associativity to correctly interpret expressions. Below are the precedence and associativity rules for Kc:

Operator Precedence and Associativity
Parentheses: ()
Highest precedence, used for grouping expressions.
Example: (a + b) * c

Unary Operators: -, !
Right-associative.
Example: -a, !b

Multiplicative Operators: *, /, %
Left-associative.
Example: a * b / c

Additive Operators: +, -
Left-associative.
Example: a + b - c

Relational Operators: <, <=, >, >=
Left-associative.
Example: a < b, a >= b

Equality Operators: ==, !=
Left-associative.
Example: a == b, a != b

Logical AND: &&
Left-associative.
Example: a && b

Logical OR: ||
Left-associative.
Example: a || b

Statement Precedence
Statements have the following precedence:

Block: { ... }
Highest precedence, contains zero or more statements. Blocks create a new scope.

Control Statements:
Includes if, else, while, for, and function calls.
These must have higher precedence than variable declarations and expressions.

Variable Declarations:
Declarations with var have lower precedence than control flow, but higher than expressions.

Return, Break, Continue:
Return statements must have the lowest precedence, as they can terminate execution within functions or loops.

## **Grammar Specification**

### Symbol Explanation:

- `()` means a group of symbols
- `|` means a choice of symbols
- `*` means repetition (zero or more occurrences)
- `+` means one or more occurrences

### Grammar:

```
program ::= statement*
statement ::= (declaration | assignment | expression | if | while | for | return | break | continue) ('\n' | '\\' '\n' | ';')

declaration ::= var | func | struct

var ::= 'var' identifier (':' type)? '=' expression
assignment ::= identifier '=' expression

func ::= 'func' identifier '(' param_list ')' ':' type block
param_list ::= identifier ':' type (',' identifier ':' type)*

struct ::= 'struct' identifier '{' struct_field_list '}'
struct_field_list ::= struct_field (',' struct_field)*
struct_field ::= identifier ':' type

expression ::= binary_expression | unary_expression | call_expression | identifier | literal | group_expression
group_expression ::= '(' expression ')'

binary_expression ::= expression binary_operator expression
unary_expression ::= unary_operator expression

call_expression ::= identifier '(' (expression (',' expression)*)? ')'

literal ::= integer | boolean | string | array_literal | struct_literal
array_literal ::= '[' (expression (',' expression)*)? ']'
struct_literal ::= '{' (struct_field_literal (',' struct_field_literal)*)? '}'
struct_field_literal ::= identifier ':' expression

binary_operator ::= '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '<=' | '>' | '>=' | '&&' | '||'
unary_operator ::= '-' | '!'

if ::= 'if' expression block ('else' block)?
while ::= 'while' expression block
for ::= 'for' identifier 'in' expression block

return ::= 'return' expression
break ::= 'break'
continue ::= 'continue'

block ::= '{' statement* '}'

type ::= 'int' | 'float' | 'bool' | char | 'string' | 'array' '[' integer ';' type ']' | 'struct' identifier | 'none'

identifier ::= letter (letter | digit | '_')*
integer ::= digit+
boolean ::= 'true' | 'false'
string ::= '"' character* '"'
char ::= '\'' character '\''
character ::= letter | digit | '_' | ' ' | '\n' | '\t' | '\r'
```

### Key Refinements:

- **Mandatory Types**: Every variable, parameter, and return value must have an explicit type.
- **Array Types**: Arrays are declared with a fixed size, `array[length; type]`.
- **None Type**: Introduced `none` type for functions that do not return a value.
- **Type Checking**: Binary and unary expressions are strongly typed, ensuring compatibility of operations.