type FilePosition* = object
  ## FilePosition represents the position of a file in the source code.
  ## It contains the line number and column number of the file.
  line*: int
  column*: int

proc pos*(line, column: int): FilePosition =
  ## pos returns a new FilePosition object with the given line and column.
  FilePosition(line: line, column: column)