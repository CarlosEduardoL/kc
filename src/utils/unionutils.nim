template checked*(kind: untyped, expected: untyped, body: untyped): untyped =
  ## Checks if the given kind is equal to the expected kind.
  ## If it is, executes the body.
  ## If not Just does nothing.
  case kind
  of expected:
    body
  else:
    discard