template doWhile*(condition: bool, body: untyped): untyped =
  ## doWhileis exactly like while, but it also executes the body the first time no matter what the condition is.
  body
  while true:
    if not condition:
      break
    body