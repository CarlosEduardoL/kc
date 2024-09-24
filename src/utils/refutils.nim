proc asRef*[T](t: T): ref T =
  ## asRef promotes the given value to a gc-managed reference.
  var temp: ref T = new T
  temp[] = t
  return temp