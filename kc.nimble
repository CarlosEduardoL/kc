# Package

version       = "0.1.0"
author        = "Carlos Lizalda"
description   = "A small toy Programing language"
license       = "MIT"
srcDir        = "src"
bin           = @["kc"]


# Dependencies

requires "nim >= 2.0.8"

task test, "Runs the test suite":
  exec "testament pattern \"tests/*/*.nim\""