package org.finos.morphir.mill.elm

import mill.*

trait MorphirElmModule extends Module {
  def make: T[PathRef] = Task {
    val outputFolder = Task.dest
    val path         = outputFolder / "morphir-ir.json"
    os.makeDir.all(outputFolder)
    os.write.over(path, "{}")
    PathRef(path)
  }
}
