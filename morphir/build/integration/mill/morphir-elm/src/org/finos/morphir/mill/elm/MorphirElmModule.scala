package org.finos.morphir.mill.elm
import mill._

trait MorphirElmModule extends Module {
  def make: T[PathRef] = T {

    val outputFolder = T.ctx().dest
    val path         = outputFolder / "morphir-ir.json"
    os.makeDir.all(outputFolder)
    os.write.over(path, "{}")
    PathRef(path)
  }
}
