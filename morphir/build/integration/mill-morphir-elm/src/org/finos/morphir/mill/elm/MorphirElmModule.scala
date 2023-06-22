package org.finos.morphir.mill.elm
import mill._
import ujson.default._

trait MorphirElmModule extends Module {
  def make: T[PathRef] = T {

    val outputFolder = T.ctx().dest
    val path = outputFolder / "morphir-ir.json"    
    os.makeDir.all(outputFolder)
    ujson.write(path.toIO, ujson.Obj())
    PathRef(path)
  }  
}
