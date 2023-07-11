package org.finos.morphir.foundations.platform.services.internal.fs

import scala.scalajs.js 
import scala.scalajs.js.annotation.JSImport

@js.native
trait Fs extends js.Object {
  def existsSync(path: Path): Boolean = js.native
}

@js.native
@JSImport("fs", JSImport.Namespace)
object Fs extends Fs {}

