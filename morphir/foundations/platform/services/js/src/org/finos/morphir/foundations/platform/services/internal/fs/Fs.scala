package org.finos.morphir.foundations.platform.services.internal.fs

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
trait Fs extends js.Object {
  def existsSync(path: Path): Boolean = js.native
  def mkdtempSync(prefix: String): String = js.native
  def mkdtempSync(prefix: String, options:FsMkTempDirOptions): String = js.native
}

@js.native
@JSImport("fs", JSImport.Namespace)
object Fs extends Fs {}

trait FsMkTempDirOptions extends js.Object {  
  val encoding: js.UndefOr[String] = js.undefined
}

object FsMkTempDirOptions {
  def apply(
      encoding: js.UndefOr[String] = js.undefined
  ): FsMkTempDirOptions = {
    val _obj$ = js.Dynamic.literal(
    )
    encoding.foreach(_v => _obj$.updateDynamic("encoding")(_v.asInstanceOf[js.Any]))
    _obj$.asInstanceOf[FsMkTempDirOptions]
  }
}
