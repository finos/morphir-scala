package org.finos.morphir.foundations.platform.services.internal
import path.Path
object PathModule extends PathApi {
  def delimiter: String = Path.delimiter
  def sep: String       = Path.sep
}
