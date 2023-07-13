package org.finos.morphir.foundations.platform.services.internal.os

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
trait Os extends js.Object {
  /// Returns a string identifying the operating system platform for which the Node.js binary was compiled. The value is set at compile time. Possible values are 'aix', 'darwin', 'freebsd', 'linux', 'openbsd', 'sunos', and 'win32'.
  def platform(): String = js.native

  /// Returns the operating system's default directory for temporary files as a string.
  def tmpdir(): String = js.native
}

@js.native
@JSImport("os", JSImport.Namespace)
object Os extends Os {}
