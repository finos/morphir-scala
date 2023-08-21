package org.finos.morphir.foundations.platform.services.internal.path

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.annotation.nowarn

/**
 * This module contains utilities for handling and transforming file paths.
 * @see
 *   https://nodejs.org/docs/latest/api/path.html
 */
@js.native
trait Path extends js.Object {
  /// The platform-specific file delimiter, ';' or ':'.
  @nowarn val delimiter: String = js.native

  /// The platform-specific file separator, '\\' or '/'.
  @nowarn val sep: String = js.native

  /// Returns an object from a path.
  def parse(path: String): PathObj = js.native
}

@js.native
@JSImport("path", JSImport.Namespace)
object Path extends Path
