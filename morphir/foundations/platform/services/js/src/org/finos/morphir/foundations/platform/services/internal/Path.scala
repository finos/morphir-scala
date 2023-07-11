package org.finos.morphir.foundations.platform.services.internal

import scala.scalajs.js 
import scala.scalajs.js.annotation.JSImport

/**
  * This module contains utilities for handling and transforming file paths.
  * @see
  *   https://nodejs.org/docs/latest/api/path.html
  */
@js.native
trait Path extends js.Object {  
  /// The platform-specific file separator, '\\' or '/'.
  val sep: String = js.native
}

@js.native
@JSImport("path", JSImport.Namespace)
object Path extends Path
