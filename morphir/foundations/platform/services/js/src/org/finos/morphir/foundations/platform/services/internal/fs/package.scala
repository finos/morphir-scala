package org.finos.morphir.foundations.platform.services.internal

import scala.scalajs.js
import scala.scalajs.js.{typedarray, |}
import url.URL

package object fs {
  type Path = typedarray.Uint8Array | URL | String
}
