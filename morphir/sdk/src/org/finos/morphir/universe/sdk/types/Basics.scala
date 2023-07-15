package org.finos.morphir.universe.sdk.types

import spire.math.SafeLong
import zio.prelude.*

object Basics {
  type Integer = Integer.Type

  object Integer extends Subtype[SafeLong] {}

  type Float = Float.Type
  object Float extends Subtype[scala.Double] {}
}
