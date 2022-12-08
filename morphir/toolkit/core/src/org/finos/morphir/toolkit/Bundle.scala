package org.finos.morphir
package toolkit

import ir.{Value => V, Type => T}
import V.Value
import zio._
import zio.prelude.fx._
import Bundle._

sealed trait Bundle {
  type TypeAttribs
  type ValueAttribs
}

object Bundle {}
