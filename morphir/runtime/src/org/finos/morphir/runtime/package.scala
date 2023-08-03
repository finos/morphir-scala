package org.finos.morphir
import zio.prelude.fx.ZPure

package object runtime {
  type RuntimeOp[+E, +A] = ZPure[Nothing, Unit, Unit, Any, E, A]
  val RuntimeOp: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure
}
