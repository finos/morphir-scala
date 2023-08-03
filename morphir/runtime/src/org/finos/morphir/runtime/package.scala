package org.finos.morphir
import zio.prelude.fx.ZPure

package object runtime {
  type RT[+E, +A] = ZPure[Nothing, Unit, Unit, Any, E, A]
  val RT: zio.prelude.fx.ZPure.type = zio.prelude.fx.ZPure
}
