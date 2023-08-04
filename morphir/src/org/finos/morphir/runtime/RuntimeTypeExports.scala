package org.finos.morphir.runtime

import zio.prelude.*
import zio.prelude.fx.ZPure

object exports {

  type RTAction[-R, +E, +A] = ZPure[Nothing, RTExecutionContext, RTExecutionContext, R, E, A]
  val RTAction: ZPure.type = ZPure

  type URTAction[+A] = RTAction[Any, Nothing, A]
  val URTAction: ZPure.type = ZPure
}
