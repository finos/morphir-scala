package org.finos.morphir.runtime.services.kernel

import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.universe.ir.Name
import zio.prelude.fx.ZPure

trait Kernel {
  type VariableRef = Any // TODO: Replace with appropriate type
  def accessVariable(name: Name): RTAction[Any, VariableAccessError, VariableRef]
}

object Kernel {
  val live: Kernel = KernelLive()

  def accessVariable(name: Name): RTAction[Kernel, VariableAccessError, Any] =
    RTAction.serviceWith(_.accessVariable(name))

}

final case class KernelLive() extends Kernel {

  def accessVariable(name: Name): RTAction[Any, VariableAccessError, VariableRef] = ???

}
