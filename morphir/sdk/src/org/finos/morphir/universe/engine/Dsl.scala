package org.finos.morphir.universe.engine

import org.finos.morphir.datamodel.DataEncoder
import org.finos.morphir.foundations.capabilities.free.Free
import org.finos.morphir.universe.ir.{Name, Type}

object Dsl {
  def subscribe[Msg](
      subscription: Subscription[Msg],
      encoder: DataEncoder[Msg]
  )(handler: Msg => Any): Instruction[Nothing, Boolean] =
    Free.eval(Instr.Subscribe(subscription, encoder, handler))

  def receiveMessage[Msg](subscription: Subscription[Msg], message: Msg): Instruction[Nothing, Unit] =
    Free.eval(Instr.ReceiveMessage(subscription, message))

  def readVariable(name: Name): Instruction[Throwable, (Type[scala.Unit], Any)] =
    Free.eval(Instr.ReadVariable(name))
}
