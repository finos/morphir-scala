package org.finos.morphir.universe.engine

import org.finos.morphir.datamodel.DataEncoder
import org.finos.morphir.core.capabilities.free.Free
import org.finos.morphir.universe.ir.{Name, Type}
import org.finos.morphir.universe.sdk.types.Basics.{Integer as MInteger, Float as MFloat}

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

  object Morphir {
    object SDK {
      object Basics {
        def add(a: MInteger, b: MInteger): Instruction[Nothing, MInteger]      = Free.eval(Instr.AddI(a, b))
        def add(a: MFloat, b: MFloat): Instruction[Nothing, MFloat]            = Free.eval(Instr.AddF(a, b))
        def multiply(a: MInteger, b: MInteger): Instruction[Nothing, MInteger] = Free.eval(Instr.MultiplyI(a, b))
        def multiply(a: MFloat, b: MFloat): Instruction[Nothing, MFloat]       = Free.eval(Instr.MultiplyF(a, b))
      }
    }
  }
}
