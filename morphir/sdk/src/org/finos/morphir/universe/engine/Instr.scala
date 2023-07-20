package org.finos.morphir.universe.engine

import org.finos.morphir.datamodel.DataEncoder
import org.finos.morphir.universe.ir.{Name, Type}

trait Instr[+E, +A] { self => }

object Instr {

  // TODO: In order to be meaningful the handler must be allowed to access some sort of state/context
  final case class Subscribe[Msg](subscription: Subscription[Msg], encoder: DataEncoder[Msg], handler: Msg => Any)
      extends Instr[Nothing, Boolean]
  final case class ReceiveMessage[Msg](subscription: Subscription[Msg], message: Msg)
      extends Instr[Nothing, Unit]

  final case class ReadVariable(name: Name) extends Instr[Throwable, (Type[scala.Unit], Any) /*TODO: Return a Value*/ ]
}
