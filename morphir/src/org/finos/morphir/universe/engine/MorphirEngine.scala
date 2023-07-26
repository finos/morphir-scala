package org.finos.morphir.universe.engine

import org.finos.morphir.core.capabilities.free.Free
import scala.collection.mutable
import org.finos.morphir.universe.ir.Type
import org.finos.morphir.universe.sdk.types.Basics.{Integer as MInteger, Float as MFloat}

object MorphirEngine {
  def unsafe(): Free.UnsafeInterpreter[Instr] = {
    import Instr.*
    val subscriptions: mutable.Map[Subscription[_], Any] = mutable.Map.empty

    new Free.UnsafeInterpreter[Instr] {

      override def interpret[E, A](fa: Instr[E, A]): Either[E, A] = fa match {
        case sub @ Subscribe(subscription, encoder, handler) =>
          pprint.pprintln(sub)
          Right(true)
        case ReceiveMessage(subscription, message) =>
          Right(())
        case ReadVariable(name) => Right((Type.Unit(()), 42))
        case AddI(a, b)         => Right(MInteger(a.value + b.value))
        case AddF(a, b)         => Right(MFloat(a.value + b.value))
        case MultiplyI(a, b)    => Right(MInteger(a.value * b.value))
        case MultiplyF(a, b)    => Right(MFloat(a.value * b.value))
      }

    }
  }
}
