package org.finos.morphir
package toolkit
package runtime

import ir.{Value => V, Type => T}
import V.Value
import zio.prelude.fx._
import zio._

trait Interpreter[+TA, +VA, +A] {
  type Ctx

  def interpret[TA1 >: TA: Tag, VA1 >: VA: Tag](
      value: Value[TA1, VA1]
  ): ZPure[EngineEvent, Ctx, Ctx, Any, KernelError, A]
}

object Interpreter {
  def interpretZIO[TA: Tag, VA: Tag](value: Value[TA, VA]) = ???
}
