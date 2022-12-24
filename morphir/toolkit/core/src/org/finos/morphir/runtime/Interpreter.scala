package org.finos.morphir
package runtime

import ir.Value.Value
import org.finos.morphir.toolkit.{EngineEvent, KernelError}
import zio.prelude.fx.*
import zio.*

trait Interpreter[+TA, +VA, +A] {
  type Ctx

  def interpret[TA1 >: TA: Tag, VA1 >: VA: Tag](
      value: Value[TA1, VA1]
  ): ZPure[EngineEvent, Ctx, Ctx, Any, KernelError, A]
}

object Interpreter {
  def interpretZIO[TA: Tag, VA: Tag](value: Value[TA, VA]) = ???
}
