package org.finos.morphir
package runtime

import ir.Value.Value
import zio.*
import zio.prelude.fx.*

abstract class Kernel[+TA, +VA, +A] {
  def interpret[TA1 >: TA: Tag, VA1 >: VA: Tag](value: Value[TA1, VA1]): A
  def compile[TA1 >: TA: Tag, VA1 >: VA: Tag](value: Value[TA1, VA1]): Kernel.Compiled
}

object Kernel {
  type Compiled
  // def make[TA: Tag, VA: Tag]: UIO[Kernel[TA, VA]] =
  //   ZIO.succeed(new Kernel[TA, VA] {
  //     override def evaluationEngine[TA1 >: TA, VA1 >: VA]: EvaluationEngine[TA1, VA1] =
  //       // EvaluationEngine.make[TA1, VA1]
  //       ???
  //   })
}
