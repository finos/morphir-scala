package org.finos.morphir
package toolkit

import ir.{Value => V, Type => T}
import V.Value
import zio._
import zio.prelude.fx._

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


