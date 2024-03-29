package org.finos.morphir.ir
import org.finos.morphir.ir.Type.UType

trait InferredTypeOf[A] {
  def inferredType(value: A): UType
}

object InferredTypeOf {
  def apply[A](implicit ev: InferredTypeOf[A]): InferredTypeOf[A] = ev
}
