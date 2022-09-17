package org.finos.morphir.mir
import org.finos.morphir.mir.Type.UType

trait InferredTypeOf[A] {
  def inferredType(value: A): UType
}

object InferredTypeOf {
  def apply[A](implicit ev: InferredTypeOf[A]): InferredTypeOf[A] = ev
}
