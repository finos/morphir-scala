package org.finos.morphir.universe

//TODO: Evaluate if still needed
trait MorphirNumber[A] {}

object MorphirNumber {
  def apply[A](implicit ev: MorphirNumber[A]): MorphirNumber[A] = ev
}
