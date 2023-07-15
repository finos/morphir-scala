package org.finos.morphir.universe.sdk.types

trait MorphirNumber[A] {}

object MorphirNumber {
  def apply[A](implicit ev: MorphirNumber[A]): MorphirNumber[A] = ev
}
