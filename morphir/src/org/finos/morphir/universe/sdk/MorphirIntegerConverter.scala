package org.finos.morphir.universe.sdk
import org.finos.morphir.universe.sdk.Basics

trait ToMorphirInteger[-A] {
  def toMorphirInteger(value: A): Basics.Integer
}

object ToMorphirInteger {
  def apply[A](implicit converter: ToMorphirInteger[A]): ToMorphirInteger[A] = converter

  implicit val intToMorphirInteger: ToMorphirInteger[Int] = new ToMorphirInteger[Int] {
    override def toMorphirInteger(value: Int): Basics.Integer = Basics.Integer.fromInt(value)
  }
}

trait FromMorphirInteger[+A] {
  def fromMorphirInteger(value: Basics.Integer): A
}

object FromMorphirInteger {
  def apply[A](implicit converter: FromMorphirInteger[A]): FromMorphirInteger[A] = converter
}

trait MorphirIntegerConverter[A] extends ToMorphirInteger[A] with FromMorphirInteger[A]

object MorphirIntegerConverter {
  def apply[A](implicit converter: MorphirIntegerConverter[A]): MorphirIntegerConverter[A] = converter
}

trait MorphirIntegerConverterLowPriorityInstances {
  implicit def fallbackMorphirIntegerConverter[A](implicit
      toInteger: ToMorphirInteger[A],
      fromInteger: FromMorphirInteger[A]
  ): MorphirIntegerConverter[A] =
    new MorphirIntegerConverter[A] {
      override def toMorphirInteger(value: A): Basics.Integer   = toInteger.toMorphirInteger(value)
      override def fromMorphirInteger(value: Basics.Integer): A = fromInteger.fromMorphirInteger(value)
    }
}
