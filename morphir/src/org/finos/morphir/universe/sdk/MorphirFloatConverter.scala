package org.finos.morphir.universe.sdk

import org.finos.morphir.universe.sdk.Basics

trait ToMorphirFloat[-A] {
  def toMorphirFloat(value: A): Basics.Float
}

object ToMorphirFloat {
  def apply[A](implicit converter: ToMorphirFloat[A]): ToMorphirFloat[A] = converter

  implicit val doubleToMorphirFloat: ToMorphirFloat[Double] = new ToMorphirFloat[Double] {
    override def toMorphirFloat(value: Double): Basics.Float = Basics.Float.fromDouble(value)
  }
}

trait FromMorphirFloat[+A] {
  def fromMorphirFloat(value: Basics.Float): A
}

trait MorphirFloatConverter[A] extends ToMorphirFloat[A] with FromMorphirFloat[A]
object MorphirFloatConverter {
  def apply[A](implicit converter: MorphirFloatConverter[A]): MorphirFloatConverter[A] = converter
}

trait MorphirFloatConverterLowPriorityInstances {
  implicit def fallbackMorphirFloatConverter[A](implicit
      toFloat: ToMorphirFloat[A],
      fromFloat: FromMorphirFloat[A]
  ): MorphirFloatConverter[A] =
    new MorphirFloatConverter[A] {
      override def toMorphirFloat(value: A): Basics.Float   = toFloat.toMorphirFloat(value)
      override def fromMorphirFloat(value: Basics.Float): A = fromFloat.fromMorphirFloat(value)
    }
}
