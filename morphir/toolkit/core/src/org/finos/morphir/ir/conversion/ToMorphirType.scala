package org.finos.morphir
package ir
package conversion

import org.finos.morphir.datamodel.{Concept, Label}
import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.Type.{Type, UType}

import scala.collection.immutable.{Map, Set}

trait ToMorphirType[A, +Attribs] { self =>
  def apply: Type[Attribs]
  final def morphirType: Type[Attribs] = apply

  def as[B]: ToMorphirType[B, Attribs] = new ToMorphirType[B, Attribs] {
    override def apply: Type[Attribs] = self.apply
  }
}

object ToMorphirType {
  def apply[A, Attribs](implicit toMorphirType: ToMorphirType[A, Attribs]): ToMorphirType[A, Attribs] = toMorphirType
  def summon[A]: SummonPartiallyApplied[A] = new SummonPartiallyApplied[A]

  def toUTypeConverter[A](f: => UType): ToMorphirUType[A] = new ToMorphirUType[A] {
    def apply: UType = f
  }

  implicit val unitUType: ToMorphirUType[scala.Unit] = toUTypeConverter(T.unit)
  implicit val boolUType: ToMorphirUType[Boolean]    = toUTypeConverter(sdk.Basics.boolType)
  implicit val intUType: ToMorphirUType[Int]         = toUTypeConverter(sdk.Basics.intType)

  final class SummonPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def withAttributesOf[Attribs](implicit toMorphirType: ToMorphirType[A, Attribs]): ToMorphirType[A, Attribs] =
      toMorphirType
  }
}
