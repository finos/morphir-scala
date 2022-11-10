package org.finos.morphir
package ir

import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.Type.Type
import org.finos.morphir.ir.{Value => V}
import Value.{Value, RawValue, TypedValue}

import zio.Tag


trait ToMorphirValue[A, +TypeAttribs, +ValueAttribs] {
  def apply(value: A)(implicit tag: Tag[A]): Value[TypeAttribs, ValueAttribs]
}

trait ToMorphirRawValue[A] extends ToMorphirValue[A, scala.Unit, scala.Unit] {
  def apply(value: A)(implicit tag: Tag[A]): RawValue
}

trait ToMorphirTypedValue[A] extends ToMorphirValue[A, scala.Unit, Type[scala.Unit]] {
  def apply(value: A)(implicit tag: Tag[A]): TypedValue
}

object ToMorphirValue {}

object ToMorphirTypedValue extends ToMorphirTypedValueInstances {
  def apply[A](implicit toMorphirTypedValue: ToMorphirTypedValue[A]): ToMorphirTypedValue[A] = toMorphirTypedValue

}

trait ToMorphirTypedValueInstances {
  implicit val unit: ToMorphirTypedValue[scala.Unit] = new ToMorphirTypedValue[scala.Unit] {
    def apply(value: scala.Unit)(implicit tag: Tag[scala.Unit]): TypedValue = V.unit(T.unit)
  }

  implicit val boolean: ToMorphirTypedValue[Boolean] = new ToMorphirTypedValue[Boolean] {
    def apply(value: Boolean)(implicit tag: Tag[Boolean]): TypedValue =
      if (value) Literal.Lit.True else Literal.Lit.False
  }

  implicit val listOfType: ToMorphirTypedValue[scala.List[Boolean]] =
    new ToMorphirTypedValue[scala.List[Boolean]] {
      def apply(value: scala.List[Boolean])(implicit tag: Tag[scala.List[Boolean]]): TypedValue = {
        val elementsAsMorphirValues = value.map(element => ToMorphirTypedValue[Boolean].apply(element))
        V.list(sdk.List.listType(sdk.Basics.boolType), zio.Chunk.fromIterable(elementsAsMorphirValues))
      }
    }
}

// Given a value in Scala to represent that value in Morphir
