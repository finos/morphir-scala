package org.finos.morphir
package ir
package conversion

import org.finos.morphir.datamodel.{Concept, Data, Label}
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.{TypedValue, Value}
import org.finos.morphir.ir.{Type => T, Value => V}

import java.time.Month
import java.time.temporal.ChronoField

trait ToMorphirValue[A, +TypeAttribs, +ValueAttribs] {
  def apply(value: A): Value[TypeAttribs, ValueAttribs]
  final def toMorphirValue(value: A): Value[TypeAttribs, ValueAttribs] = apply(value)
}

object ToMorphirValue extends ToMorphirValueFunctions with ToMorphirTypedValueInstances {

  final class SummonPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def withAttributesOf[TypeAttribs, ValueAttribs](implicit
        instance: ToMorphirValue[A, TypeAttribs, ValueAttribs]
    ): ToMorphirValue[A, TypeAttribs, ValueAttribs] = instance

    def raw(implicit instance: ToMorphirRawValue[A]): ToMorphirRawValue[A]       = instance
    def typed(implicit instance: ToMorphirTypedValue[A]): ToMorphirTypedValue[A] = instance
  }
}

trait ToMorphirValueFunctions {
  import ToMorphirValue.SummonPartiallyApplied

  def apply[A, TA, VA](implicit toMorphirValue: ToMorphirValue[A, TA, VA]): ToMorphirValue[A, TA, VA] = toMorphirValue
  def summon[A]: SummonPartiallyApplied[A] = new SummonPartiallyApplied[A]

  def makeTyped[A](f: A => TypedValue): ToMorphirTypedValue[A] = new ToMorphirTypedValue[A] {
    def apply(value: A): TypedValue = f(value)
  }
}

trait ToMorphirTypedValueInstances extends ToMorphirTypedValueInstancesLowPriority { self: ToMorphirValueFunctions =>
  import ToMorphirUType._

  implicit def listOfType[A](implicit
      elementToUType: ToMorphirUType[A],
      elementToTypedValue: ToMorphirTypedValue[A]
  ): ToMorphirTypedValue[scala.List[A]] = makeTyped { value =>
    val elementType: T.UType    = elementToUType.morphirType
    val elementsAsMorphirValues = value.map(element => elementToTypedValue(element))
    V.list(sdk.List.listType(elementType), zio.Chunk.fromIterable(elementsAsMorphirValues))
  }
}

trait ToMorphirTypedValueInstancesLowPriority { self: ToMorphirValueFunctions =>
  import ToMorphirUType._

  implicit def dataToIR[Data]: ToMorphirTypedValue[Data] = {
    case Data.Unit           => V.unit(().morphirType)
    case Data.Boolean(value) => if (value) Literal.Lit.True else Literal.Lit.False
    case Data.Byte(value: scala.Byte) =>
      V.apply(
        value.morphirType,
        V.reference(value.morphirType, FQName.fromString("Morphir.SDK:Int:toInt8")),
        V.intTyped(value)
      )
    case Data.Char(value: scala.Char)          => V.literal(value.morphirType, Lit.char(value))
    case Data.Decimal(value: scala.BigDecimal) => V.decimal(value.morphirType, value)
    case Data.Integer(value: scala.BigInt) =>
      V.intTyped(value.toInt) // TODO: to be fixed when Integer is mapped to BigInt
  }

  implicit val unitTyped: ToMorphirTypedValue[scala.Unit] = makeTyped { v =>
    V.unit(v.morphirType)
  }

  implicit val booleanTyped: ToMorphirTypedValue[Boolean] = makeTyped { value =>
    if (value) Literal.Lit.True else Literal.Lit.False
  }

  implicit val stringTyped: ToMorphirTypedValue[String] = makeTyped { value =>
    V.string(value.morphirType, value)
  }

  implicit val intTyped: ToMorphirTypedValue[Int] = makeTyped { value =>
    V.int(value.morphirType, value)
  }
}
