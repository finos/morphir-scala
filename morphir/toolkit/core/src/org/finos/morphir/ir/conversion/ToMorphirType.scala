package org.finos.morphir
package ir
package conversion

import org.finos.morphir.naming._
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

  implicit val unitUType: ToMorphirUType[scala.Unit]               = toUTypeConverter(T.unit)
  implicit val boolUType: ToMorphirUType[Boolean]                  = toUTypeConverter(sdk.Basics.boolType)
  implicit val intUType: ToMorphirUType[Int]                       = toUTypeConverter(sdk.Int.int32Type)
  implicit val stringUType: ToMorphirUType[String]                 = toUTypeConverter(sdk.String.stringType)
  implicit val byteUType: ToMorphirUType[Byte]                     = toUTypeConverter(sdk.Int.int8Type)
  implicit val shortUType: ToMorphirUType[Short]                   = toUTypeConverter(sdk.Int.int16Type)
  implicit val decimalUType: ToMorphirUType[scala.BigDecimal]      = toUTypeConverter(sdk.Decimal.decimalType)
  implicit val localDateUType: ToMorphirUType[java.time.LocalDate] = toUTypeConverter(sdk.LocalDate.localDateType)
  implicit val localTimeUType: ToMorphirUType[java.time.LocalTime] = toUTypeConverter(sdk.LocalTime.localTimeType)
  implicit val monthUType: ToMorphirUType[java.time.Month]         = toUTypeConverter(sdk.Month.dateType)
  implicit val charUType: ToMorphirUType[scala.Char]               = toUTypeConverter(sdk.Char.charType)
  implicit val bigIntUType: ToMorphirUType[scala.BigInt]           = toUTypeConverter(sdk.Basics.intType)

  implicit def optionUType[A](implicit elementToUType: ToMorphirUType[A]): ToMorphirUType[scala.Option[A]] =
    toUTypeConverter(sdk.Maybe.maybeType(elementToUType.morphirType))

  implicit def listUType[A](implicit elementToUType: ToMorphirUType[A]): ToMorphirUType[scala.List[A]] =
    toUTypeConverter(sdk.List.listType(elementToUType.morphirType))

  implicit def mapUType[A, B](implicit
      keyToUType: ToMorphirUType[A],
      valueToUType: ToMorphirUType[B]
  ): ToMorphirUType[Map[A, B]] =
    toUTypeConverter(sdk.Dict.dictType(keyToUType.morphirType, valueToUType.morphirType))

  implicit def setUType[A, B](implicit
      itemToUType: ToMorphirUType[A]
  ): ToMorphirUType[Set[A]] =
    toUTypeConverter(sdk.Set.setType(itemToUType.morphirType))

  implicit val labelUType: ToMorphirUType[Label] = stringUType.as

  implicit def conceptToTypeIR(concept: Concept): ToMorphirUType[Concept] =
    concept match {
      case Concept.Boolean                 => boolUType.as
      case Concept.Byte                    => byteUType.as
      case Concept.Decimal                 => decimalUType.as
      case Concept.Integer                 => bigIntUType.as
      case Concept.Int16                   => shortUType.as
      case Concept.Int32                   => intUType.as
      case Concept.String                  => stringUType.as
      case Concept.LocalDate               => localDateUType.as
      case Concept.Month                   => monthUType.as
      case Concept.LocalTime               => localTimeUType.as
      case Concept.Char                    => charUType.as
      case Concept.Unit                    => unitUType.as
      case Concept.Alias(name, value)      => toUTypeConverter(T.reference(name))
      case Concept.Enum(name, cases)       => toUTypeConverter(T.reference(name))
      case Concept.List(elementType)       => listUType(conceptToTypeIR(elementType)).as
      case Concept.Optional(elementType)   => optionUType(conceptToTypeIR(elementType)).as
      case Concept.Map(keyType, valueType) => mapUType(conceptToTypeIR(keyType), conceptToTypeIR(valueType)).as
      case Concept.Struct(fields) =>
        val types: scala.List[(String, UType)] = fields.map {
          case (k: Label, v: Concept) => (k.value, conceptToTypeIR(v).morphirType)
        }
        // TODO: When we provide MorphirType map this to a spec or definition
        toUTypeConverter(T.record(types: _*))
      case Concept.Record(_, fields) =>
        val types: scala.List[(String, UType)] = fields.map {
          case (k: Label, v: Concept) => (k.value, conceptToTypeIR(v).morphirType)
        }
        // TODO: When we provide MorphirType map this to a spec or definition
        toUTypeConverter(T.record(types: _*))

      case Concept.Tuple(values) =>
        val types: scala.List[UType] = values.map(conceptToTypeIR(_).morphirType)
        toUTypeConverter(T.tupleVar(types: _*))

      case Concept.Any          => toUTypeConverter(sdk.Basics.neverType) // TODO: map this to the correct type
      case Concept.Nothing      => toUTypeConverter(sdk.Basics.neverType) // TODO: map this to the correct type
      case Concept.Union(cases) => toUTypeConverter(sdk.Basics.neverType) // TODO: map this to the correct type
    }

  final class SummonPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def withAttributesOf[Attribs](implicit toMorphirType: ToMorphirType[A, Attribs]): ToMorphirType[A, Attribs] =
      toMorphirType
  }
}
