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
    case Data.Int16(value: scala.Short) =>
      V.apply(
        value.morphirType,
        V.reference(value.morphirType, FQName.fromString("Morphir.SDK:Int:toInt16")),
        V.intTyped(value)
      )
    case Data.Int32(value: scala.Int)         => V.int(value.morphirType, value)
    case Data.String(value: java.lang.String) => V.string(value.morphirType, value)
    case Data.LocalDate(value: java.time.LocalDate) =>
      V.apply(
        value.morphirType,
        V.reference(value.morphirType, FQName.fromString("Morphir.SDK:LocalDate:fromParts")),
        V.intTyped(value.getYear),
        V.intTyped(value.getMonthValue),
        V.intTyped(value.getDayOfMonth)
      )
    case Data.LocalTime(value: java.time.LocalTime) =>
      V.apply(
        value.morphirType,
        V.reference(value.morphirType, FQName.fromString("Morphir.SDK:LocalTime:fromMilliseconds")),
        V.intTyped(value.get(ChronoField.MILLI_OF_DAY))
      )
    case Data.Month(value: java.time.Month) => value match {
        case Month.JANUARY   => V.constructor("Morphir.SDK:Month:January", value.morphirType)
        case Month.FEBRUARY  => V.constructor("Morphir.SDK:Month:February", value.morphirType)
        case Month.MARCH     => V.constructor("Morphir.SDK:Month:March", value.morphirType)
        case Month.APRIL     => V.constructor("Morphir.SDK:Month:April", value.morphirType)
        case Month.MAY       => V.constructor("Morphir.SDK:Month:May", value.morphirType)
        case Month.JUNE      => V.constructor("Morphir.SDK:Month:June", value.morphirType)
        case Month.JULY      => V.constructor("Morphir.SDK:Month:July", value.morphirType)
        case Month.AUGUST    => V.constructor("Morphir.SDK:Month:August", value.morphirType)
        case Month.SEPTEMBER => V.constructor("Morphir.SDK:Month:September", value.morphirType)
        case Month.OCTOBER   => V.constructor("Morphir.SDK:Month:October", value.morphirType)
        case Month.NOVEMBER  => V.constructor("Morphir.SDK:Month:November", value.morphirType)
        case Month.DECEMBER  => V.constructor("Morphir.SDK:Month:December", value.morphirType)
      }
    case Data.Optional.None(shape: Concept.Optional) =>
      V.constructor("Morphir.SDK:Maybe:Nothing", shape.morphirType)
    case Data.Optional.Some(data, shape) =>
      V.apply(
        shape.morphirType,
        V.constructor(FQName.fromString("Morphir.SDK:Maybe:just"), shape.morphirType),
        dataToIR(data)
      )
    case Data.List(values, shape) =>
      val valuesList = values.map { data => dataToIR(data) }
      V.list(shape.morphirType, zio.Chunk.fromIterable(valuesList))
    case Data.Map(values, shape) =>
      val tupleShape = Concept.Tuple(List(shape.keyType, shape.valueType))
      val tuples = values.map { case (key, value) =>
        V.tuple(tupleShape.morphirType, dataToIR(key), dataToIR(value))
      }
      V.apply(
        shape.morphirType,
        V.reference(shape.morphirType, FQName.fromString("Morphir.SDK:Dict:fromList")),
        V.list(Concept.List(tupleShape).morphirType, zio.Chunk.fromIterable(tuples))
      )
    case record: Data.Record =>
      val fields = record.values.map { case (Label(name), value) => (name, dataToIR(value)) }
      V.record(record.shape.morphirType, fields: _*)
    case tuple: Data.Tuple =>
      val values = tuple.values.map { data => dataToIR(data) }
      V.tuple(tuple.shape.morphirType, zio.Chunk.fromIterable(values))
    case Data.Aliased(data, shape) =>
      val alias = shape.morphirType
      dataToIR(data) match {
        case Value.Apply(attributes, function, argument) => Value.Apply(alias, function, argument)
        case Value.Constructor(attributes, name)         => Value.Constructor(alias, name)
        case Value.Destructure(attributes, pattern, valueToDestruct, inValue) =>
          Value.Destructure(alias, pattern, valueToDestruct, inValue)
        case Value.Field(attributes, subjectValue, fieldName) => Value.Field(alias, subjectValue, fieldName)
        case Value.FieldFunction(attributes, name)            => Value.FieldFunction(alias, name)
        case Value.IfThenElse(attributes, condition, thenBranch, elseBranch) =>
          Value.IfThenElse(alias, condition, thenBranch, elseBranch)
        case Value.Lambda(attributes, argumentPattern, body) => Value.Lambda(alias, argumentPattern, body)
        case Value.LetDefinition(attributes, valueName, valueDefinition, inValue) =>
          Value.LetDefinition(alias, valueName, valueDefinition, inValue)
        case Value.LetRecursion(attributes, valueDefinitions, inValue) =>
          Value.LetRecursion(alias, valueDefinitions, inValue)
        case Value.List(attributes, elements)                   => Value.List(alias, elements)
        case Value.Literal(attributes, literal)                 => Value.Literal(alias, literal)
        case Value.PatternMatch(attributes, branchOutOn, cases) => Value.PatternMatch(alias, branchOutOn, cases)
        case Value.Record(attributes, fields)                   => Value.Record(alias, fields)
        case Value.Reference(attributes, fullyQualifiedName)    => Value.Reference(alias, fullyQualifiedName)
        case Value.Tuple(attributes, elements)                  => Value.Tuple(alias, elements)
        case Value.Unit(attributes)                             => Value.Unit(alias)
        case Value.UpdateRecord(attributes, valueToUpdate, fieldsToUpdate) =>
          Value.UpdateRecord(alias, valueToUpdate, fieldsToUpdate)
        case Value.Variable(attributes, name) => Value.Variable(alias, name)
      }
    case Data.Case(values, enumLabel, shape) => ??? // TODO: to be implemented
    case Data.Union(value, shape)            => ??? // TODO: to be implemented
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
