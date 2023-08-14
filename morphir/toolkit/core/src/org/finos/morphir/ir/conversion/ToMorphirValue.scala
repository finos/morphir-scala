package org.finos.morphir
package ir
package conversion

import org.finos.morphir.naming._
import org.finos.morphir.datamodel.{Concept, Data, EnumLabel, Label}
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.{TypedValue, Value}
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Type.UType

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

  implicit lazy val dataToIR: ToMorphirTypedValue[Data] = {
    case Data.Unit           => V.unit(().morphirType)
    case Data.Boolean(value) => if (value) Literal.Lit.True else Literal.Lit.False
    case Data.Byte(value: scala.Byte) =>
      V.applyInferType(
        value.morphirType,
        V.reference(FQName.fromString("Morphir.SDK:Int:toInt8")),
        V.intTyped(value.toInt)
      )
    case Data.Char(value: scala.Char)          => V.literal(value.morphirType, Lit.char(value))
    case Data.Decimal(value: scala.BigDecimal) => V.decimal(value.morphirType, value)
    case Data.Integer(value: scala.BigInt) =>
      V.intTyped(value.toInt) // TODO: to be fixed when Integer is mapped to BigInt
    case Data.Int16(value: scala.Short) =>
      V.applyInferType(
        value.morphirType,
        V.reference(FQName.fromString("Morphir.SDK:Int:toInt16")),
        V.intTyped(value.toInt)
      )
    case Data.Int32(value: scala.Int)         => V.int(value.morphirType, value)
    case Data.String(value: java.lang.String) => V.string(value.morphirType, value)
    case Data.LocalDate(value: java.time.LocalDate) =>
      V.applyInferType(
        value.morphirType,
        V.reference(FQName.fromString("Morphir.SDK:LocalDate:fromParts")),
        V.intTyped(value.getYear),
        V.intTyped(value.getMonthValue),
        V.intTyped(value.getDayOfMonth)
      )
    case Data.LocalTime(value: java.time.LocalTime) =>
      V.applyInferType(
        value.morphirType,
        V.reference(FQName.fromString("Morphir.SDK:LocalTime:fromMilliseconds")),
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
      V.applyInferType(
        shape.morphirType,
        V.constructor(FQName.fromString("Morphir.SDK:Maybe:just")),
        dataToIR(data)
      )
    case Data.Result.Err(data, shape) =>
      V.applyInferType(
        shape.morphirType,
        V.constructor(FQName.fromString("Morphir.SDK:Result:err")),
        dataToIR(data)
      )
    case Data.Result.Ok(data, shape) =>
      V.applyInferType(
        shape.morphirType,
        V.constructor(FQName.fromString("Morphir.SDK:Result:ok")),
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
      V.applyInferType(
        shape.morphirType,
        V.reference(FQName.fromString("Morphir.SDK:Dict:fromList")),
        V.list(Concept.List(tupleShape).morphirType, zio.Chunk.fromIterable(tuples))
      )

    case record: Data.Record =>
      val fields = record.values.map { case (Label(name), value) => (name, dataToIR(value)) }
      Value.Record(record.shape.morphirType, fields: _*)

    case struct: Data.Struct =>
      val fields = struct.values.map { case (Label(name), value) => (name, dataToIR(value)) }
      Value.Record(struct.shape.morphirType, fields: _*)

    case tuple: Data.Tuple =>
      val values = tuple.values.map { data => dataToIR(data) }
      V.tuple(tuple.shape.morphirType, zio.Chunk.fromIterable(values))
    case Data.Aliased(data, shape) =>
      val alias = shape.morphirType
      dataToIR(data) match {
        case Value.Apply(_, function, argument) => Value.Apply(alias, function, argument)
        case Value.Constructor(_, name)         => Value.Constructor(alias, name)
        case Value.Destructure(_, pattern, valueToDestruct, inValue) =>
          Value.Destructure(alias, pattern, valueToDestruct, inValue)
        case Value.Field(_, subjectValue, fieldName) => Value.Field(alias, subjectValue, fieldName)
        case Value.FieldFunction(_, name)            => Value.FieldFunction(alias, name)
        case Value.IfThenElse(_, condition, thenBranch, elseBranch) =>
          Value.IfThenElse(alias, condition, thenBranch, elseBranch)
        case Value.Lambda(_, argumentPattern, body) => Value.Lambda(alias, argumentPattern, body)
        case Value.LetDefinition(_, valueName, valueDefinition, inValue) =>
          Value.LetDefinition(alias, valueName, valueDefinition, inValue)
        case Value.LetRecursion(_, valueDefinitions, inValue) =>
          Value.LetRecursion(alias, valueDefinitions, inValue)
        case Value.List(_, elements)                   => Value.List(alias, elements)
        case Value.Literal(_, literal)                 => Value.Literal(alias, literal)
        case Value.PatternMatch(_, branchOutOn, cases) => Value.PatternMatch(alias, branchOutOn, cases)
        case Value.Record(_, fields)                   => Value.Record(alias, fields)
        case Value.Reference(_, fullyQualifiedName)    => Value.Reference(alias, fullyQualifiedName)
        case Value.Tuple(_, elements)                  => Value.Tuple(alias, elements)
        case Value.Unit(_)                             => Value.Unit(alias)
        case Value.UpdateRecord(_, valueToUpdate, fieldsToUpdate) =>
          Value.UpdateRecord(alias, valueToUpdate, fieldsToUpdate)
        case Value.Variable(_, name) => Value.Variable(alias, name)
      }
    case Data.Case(values, enumLabel, shape) =>
      // Okay I have an outermost thing type - the union's shape - and an innermost value - the constructor itself
      // So from the outside I walk down, building up a type by continually wrapping the outer type...?
      // And I build a value by wrapping types?
//      def curryFunctionValue(fqn: FQName, outerTpe: UType, fields: List[TypedValue]): TypedValue =
//        fields match {
//          case Nil => V.constructor(fqn, outerTpe)
//          case head :: tail =>
//            V.apply(outerTpe, curryFunctionValue(fqn, Type.function((), head.attributes, outerTpe), tail), head)
//        }
//      val args = values.map { case (_, data) => dataToIR(data) }.reverse
//      val name = shape.name.copy(localName = Name.fromString(enumLabel))
//      curryFunctionValue(name, shape.morphirType, args)
      V.applyInferType(
        shape.morphirType,
        V.constructor(shape.name.copy(localName = Name.fromString(enumLabel))),
        values.map { case (_, data) => dataToIR(data) }: _*
      )

    case Data.Union(_, _) => ??? // TODO: to be implemented
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
