package org.finos.morphir.runtime

import org.finos.morphir.ir.Type.Type
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Value.{Pattern, Value}
import org.finos.morphir.ir.{Module, Type}
import org.finos.morphir.naming.*
import Name.toTitleCase
import org.finos.morphir.MInt
import org.finos.morphir.datamodel.Concept.Result
import org.finos.morphir.runtime.TypedMorphirRuntimeDefs.{RuntimeValue, TypeAttribs, ValueAttribs}
import org.finos.morphir.runtime.internal.{NativeFunctionSignature, NativeFunctionSignatureAdv}
import org.finos.morphir.runtime.{IllegalValue, UnexpectedType}
import org.finos.morphir.runtime.internal.CallStackFrame

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

// TODO Rename to RTValue
// Represents a Morphir-Evaluator result. Typed on TypedMorphirRuntimeDefs.TypeAttribs, TypedMorphirRuntimeDefs.ValueAttribs
// instead of a a Generic VA/TA since the latter is not necessary.
sealed trait RTValue {
  def succinct(depth: Int): String = s"${this.getClass} (Default implementation)"
  def succinct: String             = succinct(2)
}

object RTValue {
  sealed trait ValueResult[T] extends RTValue {
    def value: T
  }
  sealed trait Function extends RTValue

  def coerceList(arg: RTValue) =
    arg match {
      case v: RTValue.List => v
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a ListResult value. It is not a List-based result!"
        )
    }

  def coerceSet(arg: RTValue) =
    arg match {
      case v: RTValue.Set => v
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a SetResult value. It is not a Set-based result!"
        )
    }

  def coerceMap(arg: RTValue) =
    arg match {
      case v: RTValue.Map => v
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a MapResult value. It is not a Map-based result!"
        )
    }

  def coerceTuple(arg: RTValue) =
    arg match {
      case v: RTValue.Tuple => v
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a MapResult value. It is not a list result!"
        )
    }

  def coerceBoolean(arg: RTValue) =
    arg match {
      case v: Primitive.Boolean => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Boolean value because it was not a Primitive.Boolean"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Boolean. It is not a primitive!")
    }

  // A Moprhir/ELM float is a Java Double, calling this unwrapDouble since that's the Java datatype being returned
  def coerceDouble(arg: RTValue) =
    arg match {
      case v: Primitive.Float => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Double value because it was not a Primitive.Double"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Double. It is not a primitive!")
    }

  def coerceInt(arg: RTValue) =
    arg match {
      case v: Primitive.Int => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Int value because it was not a Primitive.Int"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Int. It is not a primitive!")
    }

  def coerceFloat(arg: RTValue) =
    arg match {
      case v: Primitive.Float => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Float value because it was not a Primitive.Float"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Float. It is not a primitive!")
    }

  def coerceDecimal(arg: RTValue) =
    arg match {
      case v: Primitive.BigDecimal => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a BigDecimal value because it was not a Primitive.BigDecimal"
        )
      case _ =>
        throw UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a primitive BigDecimal. It is not a primitive!"
        )
    }

  def coerceLong(arg: RTValue) =
    arg match {
      case v: Primitive.Int => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Long value because it was not a Primitive.Long"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Long. It is not a primitive!")
    }

  def coerceString(arg: RTValue) =
    arg match {
      case v: Primitive.String => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a String value because it was not a Primitive.String"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive String. It is not a primitive!")
    }

  def coercePrimitive(arg: RTValue): Primitive[_] =
    arg match {
      case p: Primitive[_] => p.asInstanceOf[Primitive[_]]
      case _               => throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive")
    }

  def coerceNumeric(arg: RTValue): Primitive.Numeric[_] =
    arg match {
      // Need a typecast because can't match on `p: Primitive.Numeric[_]` since there's
      // no actually Primitive.Numeric that has a type of `Any`.
      case p: Primitive.Numeric[_] => p
      case _ => throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive numeric")
    }

  def coerceFunction(arg: RTValue): Function =
    arg match {
      case f: Function => f
      case _           => throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a function")
    }

  case class NumericsWithHelper[T](
      a: T,
      b: T,
      helper: scala.Numeric[T],
      fractionalHelper: Option[scala.Fractional[T]],
      integralHelper: Option[scala.Integral[T]]
  )
  case class NumericWithHelper[T](
      value: T,
      helper: scala.Numeric[T],
      fractionalHelper: Option[scala.Fractional[T]],
      integralHelper: Option[scala.Integral[T]]
  )

  // Without a function that unwraps both types together with the numeric, it is very difficult to get all the
  // types to line up correctly for the Numeric instance to properly recognize what type it is supposed to take
  def unwrapNumericsWithHelper[N](
      arg1: RTValue,
      arg2: RTValue
  ): NumericsWithHelper[N] = {
    val a = coerceNumeric(arg1)
    val b = coerceNumeric(arg2)
    if (a.numericType != b.numericType) {
      throw new UnexpectedType(
        s"Error unwrapping the Primitive Numerics ${arg1} and ${arg2} into a common type, they have different numeric types: ${a.numericType} versus ${b.numericType}"
      )
    }
    NumericsWithHelper[N](
      a.value.asInstanceOf[N],
      b.value.asInstanceOf[N],
      a.numericHelper.asInstanceOf[scala.Numeric[N]],
      a.fractionalHelper.asInstanceOf[Option[scala.Fractional[N]]],
      a.integralHelper.asInstanceOf[Option[scala.Integral[N]]]
    )
  }

  def unwrapNumericWithHelper[N](arg: RTValue): NumericWithHelper[N] = {
    val a = coerceNumeric(arg)
    NumericWithHelper[N](
      a.value.asInstanceOf[N],
      a.numericHelper.asInstanceOf[scala.Numeric[N]],
      a.fractionalHelper.asInstanceOf[Option[scala.Fractional[N]]],
      a.integralHelper.asInstanceOf[Option[scala.Integral[N]]]
    )
  }

  case class Unit() extends RTValue {
    override def succinct(depth: Int) = "Unit"
  }

  sealed trait Primitive[T] extends ValueResult[T] {
    def value: T
    def isNumeric                     = false
    override def succinct(depth: Int) = s"Primitive($value)"
  }

  object Primitive {
    sealed trait Numeric[T] extends Primitive[T] {
      override def isNumeric = false
      def numericType: Numeric.Type[T]
      def value: T
      def numericHelper: scala.Numeric[T]
      def fractionalHelper: Option[scala.Fractional[T]]
      def integralHelper: Option[scala.Integral[T]]
    }
    object Numeric {
      sealed trait Type[T] {
        def makeOrFail(value: Any): Numeric[T]
      }
      object Type {
        case object Int extends Numeric.Type[MInt] {
          override def makeOrFail(value: Any): Numeric[MInt] =
            value match {
              case v: MInt => Primitive.Int(v)
              case _       => throw IllegalValue(s"The value $value is not an MInt")
            }
        }
        case object Float extends Numeric.Type[scala.Double] {
          override def makeOrFail(value: Any): Numeric[scala.Double] =
            value match {
              case v: scala.Double => Primitive.Float(v)
              case _               => throw IllegalValue(s"The value $value is not a Double")
            }
        }
        case object BigDecimal extends Numeric.Type[scala.BigDecimal] {
          override def makeOrFail(value: Any): Numeric[scala.BigDecimal] =
            value match {
              case v: scala.BigDecimal => Primitive.BigDecimal(v)
              case _                   => throw IllegalValue(s"The value $value is not a BigDecimal")
            }
        }
      }
    }

    case class Int(value: MInt) extends Numeric[MInt] {
      val numericType      = Numeric.Type.Int
      def numericHelper    = org.finos.morphir.mIntIsNumeric
      def fractionalHelper = None
      def integralHelper   = Some(org.finos.morphir.mIntIsIntegral)
      def valueAsInt =
        if (value.isValidInt)
          value.toInt
        else
          throw new UnexpectedType(
            s"Cannot unwrap ${value} into an integer because it's value is too large or too small"
          )
    }
    object Int {
      def apply(value: scala.Int)  = new Int(MInt.fromInt(value))
      def apply(value: scala.Long) = new Int(MInt.fromLong(value))
    }

    // A Morphir/ELM Float is the same as a Java Double
    case class Float(value: scala.Double) extends Numeric[scala.Double] {
      val numericType           = Numeric.Type.Float
      lazy val numericHelper    = implicitly[scala.Numeric[scala.Double]]
      lazy val fractionalHelper = Some(implicitly[scala.Fractional[scala.Double]])
      lazy val integralHelper   = None
    }
    case class BigDecimal(value: scala.BigDecimal) extends Numeric[scala.BigDecimal] {
      val numericType           = Numeric.Type.BigDecimal
      lazy val numericHelper    = implicitly[scala.Numeric[scala.BigDecimal]]
      lazy val fractionalHelper = Some(implicitly[scala.Fractional[scala.BigDecimal]])
      lazy val integralHelper   = None
    }

    object DecimalBounded {
      def unapply(resultValue: RTValue.Primitive.Numeric[_]): Option[scala.BigDecimal] =
        resultValue match {
          case Primitive.Float(v)      => Some(scala.BigDecimal(v.toDouble))
          case Primitive.Int(v)        => Some(v.toBigDecimal)
          case Primitive.BigDecimal(v) => Some(v)
        }
    }

    case class Boolean(value: scala.Boolean)   extends Primitive[scala.Boolean]
    case class String(value: java.lang.String) extends Primitive[java.lang.String]
    case class Char(value: scala.Char)         extends Primitive[scala.Char]

    def unapply(prim: Primitive[_]): Option[Any] =
      Some(prim.value)

    def makeOrFail[T](value: T): Primitive[T] =
      make[T](value) match {
        case Some(value) => value
        case None => throw new UnexpectedType(
            s"Cannot unwrap value `$value` into a primitive. It is a ${value.getClass}. Valid Primitive values are: Int, Long, String, Boolean, Char, Double, BigDecimal, Float"
          )
      }

    // TODO: This kinds of logic is highly problematic in many places (e.g. in a mod function if we were to use it)
    //       because it creates a value based on the input-type, not based on the expected output type. For example,
    //       say for instance that somehow the output of an scala-based implementaton of mod(a, b) was a string but elm
    //       expects an MInt. This would return it as string instead. Therefore we need to move away from using this logic
    //       and return expected types as opposed to input types.
    def make[T](value: T): Option[Primitive[T]] =
      value match {
        case v: MInt             => Some(Primitive.Int(v).asInstanceOf[Primitive[T]])
        case v: scala.Int        => Some(Primitive.Int(MInt.fromInt(v)).asInstanceOf[Primitive[T]])
        case v: scala.Long       => Some(Primitive.Int(MInt.fromLong(v)).asInstanceOf[Primitive[T]])
        case v: java.lang.String => Some(Primitive.String(v).asInstanceOf[Primitive[T]])
        case v: scala.Boolean    => Some(Primitive.Boolean(v).asInstanceOf[Primitive[T]])
        case v: scala.Char       => Some(Primitive.Char(v).asInstanceOf[Primitive[T]])
        case v: scala.BigDecimal => Some(Primitive.BigDecimal(v).asInstanceOf[Primitive[T]])
        case v: scala.Float      => Some(Primitive.Float(v.toDouble).asInstanceOf[Primitive[T]])
        case v: scala.Double     => Some(Primitive.Float(v).asInstanceOf[Primitive[T]])
        case _                   => None
      }
  }

  case class LocalDate(value: java.time.LocalDate) extends ValueResult[java.time.LocalDate] {
    override def succinct(depth: Int) = s"LocalDate($value)"
  }

  case class LocalTime(value: java.time.LocalTime) extends ValueResult[java.time.LocalTime] {
    override def succinct(depth: Int) = s"LocalTime($value)"
  }

  case class Tuple(elements: scala.List[RTValue]) extends ValueResult[scala.List[RTValue]] {
    def value = elements
    override def succinct(depth: Int) = if (depth == 0) "Tuple(...)"
    else {
      s"Tuple(${elements.map(_.succinct(depth - 1)).mkString(", ")})"
    }
  }
  object Tuple {
    def apply(elements: RTValue*) = new RTValue.Tuple(elements.toList)
  }

  case class Set(elements: mutable.LinkedHashSet[RTValue]) extends ValueResult[mutable.LinkedHashSet[RTValue]] {
    def value = elements
    override def succinct(depth: Int) = if (depth == 0) "Set(..)"
    else {
      s"Set(${elements.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class Record(elements: collection.Map[Name, RTValue]) extends ValueResult[collection.Map[Name, RTValue]] {
    def value = elements
    override def succinct(depth: Int) = if (depth == 0) "Record(..)"
    else {
      s"Record(${elements.map { case (key, value) => s"$key -> ${value.succinct(depth - 1)}" }.mkString(", ")})"
    }
  }

  case class List(elements: scala.List[RTValue]) extends ValueResult[scala.List[RTValue]] {
    def value = elements
    override def succinct(depth: Int) = if (depth == 0) "List(..)"
    else {
      s"List(${elements.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class Map(elements: mutable.LinkedHashMap[RTValue, RTValue])
      extends ValueResult[mutable.LinkedHashMap[RTValue, RTValue]] {
    def value = elements
    override def succinct(depth: Int) = if (depth == 0) "Dict(..)"
    else {
      s"Dict(${elements.map { case (key, value) => s"${key.succinct(depth - 1)} -> ${value.succinct(depth - 1)}" }.mkString(", ")})"
    }
  }
  case class Applied(
      body: RuntimeValue,
      curried: scala.List[(Name, RTValue)],
      closingContext: CallStackFrame
  )

  case class FieldFunction(fieldName: Name) extends Function

  case class LambdaFunction(body: RuntimeValue, pattern: Pattern[ValueAttribs], closingContext: CallStackFrame)
      extends Function

  case class DefinitionFunction(
      body: RuntimeValue,
      arguments: scala.List[(Name, ValueAttribs, Type[TypeAttribs])],
      curried: scala.List[(Name, RTValue)],
      closingContext: CallStackFrame
  ) extends RTValue

  case class ConstructorFunction(name: FQName, arguments: scala.List[ValueAttribs], curried: scala.List[RTValue])
      extends Function

  case class ConstructorResult(name: FQName, values: scala.List[RTValue]) extends RTValue {
    override def succinct(depth: Int) = if (depth == 0) s"${name.toString}(..)"
    else {
      s"${name.toString}(${values.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  sealed trait NativeFunctionResult extends Function

  case class NativeFunction(
      arguments: Int,
      curried: scala.List[RTValue],
      function: NativeFunctionSignature
  ) extends NativeFunctionResult {}

  case class NativeInnerFunction(
      arguments: Int,
      curried: scala.List[RTValue],
      function: NativeFunctionSignatureAdv
  ) extends NativeFunctionResult {}
}
