package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.ir.{Module, Type}
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Type.Type
import org.finos.morphir.ir.Value.{Pattern, Value}

import scala.collection.mutable.LinkedHashMap
import Name.toTitleCase
import org.finos.morphir.runtime.TypedMorphirRuntime.{RuntimeValue, TypeAttribs, ValueAttribs}
import org.finos.morphir.runtime.UnexpectedType

import scala.collection.mutable

// Represents a Morphir-Evaluator result. Typed on TypedMorphirRuntime.TypeAttribs, TypedMorphirRuntime.ValueAttribs
// instead of a a Generic VA/TA since the latter is not necessary.
sealed trait Result {
  def succinct(depth: Int): String = s"${this.getClass} (Default implementation)"
  def succinct: String             = succinct(2)

  def unwrapString    = Result.unwrapString(this)
  def unwrapInt       = Result.unwrapInt(this)
  def unwrapBoolean   = Result.unwrapBoolean(this)
  def unwrapDouble    = Result.unwrapDouble(this)
  def unwrapLong      = Result.unwrapLong(this)
  def unwrapFloat     = Result.unwrapFloat(this)
  def unwrapDecimal   = Result.unwrapDecimal(this)
  def unwrapPrimitive = Result.unwrapPrimitive(this)
  def unwrapNumeric   = Result.unwrapNumeric(this)
  def unwrapList      = Result.unwrapList(this)
  def unwrapTuple     = Result.unwrapTuple(this)
  def unwrapMap       = Result.unwrapMap(this)
}

object Result {

  def unwrapList(arg: Result): List[Result] =
    arg match {
      case Result.ListResult(list) => list
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a ListResult value. It is not a list result!"
        )
    }

  def unwrapMap(arg: Result): LinkedHashMap[Result, Result] =
    arg match {
      case Result.MapResult(map) => map
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a MapResult value. It is not a list result!"
        )
    }

  def unwrapTuple(arg: Result): List[Result] =
    arg match {
      case Result.Tuple(tup) => tup
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a MapResult value. It is not a list result!"
        )
    }

  def unwrapBoolean(arg: Result): Boolean =
    arg match {
      case Primitive.Boolean(v) => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Boolean value because it was not a Primitive.Boolean"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Boolean. It is not a primitive!")
    }

  // A Moprhir/ELM float is a Java Double, calling this unwrapDouble since that's the Java datatype being returned
  def unwrapDouble(arg: Result): Double =
    arg match {
      case Primitive.Float(v) => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Double value because it was not a Primitive.Double"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Double. It is not a primitive!")
    }

  def unwrapInt(arg: Result): Int =
    arg match {
      case Primitive.Int(v) => v
      case Primitive.Long(v) =>
        if (v > Integer.MAX_VALUE)
          throw new UnexpectedType(s"Cannot unwrap ${arg} into an integer because it's value is too large")
        else
          v.toInt
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Int value because it was not a Primitive.Int"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Int. It is not a primitive!")
    }

  def unwrapFloat(arg: Result): Double =
    arg match {
      case Primitive.Float(v) => v.toDouble
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Float value because it was not a Primitive.Float"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Float. It is not a primitive!")
    }

  def unwrapDecimal(arg: Result): BigDecimal =
    arg match {
      case Primitive.BigDecimal(v) => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a BigDecimal value because it was not a Primitive.BigDecimal"
        )
      case _ =>
        throw UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a primitive BigDecimal. It is not a primitive!"
        )
    }

  def unwrapLong(arg: Result): Long =
    arg match {
      case Primitive.Int(v)  => v.toLong
      case Primitive.Long(v) => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Long value because it was not a Primitive.Long"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Long. It is not a primitive!")
    }

  def unwrapString(arg: Result): String =
    arg match {
      case Primitive.String(v) => v
      case _: Primitive[_] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a String value because it was not a Primitive.String"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive String. It is not a primitive!")
    }

  def unwrapPrimitive(arg: Result): Primitive[_] =
    arg match {
      case p: Primitive[_] => p.asInstanceOf[Primitive[_]]
      case _               => throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive")
    }

  def unwrapNumeric(arg: Result): Primitive.Numeric[_] =
    arg match {
      case p: Primitive.Numeric[_] => p.asInstanceOf[Primitive.Numeric[_]]
      case _ => throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive numeric")
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
      arg1: Result,
      arg2: Result
  ): NumericsWithHelper[N] = {
    val a = unwrapNumeric(arg1)
    val b = unwrapNumeric(arg2)
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

  def unwrapNumericWithHelper[N](arg: Result): NumericWithHelper[N] = {
    val a = unwrapNumeric(arg)
    NumericWithHelper[N](
      a.value.asInstanceOf[N],
      a.numericHelper.asInstanceOf[scala.Numeric[N]],
      a.fractionalHelper.asInstanceOf[Option[scala.Fractional[N]]],
      a.integralHelper.asInstanceOf[Option[scala.Integral[N]]]
    )
  }

  case class Unit() extends Result {
    override def succinct(depth: Int) = "Unit"
  }

  sealed trait Primitive[T] extends Result {
    def value: T
    def isNumeric                     = false
    override def succinct(depth: Int) = s"Primitive($value)"
  }

  object Primitive {
    sealed trait Numeric[T] extends Primitive[T] {
      override def isNumeric = false
      def numericType: Numeric.Type
      def value: T
      def numericHelper: scala.Numeric[T]
      def fractionalHelper: Option[scala.Fractional[T]]
      def integralHelper: Option[scala.Integral[T]]
    }
    object Numeric {
      sealed trait Type
      object Type {
        case object Int        extends Numeric.Type
        case object Long       extends Numeric.Type
        case object Float      extends Numeric.Type
        case object BigDecimal extends Numeric.Type
      }
    }

    case class Int(value: scala.Int) extends Numeric[scala.Int] {
      val numericType           = Numeric.Type.Int
      lazy val numericHelper    = implicitly[scala.Numeric[scala.Int]]
      lazy val fractionalHelper = None
      lazy val integralHelper   = Some(implicitly[scala.Integral[scala.Int]])
    }
    case class Long(value: scala.Long) extends Numeric[scala.Long] {
      val numericType           = Numeric.Type.Long
      lazy val numericHelper    = implicitly[scala.Numeric[scala.Long]]
      lazy val fractionalHelper = None
      lazy val integralHelper   = Some(implicitly[scala.Integral[scala.Long]])
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
      def unapply(resultValue: Result.Primitive.Numeric[_]): Option[scala.BigDecimal] =
        resultValue match {
          case Primitive.Float(v)      => Some(scala.BigDecimal(v.toDouble))
          case Primitive.Int(v)        => Some(scala.BigDecimal(v))
          case Primitive.Long(v)       => Some(scala.BigDecimal(v))
          case Primitive.BigDecimal(v) => Some(v)
        }
    }

    object LongBounded {
      def unapply(resultValue: Result): Option[scala.Long] =
        resultValue match {
          case Primitive.Long(v) => Some(v)
          case Primitive.Int(v)  => Some(v.toLong)
          case _                 => None
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

    def make[T](value: T): Option[Primitive[T]] =
      value match {
        case v: scala.Int        => Some(Primitive.Int(v).asInstanceOf[Primitive[T]])
        case v: scala.Long       => Some(Primitive.Long(v).asInstanceOf[Primitive[T]])
        case v: java.lang.String => Some(Primitive.String(v).asInstanceOf[Primitive[T]])
        case v: scala.Boolean    => Some(Primitive.Boolean(v).asInstanceOf[Primitive[T]])
        case v: scala.Char       => Some(Primitive.Char(v).asInstanceOf[Primitive[T]])
        case v: scala.BigDecimal => Some(Primitive.BigDecimal(v).asInstanceOf[Primitive[T]])
        case v: scala.Float      => Some(Primitive.Float(v.toDouble).asInstanceOf[Primitive[T]])
        case v: scala.Double     => Some(Primitive.Float(v).asInstanceOf[Primitive[T]])
        case _                   => None
      }
  }

  case class LocalDate(value: java.time.LocalDate) extends Result {
    override def succinct(depth: Int) = s"LocalDate($value)"
  }

  case class LocalTime(value: java.time.LocalTime) extends Result {
    override def succinct(depth: Int) = s"LocalTime($value)"
  }

  case class Tuple(elements: List[Result]) extends Result {
    override def succinct(depth: Int) = if (depth == 0) "Tuple(...)"
    else {
      s"Tuple(${elements.map(_.succinct(depth - 1)).mkString(", ")})"
    }
  }
  object Tuple {
    def apply(elements: Result*) = new Result.Tuple(elements.toList)
  }

  case class SetResult(elements: mutable.LinkedHashSet[Result]) extends Result {
    override def succinct(depth: Int) = if (depth == 0) "Set(..)"
    else {
      s"Set(${elements.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class Record(elements: Map[Name, Result]) extends Result {
    override def succinct(depth: Int) = if (depth == 0) "Record(..)"
    else {
      s"Record(${elements.map { case (key, value) => s"$key -> ${value.succinct(depth - 1)}" }.mkString(", ")})"
    }
  }

  case class ListResult(elements: List[Result]) extends Result {
    override def succinct(depth: Int) = if (depth == 0) "List(..)"
    else {
      s"List(${elements.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class MapResult(elements: mutable.LinkedHashMap[Result, Result]) extends Result {
    override def succinct(depth: Int) = if (depth == 0) "Dict(..)"
    else {
      s"Dict(${elements.map { case (key, value) => s"${key.succinct(depth - 1)} -> ${value.succinct(depth - 1)}" }.mkString(", ")})"
    }
  }
  case class Applied(
      body: RuntimeValue,
      curried: List[(Name, Result)],
      closingContext: CallStackFrame
  )

  case class FieldFunction(fieldName: Name) extends Result

  case class LambdaFunction(body: RuntimeValue, pattern: Pattern[ValueAttribs], closingContext: CallStackFrame)
      extends Result

  case class DefinitionFunction(
      body: RuntimeValue,
      arguments: List[(Name, ValueAttribs, Type[TypeAttribs])],
      curried: List[(Name, Result)],
      closingContext: CallStackFrame
  ) extends Result

  case class ConstructorFunction(name: FQName, arguments: List[ValueAttribs], curried: List[Result])
      extends Result

  case class ConstructorResult(name: FQName, values: List[Result]) extends Result {
    override def succinct(depth: Int) = if (depth == 0) s"${name.toString}(..)"
    else {
      s"${name.toString}(${values.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  sealed trait NativeFunctionResult extends Result

  case class NativeFunction(
      arguments: Int,
      curried: List[Result],
      function: NativeFunctionSignature
  ) extends NativeFunctionResult {}

  case class NativeInnerFunction(
      arguments: Int,
      curried: List[Result],
      function: NativeFunctionSignatureAdv
  ) extends NativeFunctionResult {}
}
