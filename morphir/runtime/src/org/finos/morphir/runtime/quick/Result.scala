package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.ir.{Module, Type}
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Value.{Pattern, Value}

import scala.collection.mutable.LinkedHashMap
import Name.toTitleCase
import org.finos.morphir.runtime.UnexpectedType

import scala.collection.mutable

sealed trait Result[TA, VA] {
  def succinct(depth: Int): String = s"${this.getClass} (Default implementation)"
  def succinct: String             = succinct(2)

  def unwrapString    = Result.unwrapString(this)
  def unwrapInt       = Result.unwrapInt(this)
  def unwrapBoolean   = Result.unwrapBoolean(this)
  def unwrapDouble    = Result.unwrapDouble(this)
  def unwrapLong      = Result.unwrapLong(this)
  def unwrapPrimitive = Result.unwrapPrimitive(this)
  def unwrapNumeric   = Result.unwrapNumeric(this)
  def unwrapList      = Result.unwrapList(this)
  def unwrapTuple     = Result.unwrapTuple(this)
  def unwrapMap       = Result.unwrapMap(this)
}

object Result {

  def unwrap[TA, VA](arg: Result[TA, VA]): Any =
    arg match {
      case Unit()               => ()
      case Primitive(value)     => value
      case ListResult(elements) => elements.map(unwrap(_))
      case SetResult(elements)  => elements.map(unwrap(_))
      case Tuple(elements) =>
        val mapped = elements.toList.map(unwrap(_))
        Helpers.listToTuple(mapped)
      case Record(elements)                => elements.map { case (name, value) => name.toCamelCase -> unwrap(value) }
      case MapResult(elements)             => elements.map { case (key, value) => unwrap(key) -> unwrap(value) }
      case ConstructorResult(name, values) => (toTitleCase(name.localName), values.map(unwrap(_)))
      case other =>
        throw new UnexpectedType(
          s"$other returned to top level, only Unit, Primitive, List, Maps, Sets, Tuples, Constructed Types and Records are supported"
        )
    }

  def unwrapList[TA, VA](arg: Result[TA, VA]): List[Result[TA, VA]] =
    arg match {
      case Result.ListResult(list) => list
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a ListResult value. It is not a list result!"
        )
    }

  def unwrapMap[TA, VA](arg: Result[TA, VA]): LinkedHashMap[Result[TA, VA], Result[TA, VA]] =
    arg match {
      case Result.MapResult(map) => map
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a MapResult value. It is not a list result!"
        )
    }

  def unwrapTuple[TA, VA](arg: Result[TA, VA]): TupleSigniture[TA, VA] =
    arg match {
      case Result.Tuple(tup) => tup
      case _ =>
        throw new UnexpectedType(
          s"Cannot unwrap the value `${arg}` into a MapResult value. It is not a list result!"
        )
    }

  def unwrapBoolean[TA, VA](arg: Result[TA, VA]): Boolean =
    arg match {
      case Primitive.Boolean(v) => v
      case _: Primitive[_, _, _] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Boolean value because it was not a Primitive.Boolean"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Boolean. It is not a primitive!")
    }

  def unwrapDouble[TA, VA](arg: Result[TA, VA]): Double =
    arg match {
      case Primitive.Double(v) => v
      case _: Primitive[_, _, _] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Double value because it was not a Primitive.Double"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Double. It is not a primitive!")
    }

  def unwrapInt[TA, VA](arg: Result[TA, VA]): Int =
    arg match {
      case Primitive.Int(v) => v
      case Primitive.Long(v) =>
        if (v > Integer.MAX_VALUE)
          throw new UnexpectedType(s"Cannot unwrap ${arg} into an integer because it's value is too large")
        else
          v.toInt
      case _: Primitive[_, _, _] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Int value because it was not a Primitive.Int"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Int. It is not a primitive!")
    }

  def unwrapLong[TA, VA](arg: Result[TA, VA]): Long =
    arg match {
      case Primitive.Int(v)  => v.toLong
      case Primitive.Long(v) => v
      case _: Primitive[_, _, _] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a Long value because it was not a Primitive.Long"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive Long. It is not a primitive!")
    }

  def unwrapString[TA, VA](arg: Result[TA, VA]): String =
    arg match {
      case Primitive.String(v) => v
      case _: Primitive[_, _, _] =>
        throw new UnexpectedType(
          s"Could not unwrap the primitive `${arg}` into a String value because it was not a Primitive.String"
        )
      case _ =>
        throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive String. It is not a primitive!")
    }

  def unwrapPrimitive[TA, VA](arg: Result[TA, VA]): Primitive[TA, VA, _] =
    arg match {
      case p: Primitive[_, _, _] => p.asInstanceOf[Primitive[TA, VA, _]]
      case _                     => throw new UnexpectedType(s"Cannot unwrap the value `${arg}` into a primitive")
    }

  def unwrapNumeric[TA, VA](arg: Result[TA, VA]): Primitive.Numeric[TA, VA, _] =
    arg match {
      case p: Primitive.Numeric[_, _, _] => p.asInstanceOf[Primitive.Numeric[TA, VA, _]]
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
  def unwrapNumericsWithHelper[TA, VA, N](
      arg1: Result[TA, VA],
      arg2: Result[TA, VA]
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

  def unwrapNumericWithHelper[TA, VA, N](arg: Result[TA, VA]): NumericWithHelper[N] = {
    val a = unwrapNumeric(arg)
    NumericWithHelper[N](
      a.value.asInstanceOf[N],
      a.numericHelper.asInstanceOf[scala.Numeric[N]],
      a.fractionalHelper.asInstanceOf[Option[scala.Fractional[N]]],
      a.integralHelper.asInstanceOf[Option[scala.Integral[N]]]
    )
  }

  case class Unit[TA, VA]() extends Result[TA, VA] {
    override def succinct(depth: Int) = "Unit"
  }

  sealed trait Primitive[TA, VA, T] extends Result[TA, VA] {
    def value: T
    def isNumeric                     = false
    override def succinct(depth: Int) = s"Primitive($value)"
  }

  object Primitive {
    sealed trait Numeric[TA, VA, T] extends Primitive[TA, VA, T] {
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
        case object Double     extends Numeric.Type
        case object BigDecimal extends Numeric.Type
      }
    }

    case class Int[TA, VA](value: scala.Int) extends Numeric[TA, VA, scala.Int] {
      val numericType           = Numeric.Type.Int
      lazy val numericHelper    = implicitly[scala.Numeric[scala.Int]]
      lazy val fractionalHelper = None
      lazy val integralHelper   = Some(implicitly[scala.Integral[scala.Int]])
    }
    case class Long[TA, VA](value: scala.Long) extends Numeric[TA, VA, scala.Long] {
      val numericType           = Numeric.Type.Long
      lazy val numericHelper    = implicitly[scala.Numeric[scala.Long]]
      lazy val fractionalHelper = None
      lazy val integralHelper   = Some(implicitly[scala.Integral[scala.Long]])
    }
    case class Double[TA, VA](value: scala.Double) extends Numeric[TA, VA, scala.Double] {
      val numericType           = Numeric.Type.Double
      lazy val numericHelper    = implicitly[scala.Numeric[scala.Double]]
      lazy val fractionalHelper = Some(implicitly[scala.Fractional[scala.Double]])
      lazy val integralHelper   = None
    }
    case class BigDecimal[TA, VA](value: scala.BigDecimal) extends Numeric[TA, VA, scala.BigDecimal] {
      val numericType           = Numeric.Type.BigDecimal
      lazy val numericHelper    = implicitly[scala.Numeric[scala.BigDecimal]]
      lazy val fractionalHelper = Some(implicitly[scala.Fractional[scala.BigDecimal]])
      lazy val integralHelper   = None
    }
    // TODO Morphir Float type is a double, why do we have this???
    case class Float[TA, VA](value: scala.Float) extends Numeric[TA, VA, scala.Float] {
      val numericType           = Numeric.Type.Float
      lazy val numericHelper    = implicitly[scala.Numeric[scala.Float]]
      lazy val fractionalHelper = Some(implicitly[scala.Fractional[scala.Float]])
      lazy val integralHelper   = None
    }

    object DecimalBounded {
      def unapply(resultValue: Result.Primitive.Numeric[_, _, _]): Option[scala.BigDecimal] =
        resultValue match {
          case Primitive.Double(v)     => Some(scala.BigDecimal(v))
          case Primitive.Float(v)      => Some(scala.BigDecimal(v.toDouble))
          case Primitive.Int(v)        => Some(scala.BigDecimal(v))
          case Primitive.Long(v)       => Some(scala.BigDecimal(v))
          case Primitive.BigDecimal(v) => Some(v)
        }
    }

    object LongBounded {
      def unapply[TA, VA](resultValue: Result[TA, VA]): Option[scala.Long] =
        resultValue match {
          case Primitive.Long(v) => Some(v)
          case Primitive.Int(v)  => Some(v.toLong)
          case _                 => None
        }
    }

    case class Boolean[TA, VA](value: scala.Boolean)   extends Primitive[TA, VA, scala.Boolean]
    case class String[TA, VA](value: java.lang.String) extends Primitive[TA, VA, java.lang.String]
    case class Char[TA, VA](value: scala.Char)         extends Primitive[TA, VA, scala.Char]

    def unapply(prim: Primitive[_, _, _]): Option[Any] =
      Some(prim.value)

    def makeOrFail[TA, VA, T](value: T): Primitive[TA, VA, T] =
      make[TA, VA, T](value) match {
        case Some(value) => value
        case None => throw new UnexpectedType(
            s"Cannot unwrap value `$value` into a primitive. It is a ${value.getClass}. Valid Primitive values are: Int, Long, String, Boolean, Char, Double, BigDecimal, Float"
          )
      }

    def make[TA, VA, T](value: T): Option[Primitive[TA, VA, T]] =
      value match {
        case v: scala.Int        => Some(Primitive.Int[TA, VA](v).asInstanceOf[Primitive[TA, VA, T]])
        case v: scala.Long       => Some(Primitive.Long[TA, VA](v).asInstanceOf[Primitive[TA, VA, T]])
        case v: java.lang.String => Some(Primitive.String[TA, VA](v).asInstanceOf[Primitive[TA, VA, T]])
        case v: scala.Boolean    => Some(Primitive.Boolean[TA, VA](v).asInstanceOf[Primitive[TA, VA, T]])
        case v: scala.Char       => Some(Primitive.Char[TA, VA](v).asInstanceOf[Primitive[TA, VA, T]])
        case v: scala.Double     => Some(Primitive.Double[TA, VA](v).asInstanceOf[Primitive[TA, VA, T]])
        case v: scala.BigDecimal => Some(Primitive.BigDecimal[TA, VA](v).asInstanceOf[Primitive[TA, VA, T]])
        case v: scala.Float      => Some(Primitive.Float[TA, VA](v).asInstanceOf[Primitive[TA, VA, T]])
        case _                   => None
      }
  }

  case class LocalDate[TA, VA](value: java.time.LocalDate) extends Result[TA, VA] {
    override def succinct(depth: Int) = s"LocalDate($value)"
  }

  case class LocalTime[TA, VA](value: java.time.LocalTime) extends Result[TA, VA] {
    override def succinct(depth: Int) = s"LocalTime($value)"
  }

  case class Tuple[TA, VA](elements: TupleSigniture[TA, VA]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "Tuple(...)"
    else {
      s"Tuple(${elements.toList.map(_.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class SetResult[TA, VA](elements: mutable.LinkedHashSet[Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "Set(..)"
    else {
      s"Set(${elements.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class Record[TA, VA](elements: Map[Name, Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "Record(..)"
    else {
      s"Record(${elements.map { case (key, value) => s"$key -> ${value.succinct(depth - 1)}" }.mkString(", ")})"
    }
  }

  case class ListResult[TA, VA](elements: List[Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "List(..)"
    else {
      s"List(${elements.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class MapResult[TA, VA](elements: mutable.LinkedHashMap[Result[TA, VA], Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) "Dict(..)"
    else {
      s"Dict(${elements.map { case (key, value) => s"${key.succinct(depth - 1)} -> ${value.succinct(depth - 1)}" }.mkString(", ")})"
    }
  }
  case class Applied[TA, VA](
      body: Value[TA, VA],
      curried: List[(Name, Result[TA, VA])],
      closingContext: CallStackFrame[TA, VA]
  )

  case class FieldFunction[TA, VA](fieldName: Name) extends Result[TA, VA]

  case class LambdaFunction[TA, VA](body: Value[TA, VA], pattern: Pattern[VA], closingContext: CallStackFrame[TA, VA])
      extends Result[TA, VA]

  case class DefinitionFunction[TA, VA](
      body: Value[TA, VA],
      arguments: List[(Name, VA, Type.Type[TA])],
      curried: List[(Name, Result[TA, VA])],
      closingContext: CallStackFrame[TA, VA]
  ) extends Result[TA, VA]

  case class ConstructorFunction[TA, VA](name: FQName, arguments: List[VA], curried: List[Result[TA, VA]])
      extends Result[TA, VA]

  case class ConstructorResult[TA, VA](name: FQName, values: List[Result[TA, VA]]) extends Result[TA, VA] {
    override def succinct(depth: Int) = if (depth == 0) s"${name.toString}(..)"
    else {
      s"${name.toString}(${values.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  case class NativeFunction[TA, VA](
      arguments: Int,
      curried: List[Result[TA, VA]],
      function: NativeFunctionSignature[TA, VA]
  ) extends Result[TA, VA] {}
}
