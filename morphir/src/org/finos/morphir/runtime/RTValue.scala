package org.finos.morphir.runtime

import org.finos.morphir.ir.Type.{FieldT, Type}
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Value.{Pattern, TypedValue, Value}
import org.finos.morphir.ir.{Module, Type}
import org.finos.morphir.naming.*
import Name.toTitleCase
import org.finos.morphir.MInt
import org.finos.morphir.datamodel.Concept.Result
import org.finos.morphir.runtime.internal.{
  CallStackFrame,
  DynamicNativeFunction1,
  NativeFunctionSignature,
  NativeFunctionSignatureAdv
}
import org.finos.morphir.runtime.MorphirRuntimeError.{FailedCoercion, IllegalValue}
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.runtime.sdk.KeySDK
import org.finos.morphir.util.PrintRTValue

import scala.annotation.tailrec
import scala.collection.immutable.{List as ScalaList, Map as ScalaMap, Set as ScalaSet}
import scala.collection.mutable

// TODO Integrate errors into reporting format
// Represents a Morphir-Evaluator result. Typed on TypedValue
// instead of a a Generic VA/TA since the latter is not necessary.
sealed trait RTValue {
  def succinct(depth: Int): String = s"${this.getClass} (Default implementation)"
  def succinct: String             = succinct(2)
  def printed                      = PrintRTValue(this).plainText
}

object RTValue {
  sealed trait ValueResult[T] extends RTValue {
    def value: T
  }
  sealed trait Function extends RTValue

  def coerceAggregation(arg: RTValue): Aggregation =
    arg match {
      case f: Aggregation => f
      case _              => throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a Aggregation")
    }

  def coerceList(arg: RTValue) =
    arg match {
      case v: RTValue.List => v
      case _ =>
        throw new FailedCoercion(
          s"Cannot unwrap the value `${arg}` into a ListResult value. It is not a List-based result!"
        )
    }

  def coerceConstructorResult(arg: RTValue) =
    arg match {
      case v: RTValue.ConstructorResult => v
      case _ =>
        throw new FailedCoercion(
          s"Cannot unwrap the value `${arg}` into a ConstructorResult value. It is not a Constructor result!"
        )
    }

  def coerceSet(arg: RTValue) =
    arg match {
      case v: RTValue.Set => v
      case _ =>
        throw new FailedCoercion(
          s"Cannot unwrap the value `${arg}` into a SetResult value. It is not a Set-based result!"
        )
    }

  def coerceMap(arg: RTValue) =
    arg match {
      case v: RTValue.Map => v
      case _ =>
        throw new FailedCoercion(
          s"Cannot unwrap the value `${arg}` into a MapResult value. It is not a Map-based result!"
        )
    }

  def coerceTuple(arg: RTValue) =
    arg match {
      case v: RTValue.Tuple => v
      case _ =>
        throw new FailedCoercion(
          s"Cannot unwrap the value `${arg}` into a MapResult value. It is not a list result!"
        )
    }

  def coerceBoolean(arg: RTValue) =
    arg match {
      case v: Primitive.Boolean => v
      case _: Primitive[_] =>
        throw new FailedCoercion(
          s"Could not unwrap the primitive `${arg}` into a Boolean value because it was not a Primitive.Boolean"
        )
      case _ =>
        throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a primitive Boolean. It is not a primitive!")
    }

  // A Moprhir/ELM float is a Java Double, calling this unwrapDouble since that's the Java datatype being returned
  def coerceDouble(arg: RTValue) =
    arg match {
      case v: Primitive.Float => v
      case _: Primitive[_] =>
        throw new FailedCoercion(
          s"Could not unwrap the primitive `${arg}` into a Double value because it was not a Primitive.Double"
        )
      case _ =>
        throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a primitive Double. It is not a primitive!")
    }

  def coerceInt(arg: RTValue) =
    arg match {
      case v: Primitive.Int => v
      case _: Primitive[_] =>
        throw new FailedCoercion(
          s"Could not unwrap the primitive `${arg}` into a Int value because it was not a Primitive.Int"
        )
      case _ =>
        throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a primitive Int. It is not a primitive!")
    }

  def coerceKey(arg: RTValue): Key =
    arg match {
      case k: Key      => k
      case rt: RTValue => Key1(rt) // keys are arbitrary, so if not a key, just make it a Key1
    }

  def coerceFloat(arg: RTValue) =
    arg match {
      case v: Primitive.Float => v
      case _: Primitive[_] =>
        throw new FailedCoercion(
          s"Could not unwrap the primitive `${arg}` into a Float value because it was not a Primitive.Float"
        )
      case _ =>
        throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a primitive Float. It is not a primitive!")
    }

  def coerceDecimal(arg: RTValue) =
    arg match {
      case v: Primitive.BigDecimal => v
      case _: Primitive[_] =>
        throw new FailedCoercion(
          s"Could not unwrap the primitive `${arg}` into a BigDecimal value because it was not a Primitive.BigDecimal"
        )
      case _ =>
        throw FailedCoercion(
          s"Cannot unwrap the value `${arg}` into a primitive BigDecimal. It is not a primitive!"
        )
    }

  def coerceLong(arg: RTValue) =
    arg match {
      case v: Primitive.Int => v
      case _: Primitive[_] =>
        throw new FailedCoercion(
          s"Could not unwrap the primitive `${arg}` into a Long value because it was not a Primitive.Long"
        )
      case _ =>
        throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a primitive Long. It is not a primitive!")
    }

  def coerceString(arg: RTValue) =
    arg match {
      case v: Primitive.String => v
      case _: Primitive[_] =>
        throw new FailedCoercion(
          s"Could not unwrap the primitive `${arg}` into a String value because it was not a Primitive.String"
        )
      case _ =>
        throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a primitive String. It is not a primitive!")
    }

  def coerceChar(arg: RTValue) =
    arg match {
      case v: Primitive.Char => v
      case _: Primitive[_] =>
        throw new FailedCoercion(
          s"Could not unwrap the primitive `${arg}` into a Char value because it was not a Primitive.Char"
        )
      case _ =>
        throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a primitive Char. It is not a primitive!")
    }

  def coercePrimitive(arg: RTValue): Primitive[_] =
    arg match {
      case p: Primitive[_] => p.asInstanceOf[Primitive[_]]
      case _               => throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a primitive")
    }

  def coerceComparable(arg: RTValue): Comparable =
    arg match {
      case c: Comparable => c
      case _             => throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a Comparable")
    }

  def coerceNumeric(arg: RTValue): Primitive.Numeric[_] =
    arg match {
      // Need a typecast because can't match on `p: Primitive.Numeric[_]` since there's
      // no actually Primitive.Numeric that has a type of `Any`.
      case p: Primitive.Numeric[_] => p
      case _ => throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a primitive numeric")
    }

  def coerceFunction(arg: RTValue): Function =
    arg match {
      case f: Function => f
      case _           => throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a function")
    }

  def coerceLocalDate(arg: RTValue): LocalDate =
    arg match {
      case ld: LocalDate => ld
      case _             => throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a LocalDate")
    }

  def coerceLocalTime(arg: RTValue): LocalTime =
    arg match {
      case lt: LocalTime => lt
      case _             => throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a LocalTime")
    }

  def coerceMonth(arg: RTValue): ConstructorResult =
    arg match {
      case month: ConstructorResult if Month.isMonth(month) => month
      case _ => throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a Month")
    }

  def coerceDayOfWeek(arg: RTValue): ConstructorResult =
    arg match {
      case dayOfWeek: ConstructorResult if DayOfWeek.isDayOfWeek(dayOfWeek) => dayOfWeek
      case _ => throw new FailedCoercion(s"Cannot unwrap the value `${arg}` into a DayOfWeek")
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
      throw new FailedCoercion(
        s"Error unwrapping the Primitive Numerics ${arg1} and ${arg2} into a common type, they have different numeric types: ${a
            .numericType} versus ${b.numericType}"
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

  object Order {
    private def constructorResult(fqn: FQName): ConstructorResult = ConstructorResult(fqn, ScalaList())
    val GTFQN                                                     = FQName.fromString("Morphir.SDK:Basics:GT")
    val LTFQN                                                     = FQName.fromString("Morphir.SDK:Basics:LT")
    val EQFQN                                                     = FQName.fromString("Morphir.SDK:Basics:EQ")

    val allFqns: ScalaSet[FQName] = ScalaList(GTFQN, LTFQN, EQFQN).toSet

    val GT = constructorResult(GTFQN)
    val LT = constructorResult(LTFQN)
    val EQ = constructorResult(EQFQN)
  }

  /**
   * Trait for types that *may* be comparable in Morphir; Note that not everythign that extends this may actually be
   * compared. For instance, lists are comparable if and only if their elements are comparable.
   */
  sealed trait Comparable extends RTValue {
    final def compare(that: Comparable): Int =
      Comparable.compareOrThrow(this, that)
  }
  object Comparable {
    def compare(first: Comparable, second: Comparable): Either[IllegalValue, Int] = {
      def lexicalHelper(first: scala.List[RTValue], second: scala.List[RTValue]) = {
        val zipped = first.zip(second)
        val mapped = zipped.map {
          case (a: Comparable, b: Comparable) => recursiveHelper(a, b)
          case (a, b: Comparable) =>
            Left(IllegalValue(s"Cannot compare values $a and $b because the first is not comparable"))
          case (a: Comparable, b) =>
            Left(IllegalValue(s"Cannot compare values $a and $b because the second is not comparable"))
          case (a, b) => Left(IllegalValue(s"Cannot compare values $a and $b because neither is comparable"))
        }
        mapped.find(_.isLeft) match {
          case Some(error) => error
          case None =>
            val rights       = mapped.map(_.getOrElse(throw new Exception("unreachable branch reached")))
            val firstNonZero = rights.find(_ != 0)
            firstNonZero match {
              case Some(value) => Right(value)
              case None        => Right(first.length.compare(second.length))
            }
        }
      }
      // This helper function exists just to neatly wrap leaf errors with context from the top-level comparison that was attempted
      def recursiveHelper(first: Comparable, second: Comparable): Either[IllegalValue, Int] =
        (first, second) match {
          case (Primitive.Int(a), Primitive.Int(b))       => Right(a.compare(b))
          case (Primitive.Float(a), Primitive.Float(b))   => Right(a.compare(b))
          case (Primitive.Char(a), Primitive.Char(b))     => Right(a.compare(b))
          case (Primitive.String(a), Primitive.String(b)) => Right(a.compare(b))
          case (Tuple(a_elements), Tuple(b_elements)) =>
            if (a_elements.length == b_elements.length) lexicalHelper(a_elements, b_elements)
            else Left(IllegalValue(s"Cannot compare tuples $first and $second because they have different lengths"))
          case (List(a_elements), List(b_elements)) => lexicalHelper(a_elements, b_elements)
          case (firstOther, secondOther) => Left(IllegalValue(s"Cannot compare values $firstOther and $secondOther"))
        }
      recursiveHelper(first, second) match {
        case Left(error) => Left(error.withContext(s"While comparing $first and $second"))
        case right       => right
      }
    }
    def compareOrThrow(first: Comparable, second: Comparable): Int =
      compare(first, second) match {
        case Left(error)  => throw error
        case Right(value) => value
      }
    def intToOrder(i: Int): RTValue =
      if (i == 0) RTValue.Order.EQ
      else if (i > 0) RTValue.Order.GT
      else RTValue.Order.LT

    def orderToInt(order: RTValue): Int =
      order match {
        case RTValue.Order.EQ => 0
        case RTValue.Order.GT => 1
        case RTValue.Order.LT => -1
        case _                => throw IllegalValue(s"Tried to convert $order to an integer as if it were an Order")
      }
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

    case class Int(value: MInt) extends Numeric[MInt] with Comparable {
      val numericType      = Numeric.Type.Int
      def numericHelper    = org.finos.morphir.mIntIsNumeric
      def fractionalHelper = None
      def integralHelper   = Some(org.finos.morphir.mIntIsIntegral)
      def valueAsInt =
        if (value.isValidInt)
          value.toInt
        else
          throw new FailedCoercion(
            s"Cannot unwrap ${value} into an integer because it's value is too large or too small"
          )
    }
    object Int {
      def apply(value: scala.Int)  = new Int(MInt.fromInt(value))
      def apply(value: scala.Long) = new Int(MInt.fromLong(value))
    }

    // A Morphir/ELM Float is the same as a Java Double
    case class Float(value: scala.Double) extends Numeric[scala.Double] with Comparable {
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
    case class String(value: java.lang.String) extends Primitive[java.lang.String] with Comparable
    case class Char(value: scala.Char)         extends Primitive[scala.Char] with Comparable

    def unapply(prim: Primitive[_]): Option[Any] =
      Some(prim.value)

    def makeOrFail[T](value: T): Primitive[T] =
      make[T](value) match {
        case Some(value) => value
        case None => throw new FailedCoercion(
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

  object Month {
    private final case class MonthData(
        fqn: FQName,
        javaMonth: java.time.Month
    ) {
      val constructorResult: ConstructorResult = ConstructorResult(fqn, ScalaList())
    }

    private object MonthData {
      val January   = MonthData(fqn"Morphir.SDK:LocalDate:January", java.time.Month.JANUARY)
      val February  = MonthData(fqn"Morphir.SDK:LocalDate:February", java.time.Month.FEBRUARY)
      val March     = MonthData(fqn"Morphir.SDK:LocalDate:March", java.time.Month.MARCH)
      val April     = MonthData(fqn"Morphir.SDK:LocalDate:April", java.time.Month.APRIL)
      val May       = MonthData(fqn"Morphir.SDK:LocalDate:May", java.time.Month.MAY)
      val June      = MonthData(fqn"Morphir.SDK:LocalDate:June", java.time.Month.JUNE)
      val July      = MonthData(fqn"Morphir.SDK:LocalDate:July", java.time.Month.JULY)
      val August    = MonthData(fqn"Morphir.SDK:LocalDate:August", java.time.Month.AUGUST)
      val September = MonthData(fqn"Morphir.SDK:LocalDate:September", java.time.Month.SEPTEMBER)
      val October   = MonthData(fqn"Morphir.SDK:LocalDate:October", java.time.Month.OCTOBER)
      val November  = MonthData(fqn"Morphir.SDK:LocalDate:November", java.time.Month.NOVEMBER)
      val December  = MonthData(fqn"Morphir.SDK:LocalDate:December", java.time.Month.DECEMBER)

      val all: ScalaSet[MonthData] =
        ScalaSet(January, February, March, April, May, June, July, August, September, October, November, December)

      val byConstructorResult: ScalaMap[ConstructorResult, MonthData] = all.map(m => m.constructorResult -> m).toMap
      val byJavaMonth: ScalaMap[java.time.Month, MonthData]           = all.map(m => m.javaMonth -> m).toMap
    }

    val January: ConstructorResult   = MonthData.January.constructorResult
    val February: ConstructorResult  = MonthData.February.constructorResult
    val March: ConstructorResult     = MonthData.March.constructorResult
    val April: ConstructorResult     = MonthData.April.constructorResult
    val May: ConstructorResult       = MonthData.May.constructorResult
    val June: ConstructorResult      = MonthData.June.constructorResult
    val July: ConstructorResult      = MonthData.July.constructorResult
    val August: ConstructorResult    = MonthData.August.constructorResult
    val September: ConstructorResult = MonthData.September.constructorResult
    val October: ConstructorResult   = MonthData.October.constructorResult
    val November: ConstructorResult  = MonthData.November.constructorResult
    val December: ConstructorResult  = MonthData.December.constructorResult

    val allFqns: ScalaSet[FQName] = MonthData.all.map(_.fqn).toSet

    def isMonth(constructorResult: ConstructorResult): Boolean =
      MonthData.byConstructorResult.contains(constructorResult)

    def fromJavaMonth(month: java.time.Month): ConstructorResult = {
      val monthData = MonthData.byJavaMonth.get(month)
        .getOrElse(throw new Exception(s"unreachable branch reached: unknown java.time.Month $month"))
      monthData.constructorResult
    }

    def fromConstructorResult(monthConstructorResult: RTValue.ConstructorResult): Option[java.time.Month] =
      MonthData.byConstructorResult.get(monthConstructorResult).map(_.javaMonth)

    def coerceJavaMonth(monthArg: RTValue): java.time.Month =
      fromConstructorResult(coerceMonth(monthArg))
        .getOrElse(throw new FailedCoercion(s"Cannot unwrap the value `${monthArg}` into a Month"))

    def unapply(arg: RTValue): Option[java.time.Month] =
      arg match {
        case cr: ConstructorResult => fromConstructorResult(cr)
        case _                     => None
      }
  }

  object DayOfWeek {
    private final case class DayOfWeekData(
        fqn: FQName,
        javaDayOfWeek: java.time.DayOfWeek
    ) {
      val constructorResult: ConstructorResult = ConstructorResult(fqn, ScalaList())
    }

    private object DayOfWeekData {
      val Monday    = DayOfWeekData(fqn"Morphir.SDK:LocalDate:Monday", java.time.DayOfWeek.MONDAY)
      val Tuesday   = DayOfWeekData(fqn"Morphir.SDK:LocalDate:Tuesday", java.time.DayOfWeek.TUESDAY)
      val Wednesday = DayOfWeekData(fqn"Morphir.SDK:LocalDate:Wednesday", java.time.DayOfWeek.WEDNESDAY)
      val Thursday  = DayOfWeekData(fqn"Morphir.SDK:LocalDate:Thursday", java.time.DayOfWeek.THURSDAY)
      val Friday    = DayOfWeekData(fqn"Morphir.SDK:LocalDate:Friday", java.time.DayOfWeek.FRIDAY)
      val Saturday  = DayOfWeekData(fqn"Morphir.SDK:LocalDate:Saturday", java.time.DayOfWeek.SATURDAY)
      val Sunday    = DayOfWeekData(fqn"Morphir.SDK:LocalDate:Sunday", java.time.DayOfWeek.SUNDAY)

      val all: ScalaSet[DayOfWeekData] = ScalaSet(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)

      val byConstructorResult: ScalaMap[ConstructorResult, DayOfWeekData] = all.map(m => m.constructorResult -> m).toMap
      val byJavaDayOfWeek: ScalaMap[java.time.DayOfWeek, DayOfWeekData]   = all.map(m => m.javaDayOfWeek -> m).toMap
    }

    val Monday    = DayOfWeekData.Monday.constructorResult
    val Tuesday   = DayOfWeekData.Tuesday.constructorResult
    val Wednesday = DayOfWeekData.Wednesday.constructorResult
    val Thursday  = DayOfWeekData.Thursday.constructorResult
    val Friday    = DayOfWeekData.Friday.constructorResult
    val Saturday  = DayOfWeekData.Saturday.constructorResult
    val Sunday    = DayOfWeekData.Sunday.constructorResult

    val allFqns: ScalaSet[FQName] = DayOfWeekData.all.map(_.fqn).toSet

    def isDayOfWeek(constructorResult: ConstructorResult): Boolean =
      DayOfWeekData.byConstructorResult.contains(constructorResult)

    def fromJavaDayOfWeek(dayOfWeek: java.time.DayOfWeek): ConstructorResult = {
      val dayOfWeekData = DayOfWeekData.byJavaDayOfWeek.get(dayOfWeek)
        .getOrElse(throw new Exception(s"unreachable branch reached: unknown java.time.DayOfWeek $dayOfWeek"))
      dayOfWeekData.constructorResult
    }

    def fromConstructorResult(dayOfWeekConstructorResult: RTValue.ConstructorResult): Option[java.time.DayOfWeek] =
      DayOfWeekData.byConstructorResult.get(dayOfWeekConstructorResult).map(_.javaDayOfWeek)

    def coerceJavaDayOfWeek(dayOfWeekArg: RTValue): java.time.DayOfWeek =
      fromConstructorResult(coerceMonth(dayOfWeekArg))
        .getOrElse(throw new FailedCoercion(s"Cannot unwrap the value `${dayOfWeekArg}` into a Month"))

    def unapply(arg: RTValue): Option[java.time.DayOfWeek] =
      arg match {
        case cr: ConstructorResult => fromConstructorResult(cr)
        case _                     => None
      }
  }

  case class Tuple(elements: scala.List[RTValue]) extends ValueResult[scala.List[RTValue]] with Comparable {
    def value = elements
    def asTuple: Option[(RTValue, RTValue)] = elements match {
      case scala.List(_1, _2) => Some((_1, _2))
      case _                  => None
    }
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

  case class List(elements: scala.List[RTValue]) extends ValueResult[scala.List[RTValue]] with Comparable {
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
      body: TypedValue,
      curried: scala.List[(Name, RTValue)],
      closingContext: CallStackFrame
  )

  object Key {
    @tailrec
    private def flattenTuples(list: scala.List[RTValue], tuple: scala.List[RTValue]): scala.List[RTValue] =
      tuple match {
        case scala.List(k1: RTValue, k2: RTValue, t1: List) => flattenTuples(scala.List(k1, k2), t1.value)
        case _                                              => list ++ tuple
      }

    private[RTValue] def flatten(keys: Tuple): scala.List[RTValue] = flattenTuples(scala.List[RTValue](), keys.value)
  }

  sealed trait Key extends ValueResult[scala.List[RTValue]] {}

  // format: off
  object Key0 extends Key {
    def value = scala.List.empty[RTValue]
    override def succinct(depth: Int) = s"Key0(0)"
  }

  case class Key1(key: RTValue) extends Key {
    def value = scala.List(key)
    override def succinct(depth: Int) = s"Key0(0)"
  }
  
  case class Key2(keys: Tuple) extends Key {
    def value = keys.value
  }

  case class Key3(keys: Tuple) extends Key {
    def value = keys.value
  }

  case class Key4(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key5(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key6(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key7(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key8(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key9(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key10(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key11(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key12(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key13(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key14(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key15(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  case class Key16(keys: Tuple) extends Key {
    def value = Key.flatten(keys)
  }

  // format: on

  object Aggregation {
    private val noFilter: RTValue => Primitive.Boolean = _ => Primitive.Boolean(true)

    private val key0 = NativeFunction(
      1,
      Nil,
      NativeFunctionSignature.Fun1(_ => Key0),
      CodeLocation.NativeFunction(FQName.fqn("Morphir.SDK", "Key", "key0"))
    )

    def apply(operation: List => Primitive.Float, key: Function = key0): Aggregation =
      Aggregation(operation, key, noFilter)
  }

  case class Aggregation(operation: List => Primitive.Float, key: Function, filter: RTValue => Primitive.Boolean)
      extends RTValue {
    def value                         = "Aggregation" // TODO Handle for printable value for logging
    override def succinct(depth: Int) = s"Aggregation($operation, $key, $filter)"
  }

  case class FieldFunction(fieldName: Name) extends Function

  case class LambdaFunction(
      body: TypedValue,
      pattern: Pattern[UType],
      closingContext: CallStackFrame,
      loc: CodeLocation.AnonymousFunction
  ) extends Function

  case class DefinitionFunction(
      body: TypedValue,
      arguments: scala.List[(Name, UType, UType)],
      curried: scala.List[(Name, RTValue)],
      closingContext: CallStackFrame,
      loc: CodeLocation
  ) extends Function

  case class ConstructorFunction(name: FQName, arguments: scala.List[UType], curried: scala.List[RTValue])
      extends Function

  case class ImplicitConstructorFunction(
      name: FQName,
      arguments: scala.List[FieldT[scala.Unit]],
      curried: collection.Map[Name, RTValue]
  ) extends Function

  // TODO: We are currently using this for Maybe and Result types; those should be promoted to their own RTValues
  case class ConstructorResult(name: FQName, values: scala.List[RTValue]) extends RTValue {
    override def succinct(depth: Int) = if (depth == 0) s"${name.toString}(..)"
    else {
      s"${name.toString}(${values.map(value => value.succinct(depth - 1)).mkString(", ")})"
    }
  }

  // GT, LT, Etc

  sealed trait NativeFunctionResult extends Function {
    def arguments: Int
    def curried: scala.List[RTValue]
    def loc: CodeLocation.NativeFunction
  }

  case class NativeFunction(
      arguments: Int,
      curried: scala.List[RTValue],
      function: NativeFunctionSignature,
      loc: CodeLocation.NativeFunction
  ) extends NativeFunctionResult {}

  case class NativeInnerFunction(
      arguments: Int,
      curried: scala.List[RTValue],
      function: NativeFunctionSignatureAdv,
      loc: CodeLocation.NativeFunction
  ) extends NativeFunctionResult {}
}
