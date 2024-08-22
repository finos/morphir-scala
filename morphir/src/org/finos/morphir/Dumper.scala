/*
 * Code inspired by the `Dumper` type class from the `zio-prelude` project.
 * Licensed under the Apache License, Version 2.0 as shown here: https://github.com/zio/zio-prelude/blob/a96ca8f91705bbc06ee5c4666b9cae64bb693cd1/LICENSE
 * https://github.com/zio/zio-prelude/blob/a96ca8f91705bbc06ee5c4666b9cae64bb693cd1/core/shared/src/main/scala/zio/prelude/Dumper.scala
 */
package org.finos.morphir

import java.util.concurrent.TimeUnit
import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.concurrent.duration.Duration as ScalaDuration

/**
 * `Dumper` is an abstraction that describes the ability to render a value of type `A` to some format (most likely human
 * readable).
 *
 * `Dumper` captures this information in a structured format called a `Repr`, or a representation of the data. This
 * representation can then be formatted to a target format using a `Formatter`. This formatter allows specification of
 * the target format (such as a `String` or `Json`) via its nested `Format` type.
 */
trait Dumper[-A] {
  def dumpRepr(a: A): Dumper.Repr
  final def dump(a: A): String = dumpRepr(a).dump
}

object Dumper extends DumperVersionSpecific with DumperSyntax {
  def apply[A](implicit dumper: Dumper[A]): Dumper[A] = dumper

  /// A `Formatter` knows how to format a `Repr` ro a target format.
  trait Formatter[+Out] extends ((Repr) => Out) {
    /// The target output format.
    type Format <: Out
  }

  object Formatter {

    /**
     * A renderer that renders the `Repr` as a string containing the full information in the `Repr`.
     */
    val Full: Formatter[String] = {
      case Repr.KeyValue(k, v) => s"key: ${k.dump(Full)} -> value: ${v.dump(Full)}"
      case Repr.Object(ns, n)  => (ns :+ n).mkString(".")
      case Repr.Constructor(ns, n, reprs) =>
        (ns :+ s"$n(${reprs.map(kv => s"${kv._1} = ${kv._2.dump(Full)}").mkString(", ")})").mkString(".")
      case Repr.VConstructor(ns, n, reprs) =>
        (ns :+ n).mkString(".") + s"(${reprs.map(_.dump(Full)).mkString(", ")})"
      case any => Simple(any)
    }

    /**
     * A `Formatter` that renders the `Repr` as valid Scala code that could be copy and pasted into an IDE or REPL.
     */
    val Scala: Formatter[String] = {
      case Repr.Float(v)       => v.toString
      case Repr.Long(v)        => v.toString
      case Repr.Char(v)        => v.toString
      case Repr.String(v)      => v
      case Repr.KeyValue(k, v) => s"${k.dump(Scala)} -> ${v.dump(Scala)}"
      case Repr.Object(_, n)   => n
      case Repr.Constructor(_, n, reprs) =>
        s"$n(${reprs.map(kv => kv._2.dump(Scala)).mkString(",")})"
      case Repr.VConstructor(_, n, reprs) if List("List", "Vector", "Map").contains(n) =>
        s"$n(${reprs.map(_.dump(Scala)).mkString(", ")})"
      case Repr.VConstructor(List("scala"), n, reprs) if n.matches("^Tuple\\d+$") =>
        s"(${reprs.map(_.dump(Scala)).mkString(",")})"
      case Repr.VConstructor(_, n, reprs) => s"$n(${reprs.map(_.dump(Scala)).mkString(",")})"
      case any                            => Simple(any)
    }

    /**
     * A formatter that renders the `Repr` as a simple string.
     */
    val Simple: Formatter[String] = {
      case Repr.Int(v)         => v.toString
      case Repr.Double(v)      => v.toString
      case Repr.Float(v)       => s"${v}f"
      case Repr.Long(v)        => s"${v}L"
      case Repr.Byte(v)        => v.toString
      case Repr.Char(v)        => s"'$v'"
      case Repr.Boolean(v)     => v.toString
      case Repr.Short(v)       => v.toString
      case Repr.String(v)      => s""""$v""""
      case Repr.KeyValue(k, v) => s"${k.dump(Simple)} -> ${v.dump(Simple)}"
      case Repr.Object(_, n)   => n
      case Repr.Constructor(_, n, reprs) =>
        s"$n(${reprs.map(kv => s"${kv._1} = ${kv._2.dump(Simple)}").mkString(", ")})"
      case Repr.VConstructor(List("scala"), n, reprs) if n.matches("^Tuple\\d+$") =>
        s"(${reprs.map(_.dump(Simple)).mkString(", ")})"
      case Repr.VConstructor(_, n, reprs) => s"$n(${reprs.map(_.dump(Simple)).mkString(", ")})"
    }
  }
  sealed trait Repr { self =>

    /**
     * Dump the `Repr` to a target format using the specified `Formatter
     */
    def dump[Format](formatter: Formatter[Format]): Format = formatter(self)

    /**
     * Dump the `Repr` to a human readable format using the `Simple` formatter.
     */
    def dump: String = dump(Formatter.Simple)
  }

  object Repr {

    import java.lang.{String => SString}
    import scala.{
      Boolean => SBoolean,
      Byte => SByte,
      Char => SChar,
      Double => SDouble,
      Float => SFloat,
      Int => SInt,
      Long => SLong,
      Short => SShort
    }

    implicit def deriveRepr[A](a: A)(implicit dumper: Dumper[A]): Repr = dumper.dumpRepr(a)

    final case class Boolean(value: SBoolean)                        extends Repr
    final case class Byte(value: SByte)                              extends Repr
    final case class Char(value: SChar)                              extends Repr
    final case class Double(value: SDouble)                          extends Repr
    final case class Float(value: SFloat)                            extends Repr
    final case class Int(value: SInt)                                extends Repr
    final case class Long(value: SLong)                              extends Repr
    final case class Short(value: SShort)                            extends Repr
    final case class String(value: SString)                          extends Repr
    final case class Object(namespace: List[SString], name: SString) extends Repr
    final case class VConstructor(
        namespace: List[SString],
        name: SString,
        reprs: List[Repr]
    ) extends Repr
    final case class Constructor(
        namespace: List[SString],
        name: SString,
        reprs: ListMap[SString, Repr]
    ) extends Repr

    object Constructor {

      /**
       * Constructs a structured representation of a class constructor with the specified namespace, name, and fields.
       */
      def apply(namespace: List[SString], name: SString, repr: (SString, Repr), reprs: (SString, Repr)*): Repr =
        new Constructor(namespace, name, ListMap(repr :: reprs.toList: _*))
    }
    final case class KeyValue(key: Repr, value: Repr) extends Repr
  }

  /**
   * Constructs a `Dumper` instance for a pair of a key and a value given `Dumper` instances for the key and value
   * types.
   */
  def keyValueDumper[A: Dumper, B: Dumper]: Dumper[(A, B)] =
    n => Repr.KeyValue(n._1.dumpRepr, n._2.dumpRepr)

  /**
   * Constructs a `Dumper[A]` from a function that converts a value of type `A` to a `Repr`.
   */
  def make[A](f: A => Dumper.Repr): Dumper[A] =
    f(_)

  /**
   * Derives a `Dumper[Array[A]]` given a `Dumper[A]`.
   */
  implicit def ArrayDumper[A: Dumper]: Dumper[Array[A]] =
    array => Repr.VConstructor(List("scala"), "Array", array.map(_.dumpRepr).toList)

  /**
   * The `Dumper` instance for `BigDecimal`.
   */
  implicit val BigDecimalDumper: Dumper[BigDecimal] =
    bigDecimal =>
      Repr.VConstructor(
        List("scala", "math"),
        "BigDecimal",
        List(bigDecimal.toString.dumpRepr, bigDecimal.mc.dumpRepr)
      )

  /**
   * The `Dumper` instance for `BigInt`.
   */
  implicit val BigIntDumper: Dumper[BigInt] =
    bigInt => Repr.VConstructor(List("scala", "math"), "BigInt", List(bigInt.toString.dumpRepr))

  /**
   * The `Dumper` instance for `Boolean`.
   */
  implicit val BooleanDumper: Dumper[Boolean] =
    Repr.Boolean(_)

  /**
   * The `Dumper` instance for `Byte`.
   */
  implicit val ByteDumper: Dumper[Byte] =
    Repr.Byte(_)

  /**
   * The `Dumper` instance for `Char`.
   */
  implicit val CharDumper: Dumper[Char] =
    Repr.Char(_)

  /**
   * The `Dumper` instance for `Double`.
   */
  implicit val DoubleDumper: Dumper[Double] =
    Repr.Double(_)

  /**
   * The `Dumper` instance for `scala.concurrent.Duration`.
   */
  implicit val DurationScalaDumper: Dumper[ScalaDuration] = {
    val namespace            = List("scala", "concurrent", "duration")
    val constructor          = "Duration"
    val namespaceConstructor = namespace ++ List(constructor)

    {
      case ScalaDuration.Zero      => Repr.Object(namespaceConstructor, "Zero")
      case ScalaDuration.Inf       => Repr.Object(namespaceConstructor, "Inf")
      case ScalaDuration.MinusInf  => Repr.Object(namespaceConstructor, "MinusInf")
      case ScalaDuration.Undefined => Repr.Object(namespaceConstructor, "Undefined")
      case d =>
        val (length, unit) = nanosToPrettyUnit(d.toNanos)
        Repr.Constructor(
          namespace,
          constructor,
          ("length", Repr.Long(length)),
          ("unit", unit.dumpRepr)
        )
    }
  }

  /**
   * Derives a `Dumper[Either[E, A]]` given a `Dumper[E]` and a `Dumper[A]`.
   */
  implicit def EitherDumper[E: Dumper, A: Dumper]: Dumper[Either[E, A]] = {
    case Left(e)  => Repr.VConstructor(List("scala"), "Left", List(e.dumpRepr))
    case Right(a) => Repr.VConstructor(List("scala"), "Right", List(a.dumpRepr))
  }

  /**
   * The `Dumper` instance for `Float`.
   */
  implicit val FloatDumper: Dumper[Float] =
    Repr.Float(_)

  /**
   * The `Dumper` instance for `Int`.
   */
  implicit val IntDumper: Dumper[Int] =
    Repr.Int(_)

  /**
   * Derives a `Dumper[List[A]]` given a `Dumper[A]`.
   */
  implicit def ListDumper[A: Dumper]: Dumper[List[A]] =
    list => Repr.VConstructor(List("scala"), "List", list.map(_.dumpRepr))

  /**
   * The `Dumper` instance for `Long`.
   */
  implicit val LongDumper: Dumper[Long] =
    Repr.Long(_)

  /**
   * The `Dumper` instance for `java.math.MathContext`.
   */
  implicit val MathContextDumper: Dumper[java.math.MathContext] =
    mc =>
      Repr.VConstructor(
        List("java", "math"),
        "MathContext",
        List(mc.getPrecision.dumpRepr, mc.getRoundingMode.dumpRepr)
      )

  /**
   * Derives a `Dumper[Map[K, V]]` given a `Dumper[K]` and a `Dumper[V]`.
   */
  implicit def MapDumper[K: Dumper, V: Dumper]: Dumper[Map[K, V]] =
    map => Repr.VConstructor(List("scala"), "Map", map.map(_.dumpRepr(keyValueDumper)).toList)

  /**
   * Derives a `Dumper[Option[A]]` given a `Dumper[A]`.
   */
  implicit def OptionDumper[A: Dumper]: Dumper[Option[A]] = {
    case None    => Repr.Object(List("scala"), "None")
    case Some(a) => Repr.VConstructor(List("scala"), "Some", List(a.dumpRepr))
  }

  /**
   * The `Dumper` instance for `java.math.RoundingMode`.
   */
  implicit val RoundingModeDumper: Dumper[java.math.RoundingMode] = {
    case java.math.RoundingMode.CEILING     => Repr.Object(List("java", "math"), "RoundingMode.CEILING")
    case java.math.RoundingMode.DOWN        => Repr.Object(List("java", "math"), "RoundingMode.DOWN")
    case java.math.RoundingMode.FLOOR       => Repr.Object(List("java", "math"), "RoundingMode.FLOOR")
    case java.math.RoundingMode.HALF_DOWN   => Repr.Object(List("java", "math"), "RoundingMode.HALF_DOWN")
    case java.math.RoundingMode.HALF_EVEN   => Repr.Object(List("java", "math"), "RoundingMode.HALF_EVEN")
    case java.math.RoundingMode.HALF_UP     => Repr.Object(List("java", "math"), "RoundingMode.HALF_UP")
    case java.math.RoundingMode.UNNECESSARY => Repr.Object(List("java", "math"), "RoundingMode.UNNECESSARY")
    case java.math.RoundingMode.UP          => Repr.Object(List("java", "math"), "RoundingMode.UP")
  }

  /**
   * The `Dumper` instance for `Short`.
   */
  implicit val ShortDumper: Dumper[Short] =
    Repr.Short(_)

  /**
   * The `Dumper` instance for `String`.
   */
  implicit val StringDumper: Dumper[String] =
    Repr.String(_)

  /**
   * The `Dumper` instance for `TimeUnit`.
   */
  implicit val TimeUnitDumper: Dumper[TimeUnit] = tu =>
    Repr.Object(
      List("java", "util", "concurrent", "TimeUnit"),
      tu match {
        case TimeUnit.NANOSECONDS  => "NANOSECONDS"
        case TimeUnit.MICROSECONDS => "MICROSECONDS"
        case TimeUnit.MILLISECONDS => "MILLISECONDS"
        case TimeUnit.SECONDS      => "SECONDS"
        case TimeUnit.MINUTES      => "SECONDS"
        case TimeUnit.HOURS        => "HOURS"
        case TimeUnit.DAYS         => "DAYS"
      }
    )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple2Dumper[A: Dumper, B: Dumper]: Dumper[(A, B)] =
    tup2 => Repr.VConstructor(List("scala"), "Tuple2", List(tup2._1.dumpRepr, tup2._2.dumpRepr))

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple3Dumper[A: Dumper, B: Dumper, C: Dumper]: Dumper[(A, B, C)] =
    tuple => Repr.VConstructor(List("scala"), "Tuple3", List(tuple._1.dumpRepr, tuple._2.dumpRepr, tuple._3.dumpRepr))

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple4Dumper[A: Dumper, B: Dumper, C: Dumper, D: Dumper]: Dumper[(A, B, C, D)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple4",
        List(tuple._1.dumpRepr, tuple._2.dumpRepr, tuple._3.dumpRepr, tuple._4.dumpRepr)
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple5Dumper[A: Dumper, B: Dumper, C: Dumper, D: Dumper, E: Dumper]: Dumper[(A, B, C, D, E)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple5",
        List(tuple._1.dumpRepr, tuple._2.dumpRepr, tuple._3.dumpRepr, tuple._4.dumpRepr, tuple._5.dumpRepr)
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple6Dumper[A: Dumper, B: Dumper, C: Dumper, D: Dumper, E: Dumper, F: Dumper]
      : Dumper[(A, B, C, D, E, F)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple6",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple7Dumper[A: Dumper, B: Dumper, C: Dumper, D: Dumper, E: Dumper, F: Dumper, G: Dumper]
      : Dumper[(A, B, C, D, E, F, G)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple7",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple8Dumper[A: Dumper, B: Dumper, C: Dumper, D: Dumper, E: Dumper, F: Dumper, G: Dumper, H: Dumper]
      : Dumper[(A, B, C, D, E, F, G, H)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple8",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple9Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple9",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple10Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple10",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple11Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple11",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple12Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple12",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple13Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple13",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple14Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper,
      N: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple14",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr,
          tuple._14.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple15Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper,
      N: Dumper,
      O: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple15",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr,
          tuple._14.dumpRepr,
          tuple._15.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple16Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper,
      N: Dumper,
      O: Dumper,
      P: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple16",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr,
          tuple._14.dumpRepr,
          tuple._15.dumpRepr,
          tuple._16.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple17Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper,
      N: Dumper,
      O: Dumper,
      P: Dumper,
      Q: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple17",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr,
          tuple._14.dumpRepr,
          tuple._15.dumpRepr,
          tuple._16.dumpRepr,
          tuple._17.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple18Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper,
      N: Dumper,
      O: Dumper,
      P: Dumper,
      Q: Dumper,
      R: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple18",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr,
          tuple._14.dumpRepr,
          tuple._15.dumpRepr,
          tuple._16.dumpRepr,
          tuple._17.dumpRepr,
          tuple._18.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple19Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper,
      N: Dumper,
      O: Dumper,
      P: Dumper,
      Q: Dumper,
      R: Dumper,
      S: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple19",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr,
          tuple._14.dumpRepr,
          tuple._15.dumpRepr,
          tuple._16.dumpRepr,
          tuple._17.dumpRepr,
          tuple._18.dumpRepr,
          tuple._19.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple20Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper,
      N: Dumper,
      O: Dumper,
      P: Dumper,
      Q: Dumper,
      R: Dumper,
      S: Dumper,
      T: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple20",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr,
          tuple._14.dumpRepr,
          tuple._15.dumpRepr,
          tuple._16.dumpRepr,
          tuple._17.dumpRepr,
          tuple._18.dumpRepr,
          tuple._19.dumpRepr,
          tuple._20.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple21Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper,
      N: Dumper,
      O: Dumper,
      P: Dumper,
      Q: Dumper,
      R: Dumper,
      S: Dumper,
      T: Dumper,
      U: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple21",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr,
          tuple._14.dumpRepr,
          tuple._15.dumpRepr,
          tuple._16.dumpRepr,
          tuple._17.dumpRepr,
          tuple._18.dumpRepr,
          tuple._19.dumpRepr,
          tuple._20.dumpRepr,
          tuple._21.dumpRepr
        )
      )

  /**
   * Derives an `Dumper` for a product type given an `Dumper` for each element of the product type.
   */
  implicit def Tuple22Dumper[
      A: Dumper,
      B: Dumper,
      C: Dumper,
      D: Dumper,
      E: Dumper,
      F: Dumper,
      G: Dumper,
      H: Dumper,
      I: Dumper,
      J: Dumper,
      K: Dumper,
      L: Dumper,
      M: Dumper,
      N: Dumper,
      O: Dumper,
      P: Dumper,
      Q: Dumper,
      R: Dumper,
      S: Dumper,
      T: Dumper,
      U: Dumper,
      V: Dumper
  ]: Dumper[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple22",
        List(
          tuple._1.dumpRepr,
          tuple._2.dumpRepr,
          tuple._3.dumpRepr,
          tuple._4.dumpRepr,
          tuple._5.dumpRepr,
          tuple._6.dumpRepr,
          tuple._7.dumpRepr,
          tuple._8.dumpRepr,
          tuple._9.dumpRepr,
          tuple._10.dumpRepr,
          tuple._11.dumpRepr,
          tuple._12.dumpRepr,
          tuple._13.dumpRepr,
          tuple._14.dumpRepr,
          tuple._15.dumpRepr,
          tuple._16.dumpRepr,
          tuple._17.dumpRepr,
          tuple._18.dumpRepr,
          tuple._19.dumpRepr,
          tuple._20.dumpRepr,
          tuple._21.dumpRepr,
          tuple._22.dumpRepr
        )
      )

  /**
   * The `Dumper`instance for `Nothing`. Note that since there are no values of type `Nothing` this `Dumper` instance
   * can never be called.
   */
  @nowarn
  implicit val NothingDumper: Dumper[Nothing] =
    n => n

  /**
   * The `Dumper` instance for `Unit`.
   */
  implicit val UnitDumper: Dumper[Unit] =
    _ => Repr.Object("scala" :: Nil, "()")

  /**
   * Derives a `Dumper[Vector[A]]` given a `Dumper[A]`.
   */
  implicit def VectorDumper[A: Dumper]: Dumper[Vector[A]] =
    vector => Repr.VConstructor(List("scala"), "Vector", vector.map(_.dumpRepr).toList)

  private val nanosToPrettyUnit: Long => (Long, TimeUnit) = {
    val ns_per_us  = 1000L
    val ns_per_ms  = ns_per_us * 1000
    val ns_per_s   = ns_per_ms * 1000
    val ns_per_min = ns_per_s * 60
    val ns_per_h   = ns_per_min * 60
    val ns_per_d   = ns_per_h * 24

    (nanos: Long) =>
      import java.util.concurrent.TimeUnit._
      if (nanos % ns_per_d == 0) (nanos / ns_per_d, DAYS)
      else if (nanos % ns_per_h == 0) (nanos / ns_per_h, HOURS)
      else if (nanos % ns_per_min == 0) (nanos / ns_per_min, MINUTES)
      else if (nanos % ns_per_s == 0) (nanos / ns_per_s, SECONDS)
      else if (nanos % ns_per_ms == 0) (nanos / ns_per_ms, MILLISECONDS)
      else if (nanos % ns_per_us == 0) (nanos / ns_per_us, MICROSECONDS)
      else (nanos, NANOSECONDS)
  }
}

trait DumperSyntax {
  implicit class DumperOps[A](self: A) {
    def dumpRepr(implicit dumper: Dumper[A]): Dumper.Repr = dumper.dumpRepr(self)
    def dump(implicit dumper: Dumper[A]): String          = dumper.dump(self)
  }

  implicit final class DumperInterpolator(_sc: StringContext) {
    def dump(args: Dumper.Repr*): String = _sc.s(args.map(_.dump): _*)
  }
}
