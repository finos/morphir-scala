package org.finos.morphir.runtime

import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.RTValue.*

trait Coercer[T] {
  def coerce(result: RTValue): T
}

object Coercer {
  implicit val selfCoercer: Coercer[RTValue] = new Coercer[RTValue] {
    def coerce(result: RTValue): RTValue = result
  }

  implicit val intCoercer: Coercer[RTValue.Primitive.Int] = new Coercer[RTValue.Primitive.Int] {
    def coerce(result: RTValue): RTValue.Primitive.Int = RTValue.coerceInt(result)
  }

  implicit val floatCoercer: Coercer[RTValue.Primitive.Float] = new Coercer[RTValue.Primitive.Float] {
    def coerce(result: RTValue): RTValue.Primitive.Float = RTValue.coerceFloat(result)
  }

  implicit val bigDecimalCoercer: Coercer[RTValue.Primitive.BigDecimal] =
    new Coercer[RTValue.Primitive.BigDecimal] {
      def coerce(result: RTValue): RTValue.Primitive.BigDecimal = RTValue.coerceDecimal(result)
    }

  implicit val stringCoercer: Coercer[RTValue.Primitive.String] = new Coercer[RTValue.Primitive.String] {
    def coerce(result: RTValue): RTValue.Primitive.String = RTValue.coerceString(result)
  }

  implicit val charCoercer: Coercer[RTValue.Primitive.Char] = new Coercer[RTValue.Primitive.Char] {
    def coerce(result: RTValue): RTValue.Primitive.Char = RTValue.coerceChar(result)
  }

  implicit val primitiveCoercer: Coercer[RTValue.Primitive[_]] = new Coercer[RTValue.Primitive[_]] {
    def coerce(result: RTValue): RTValue.Primitive[_] = RTValue.coercePrimitive(result)
  }

  implicit val booleanCoercer: Coercer[RTValue.Primitive.Boolean] = new Coercer[RTValue.Primitive.Boolean] {
    def coerce(result: RTValue): RTValue.Primitive.Boolean = RTValue.coerceBoolean(result)
  }

  implicit def numericCoercer[T]: Coercer[RTValue.Primitive.Numeric[T]] =
    new Coercer[RTValue.Primitive.Numeric[T]] {
      def coerce(result: RTValue): RTValue.Primitive.Numeric[T] =
        RTValue.coerceNumeric(result).asInstanceOf[RTValue.Primitive.Numeric[T]]
    }
  implicit val comparableCoercer: Coercer[RTValue.Comparable] =
    new Coercer[RTValue.Comparable] {
      def coerce(result: RTValue): RTValue.Comparable = RTValue.coerceComparable(result)
    }

  implicit val fallbackNumericCoercer: Coercer[RTValue.Primitive.Numeric[_]] =
    new Coercer[RTValue.Primitive.Numeric[_]] {
      def coerce(result: RTValue): RTValue.Primitive.Numeric[_] =
        RTValue.coerceNumeric(result).asInstanceOf[RTValue.Primitive.Numeric[_]]
    }

  implicit val listCoercer: Coercer[RTValue.List] = new Coercer[RTValue.List] {
    def coerce(result: RTValue): RTValue.List = RTValue.coerceList(result)
  }
  implicit val constructorResultCoercer: Coercer[RTValue.ConstructorResult] = new Coercer[RTValue.ConstructorResult] {
    def coerce(result: RTValue): RTValue.ConstructorResult = RTValue.coerceConstructorResult(result)
  }

  implicit val functionCoercer: Coercer[RTValue.Function] = new Coercer[RTValue.Function] {
    def coerce(result: RTValue): RTValue.Function = RTValue.coerceFunction(result)
  }

  implicit val setCoercer: Coercer[RTValue.Set] = new Coercer[RTValue.Set] {
    def coerce(result: RTValue): RTValue.Set = RTValue.coerceSet(result)
  }

  implicit val mapCoercer: Coercer[RTValue.Map] = new Coercer[RTValue.Map] {
    def coerce(result: RTValue): RTValue.Map = RTValue.coerceMap(result)
  }

  implicit val localDateCoercer: Coercer[RTValue.LocalDate] = new Coercer[RTValue.LocalDate] {
    def coerce(result: RTValue): RTValue.LocalDate = RTValue.coerceLocalDate(result)
  }

  implicit val localTimeCoercer: Coercer[RTValue.LocalTime] = new Coercer[RTValue.LocalTime] {
    def coerce(result: RTValue): RTValue.LocalTime = RTValue.coerceLocalTime(result)
  }

  implicit val tupleCoercer: Coercer[RTValue.Tuple] = new Coercer[RTValue.Tuple] {
    def coerce(result: RTValue): RTValue.Tuple = RTValue.coerceTuple(result)
  }

  implicit val aggregationCoercer: Coercer[RTValue.Aggregation] = new Coercer[RTValue.Aggregation] {
    def coerce(result: RTValue): RTValue.Aggregation = RTValue.coerceAggregation(result)
  }

  implicit val keyCoercer: Coercer[RTValue.Key] = new Coercer[RTValue.Key] {
    def coerce(result: RTValue): RTValue.Key = RTValue.coerceKey(result)
  }
}
