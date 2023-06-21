package org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline, error, codeOf}
import scala.BigInt
import scala.BigDecimal
import java.time.{LocalDate, LocalTime, Month}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Concept

object Derivers {
  given SpecificDeriver[Boolean] with
    def derive(value: Boolean) = Data.Boolean(value)
    def concept                = Concept.Boolean

  given SpecificDeriver[Byte] with
    def derive(value: Byte) = Data.Byte(value)
    def concept             = Concept.Byte

  given SpecificDeriver[BigDecimal] with
    def derive(value: BigDecimal) = Data.Decimal(value)
    def concept                   = Concept.Decimal

  given SpecificDeriver[BigInt] with
    def derive(value: BigInt) = Data.Integer(value)
    def concept               = Concept.Integer

  given SpecificDeriver[Short] with
    def derive(value: Short) = Data.Int16(value)
    def concept              = Concept.Int16

  given SpecificDeriver[Int] with
    def derive(value: Int) = Data.Int32(value)
    def concept            = Concept.Int32

  given SpecificDeriver[String] with
    def derive(value: String) = Data.String(value)
    def concept               = Concept.String

  given SpecificDeriver[LocalDate] with
    def derive(value: LocalDate) = Data.LocalDate(value)
    def concept                  = Concept.LocalDate

  given SpecificDeriver[Month] with
    def derive(value: Month) = Data.Month(value.getValue)
    def concept              = Concept.Month

  given SpecificDeriver[LocalTime] with
    def derive(value: LocalTime) = Data.LocalTime(value)
    def concept                  = Concept.LocalTime

  given SpecificDeriver[Char] with
    def derive(value: Char) = Data.Char(value)
    def concept             = Concept.Char

  import Data.{List => DataList}

  given listDeriver[T](using elementDeriver: Deriver[T]): SpecificDeriver[List[T]] with
    def derive(value: scala.List[T]) =
      def toData(value: T) = elementDeriver.derive(value)
      value match {
        // If there are actual elements available in the list, take the schema from them
        case head :: tail => Data.List(toData(head), (tail.map(toData(_))): _*)
        // Otherwise take the schema from the property
        case Nil => Data.List.empty(Concept.List(elementDeriver.concept))
      }
    def concept: Concept.List = Concept.List(elementDeriver.concept)

  implicit inline def autoProductDeriver[T <: Product]: GenericProductDeriver[T] =
    GenericProductDeriver.gen[T]
}
