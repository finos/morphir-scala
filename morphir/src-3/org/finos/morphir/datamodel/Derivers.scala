package org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}
import scala.BigInt
import scala.BigDecimal
import java.time.{LocalDate, LocalTime, Month}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Concept

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap

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
    def derive(value: Month) = Data.Month(value)
    def concept              = Concept.Month

  given SpecificDeriver[LocalTime] with
    def derive(value: LocalTime) = Data.LocalTime(value)
    def concept                  = Concept.LocalTime

  given SpecificDeriver[Char] with
    def derive(value: Char) = Data.Char(value)
    def concept             = Concept.Char

  given SpecificDeriver[Unit] with
    def derive(value: Unit) = Data.Unit
    def concept             = Concept.Unit

  given optionDeriver[T](using elementDeriver: Deriver[T]): SpecificDeriver[Option[T]] with
    def derive(value: Option[T]) =
      value match
        case Some(value) => Data.Optional.Some(elementDeriver.derive(value), elementDeriver.concept)
        case None        => Data.Optional.None(elementDeriver.concept)
    def concept = Concept.Optional(elementDeriver.concept)

  given optionSomeDeriver[T](using elementDeriver: Deriver[T]): SpecificDeriver[Some[T]] with
    def derive(value: Some[T]) = Data.Optional.Some(elementDeriver.derive(value.value), elementDeriver.concept)
    def concept                = Concept.Optional(elementDeriver.concept)

  given optionNoneDeriver: SpecificDeriver[scala.None.type] with
    def derive(value: scala.None.type) = Data.Optional.None(Concept.Nothing)
    def concept                        = Concept.Optional(Concept.Nothing)

  given listDeriver[T](using elementDeriver: Deriver[T]): SpecificDeriver[List[T]] with {
    def derive(value: scala.List[T]) =
      def toData(value: T) = elementDeriver.derive(value)
      // Take the schema from the elementDeriver instead of the list elements
      // because even if the elements themeselves have more specific schemas than the derver schema,
      // the deriver schema has a generalization of the elements whose type is the only valid
      // type for the whole list.
      Data.List(value.map(toData(_)), elementDeriver.concept)

    def concept: Concept.List = Concept.List(elementDeriver.concept)
  }

  /*
   * Since we want to have the option to use either ordered or unordered maps in the DDL,
   * derivers for ordered and non-ordered map variants have been provided. In particular
   * since the Scala ListMap is problematic in many ways (e.g. lookup time is O(n)) the
   * baseline implementation for the deriver uses scala's LinkedHashMap. Since
   * this datastructure is mutable, we make a copy of it during the derivation process
   * so that changes to it will not cause changes to the underlying Data object.
   */

  given linkedMapDeriver[K, V](using
      keyDeriver: Deriver[K],
      valueDeriver: Deriver[V]
  ): SpecificDeriver[LinkedHashMap[K, V]] with {
    def derive(value: LinkedHashMap[K, V]) =
      def toData(value: (K, V)) = (keyDeriver.derive(value._1), valueDeriver.derive(value._2))
      Data.Map.copyFrom(value.map(toData(_)), Concept.Map(keyDeriver.concept, valueDeriver.concept))

    def concept: Concept.Map = Concept.Map(keyDeriver.concept, valueDeriver.concept)
  }

  given listMapDeriver[K, V](using
      keyDeriver: Deriver[K],
      valueDeriver: Deriver[V]
  ): SpecificDeriver[ListMap[K, V]] with
    def derive(value: ListMap[K, V]): Data = linkedMapDeriver[K, V].derive(LinkedHashMap.from(value))
    def concept: Concept                   = linkedMapDeriver[K, V].concept

  given mapDeriver[K, V](using
      keyDeriver: Deriver[K],
      valueDeriver: Deriver[V]
  ): SpecificDeriver[Map[K, V]] with
    def derive(value: Map[K, V]): Data = linkedMapDeriver[K, V].derive(LinkedHashMap.from(value))
    def concept: Concept               = linkedMapDeriver[K, V].concept

  implicit inline def autoProductDeriver[T <: Product]: GenericProductDeriver[T] =
    GenericProductDeriver.gen[T]

  implicit inline def autoSumDeriver[T]: GenericSumDeriver[T] =
    GenericSumDeriver.gen[T]
}
