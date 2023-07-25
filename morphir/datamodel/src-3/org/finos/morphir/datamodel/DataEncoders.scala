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

given booleanDataEncoder: SpecificDataEncoder[Boolean] with
  def encode(value: Boolean) = Data.Boolean(value)
  def concept                = Concept.Boolean

given byteDataEncoder: SpecificDataEncoder[Byte] with
  def encode(value: Byte) = Data.Byte(value)
  def concept             = Concept.Byte

given bigDecimalDataEncoder: SpecificDataEncoder[BigDecimal] with
  def encode(value: BigDecimal) = Data.Decimal(value)
  def concept                   = Concept.Decimal

given bigIntDataEncoder: SpecificDataEncoder[BigInt] with
  def encode(value: BigInt) = Data.Integer(value)
  def concept               = Concept.Integer

given shortDataEncoder: SpecificDataEncoder[Short] with
  def encode(value: Short) = Data.Int16(value)
  def concept              = Concept.Int16

given intDataEncoder: SpecificDataEncoder[Int] with
  def encode(value: Int) = Data.Int32(value)
  def concept            = Concept.Int32

given stringDataEncoder: SpecificDataEncoder[String] with
  def encode(value: String) = Data.String(value)
  def concept               = Concept.String

given localDateDataEncoder: SpecificDataEncoder[LocalDate] with
  def encode(value: LocalDate) = Data.LocalDate(value)
  def concept                  = Concept.LocalDate

given monthDataEncoder: SpecificDataEncoder[Month] with
  def encode(value: Month) = Data.Month(value)
  def concept              = Concept.Month

given localTimeDataEncoder: SpecificDataEncoder[LocalTime] with
  def encode(value: LocalTime) = Data.LocalTime(value)
  def concept                  = Concept.LocalTime

given charDataEncoder: SpecificDataEncoder[Char] with
  def encode(value: Char) = Data.Char(value)
  def concept             = Concept.Char

given unitDataEncoder: SpecificDataEncoder[Unit] with
  def encode(value: Unit) = Data.Unit
  def concept             = Concept.Unit

given optionDataEncoder[T](using elementDeriver: DataEncoder[T]): SpecificDataEncoder[Option[T]] with
  def encode(value: Option[T]) =
    value match
      case Some(value) => Data.Optional.Some(elementDeriver.encode(value), elementDeriver.concept)
      case None        => Data.Optional.None(elementDeriver.concept)
  def concept = Concept.Optional(elementDeriver.concept)

given optionSomeDataEncoder[T](using elementDeriver: DataEncoder[T]): SpecificDataEncoder[Some[T]] with
  def encode(value: Some[T]) = Data.Optional.Some(elementDeriver.encode(value.value), elementDeriver.concept)
  def concept                = Concept.Optional(elementDeriver.concept)

given optionNoneDataEncoder: SpecificDataEncoder[scala.None.type] with
  def encode(value: scala.None.type) = Data.Optional.None(Concept.Nothing)
  def concept                        = Concept.Optional(Concept.Nothing)

given listDataEncoder[T](using elementDeriver: DataEncoder[T]): SpecificDataEncoder[List[T]] with {
  def encode(value: scala.List[T]) =
    def toData(value: T) = elementDeriver.encode(value)
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

given linkedMapDataEncoder[K, V](using
    keyDeriver: DataEncoder[K],
    valueDeriver: DataEncoder[V]
): SpecificDataEncoder[LinkedHashMap[K, V]] with {
  def encode(value: LinkedHashMap[K, V]) =
    def toData(value: (K, V)) = (keyDeriver.encode(value._1), valueDeriver.encode(value._2))
    Data.Map.copyFrom(value.map(toData(_)), Concept.Map(keyDeriver.concept, valueDeriver.concept))

  def concept: Concept.Map = Concept.Map(keyDeriver.concept, valueDeriver.concept)
}

given listMapDataEncoder[K, V](using
    keyDeriver: DataEncoder[K],
    valueDeriver: DataEncoder[V]
): SpecificDataEncoder[ListMap[K, V]] with
  def encode(value: ListMap[K, V]): Data = linkedMapDataEncoder[K, V].encode(LinkedHashMap.from(value))
  def concept: Concept                   = linkedMapDataEncoder[K, V].concept

given mapDataEncoder[K, V](using
    keyDeriver: DataEncoder[K],
    valueDeriver: DataEncoder[V]
): SpecificDataEncoder[Map[K, V]] with
  def encode(value: Map[K, V]): Data = linkedMapDataEncoder[K, V].encode(LinkedHashMap.from(value))
  def concept: Concept               = linkedMapDataEncoder[K, V].concept

implicit inline def autoProductDeriver[T <: Product]: GenericProductDataEncoder[T] =
  GenericProductDataEncoder.gen[T]

implicit inline def autoSumDeriver[T]: GenericSumDataEncoder[T] =
  GenericSumDataEncoder.gen[T]
