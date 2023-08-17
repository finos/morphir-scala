package org.finos.morphir

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
import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

package object datamodel {

  implicit val booleanDeriver: SpecificDeriver[Boolean] = new SpecificDeriver[Boolean] {
    def derive(value: Boolean) = Data.Boolean(value)
    def concept                = Concept.Boolean
  }

  implicit val byteDeriver: SpecificDeriver[Byte] = new SpecificDeriver[Byte] {
    def derive(value: Byte) = Data.Byte(value)
    def concept             = Concept.Byte
  }

  implicit val bigDecimalDeriver: SpecificDeriver[BigDecimal] = new SpecificDeriver[BigDecimal] {
    def derive(value: BigDecimal) = Data.Decimal(value)
    def concept                   = Concept.Decimal
  }

  implicit val bigIntDeriver: SpecificDeriver[BigInt] = new SpecificDeriver[BigInt] {
    def derive(value: BigInt) = Data.Integer(value)
    def concept               = Concept.Integer
  }

  implicit val shortDeriver: SpecificDeriver[Short] = new SpecificDeriver[Short] {
    def derive(value: Short) = Data.Int16(value)
    def concept              = Concept.Int16
  }

  implicit val intDeriver: SpecificDeriver[Int] = new SpecificDeriver[Int] {
    def derive(value: Int) = Data.Int32(value)
    def concept            = Concept.Int32
  }

  implicit val stringDeriver: SpecificDeriver[String] = new SpecificDeriver[String] {
    def derive(value: String) = Data.String(value)
    def concept               = Concept.String
  }

  implicit val localDateDeriver: SpecificDeriver[LocalDate] = new SpecificDeriver[LocalDate] {
    def derive(value: LocalDate) = Data.LocalDate(value)
    def concept                  = Concept.LocalDate
  }

  implicit val monthDeriver: SpecificDeriver[Month] = new SpecificDeriver[Month] {
    def derive(value: Month) = Data.Month(value)
    def concept              = Concept.Month
  }

  implicit val localTimeDeriver: SpecificDeriver[LocalTime] = new SpecificDeriver[LocalTime] {
    def derive(value: LocalTime) = Data.LocalTime(value)
    def concept                  = Concept.LocalTime
  }

  implicit val charDeriver: SpecificDeriver[Char] = new SpecificDeriver[Char] {
    def derive(value: Char) = Data.Char(value)
    def concept             = Concept.Char
  }

  implicit val unitDeriver: SpecificDeriver[Unit] = new SpecificDeriver[Unit] {
    def derive(value: Unit) = Data.Unit
    def concept             = Concept.Unit
  }

  implicit def optionDeriver[T](implicit elementDeriver: Deriver[T]): SpecificDeriver[Option[T]] =
    new SpecificDeriver[Option[T]] {
      def derive(value: Option[T]) =
        value match
          case Some(value) => Data.Optional.Some(elementDeriver.derive(value), elementDeriver.concept)
          case None        => Data.Optional.None(elementDeriver.concept)
      def concept = Concept.Optional(elementDeriver.concept)
    }

  implicit def optionSomeDeriver[T](implicit elementDeriver: Deriver[T]): SpecificDeriver[Some[T]] =
    new SpecificDeriver[Some[T]] {
      def derive(value: Some[T]) = Data.Optional.Some(elementDeriver.derive(value.value), elementDeriver.concept)
      def concept                = Concept.Optional(elementDeriver.concept)
    }

  implicit val optionNoneDeriver: SpecificDeriver[scala.None.type] = new SpecificDeriver[scala.None.type] {
    def derive(value: scala.None.type) = Data.Optional.None(Concept.Nothing)
    def concept                        = Concept.Optional(Concept.Nothing)
  }

  implicit def listDeriver[T](implicit elementDeriver: Deriver[T]): SpecificDeriver[List[T]] =
    new SpecificDeriver[List[T]] {
      def derive(value: scala.List[T]) = {
        def toData(value: T) = elementDeriver.derive(value)
        // Take the schema from the elementDeriver instead of the list elements
        // because even if the elements themeselves have more specific schemas than the derver schema,
        // the deriver schema has a generalization of the elements whose type is the only valid
        // type for the whole list.
        Data.List(value.map(toData(_)), elementDeriver.concept)
      }

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

  implicit def linkedMapDeriver[K, V](implicit
      keyDeriver: Deriver[K],
      valueDeriver: Deriver[V]
  ): SpecificDeriver[mutable.LinkedHashMap[K, V]] =
    new SpecificDeriver[LinkedHashMap[K, V]] {
      def derive(value: LinkedHashMap[K, V]) = {
        def toData(value: (K, V)) = (keyDeriver.derive(value._1), valueDeriver.derive(value._2))
        Data.Map.copyFrom(value.map(toData(_)), Concept.Map(keyDeriver.concept, valueDeriver.concept))
      }

      def concept: Concept.Map = Concept.Map(keyDeriver.concept, valueDeriver.concept)
    }

  implicit def listMapDeriver[K, V](implicit
      keyDeriver: Deriver[K],
      valueDeriver: Deriver[V]
  ): SpecificDeriver[ListMap[K, V]] =
    new SpecificDeriver[ListMap[K, V]] {
      def derive(value: ListMap[K, V]): Data = linkedMapDeriver[K, V].derive(LinkedHashMap.from(value))
      def concept: Concept                   = linkedMapDeriver[K, V].concept
    }

  implicit def mapDeriver[K, V](implicit keyDeriver: Deriver[K], valueDeriver: Deriver[V]): SpecificDeriver[Map[K, V]] =
    new SpecificDeriver[Map[K, V]] {
      def derive(value: Map[K, V]): Data = linkedMapDeriver[K, V].derive(LinkedHashMap.from(value))
      def concept: Concept               = linkedMapDeriver[K, V].concept
    }

}
