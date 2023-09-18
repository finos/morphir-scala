package org.finos.morphir.datamodel

import scala.BigInt
import scala.BigDecimal
import java.time.{LocalDate, LocalTime, Month}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Concept

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

trait DeriverInstances {

  implicit val booleanDeriver: CustomDeriver[Boolean] = new CustomDeriver[Boolean] {
    def derive(value: Boolean):Data = Data.Boolean(value)
    def concept:Concept                = Concept.Boolean
  }

  implicit val byteDeriver: CustomDeriver[Byte] = new CustomDeriver[Byte] {
    def derive(value: Byte):Data = Data.Byte(value)
    def concept:Concept             = Concept.Byte
  }

  implicit val doubleDeriver: CustomDeriver[Double] = new CustomDeriver[Double] {
    def derive(value: Double):Data = Data.Float(value)
    def concept:Concept               = Concept.Float
  }

  implicit val floatDeriver: CustomDeriver[Float] = new CustomDeriver[Float] {
    def derive(value: Float):Data = Data.Float(value.toDouble)
    def concept: Concept              = Concept.Float
  }

  implicit val bigDecimalDeriver: CustomDeriver[BigDecimal] = new CustomDeriver[BigDecimal] {
    def derive(value: BigDecimal):Data = Data.Decimal(value)
    def concept:Concept                   = Concept.Decimal
  }

  implicit val bigIntDeriver: CustomDeriver[BigInt] = new CustomDeriver[BigInt] {
    def derive(value: BigInt):Data = Data.Integer(value)
    def concept:Concept               = Concept.Integer
  }

  implicit val shortDeriver: CustomDeriver[Short] = new CustomDeriver[Short] {
    def derive(value: Short):Data = Data.Int16(value)
    def concept:Cncept              = Concept.Int16
  }

  implicit val intDeriver: CustomDeriver[Int] = new CustomDeriver[Int] {
    def derive(value: Int):Data = Data.Int32(value)
    def concept:Concept            = Concept.Int32
  }

  implicit val longDeriver: CustomDeriver[Long] = new CustomDeriver[Long] {
    def derive(value: Long):Data = Data.Int64(value)
    def concept             = Concept.Int64
  }

  implicit val stringDeriver: CustomDeriver[String] = new CustomDeriver[String] {
    def derive(value: String):Data = Data.String(value)
    def concept               = Concept.String
  }

  implicit val localDateDeriver: CustomDeriver[LocalDate] = new CustomDeriver[LocalDate] {
    def derive(value: LocalDate):Data = Data.LocalDate(value)
    def concept                  = Concept.LocalDate
  }

  implicit val monthDeriver: CustomDeriver[Month] = new CustomDeriver[Month] {
    def derive(value: Month):Data = Data.Month(value)
    def concept              = Concept.Month
  }

  implicit val localTimeDeriver: CustomDeriver[LocalTime] = new CustomDeriver[LocalTime] {
    def derive(value: LocalTime):Data = Data.LocalTime(value)
    def concept                  = Concept.LocalTime
  }

  implicit val charDeriver: CustomDeriver[Char] = new CustomDeriver[Char] {
    def derive(value: Char):Data = Data.Char(value)
    def concept             = Concept.Char
  }

  implicit val unitDeriver: CustomDeriver[Unit] = new CustomDeriver[Unit] {
    def derive(value: Unit) = Data.Unit
    def concept             = Concept.Unit
  }

  implicit def eitherDeriver[L, R](implicit
      leftDeriver: Deriver[L],
      rightDeriver: Deriver[R]
  ): CustomDeriver[Either[L, R]] =
    new CustomDeriver[Either[L, R]] {
      def derive(value: Either[L, R]) =
        value match {
          case Right(value) => Data.Result.Ok(rightDeriver.derive(value), this.concept)
          case Left(value)  => Data.Result.Err(leftDeriver.derive(value), this.concept)
        }
      def concept: Concept.Result = Concept.Result(leftDeriver.concept, rightDeriver.concept)
    }

  implicit def rightDeriver[L, R](implicit
      leftDeriver: Deriver[L],
      rightDeriver: Deriver[R]
  ): CustomDeriver[Right[L, R]] = eitherDeriver[L, R](leftDeriver, rightDeriver).contramap(v => v)

  implicit def leftDeriver[L, R](implicit
      leftDeriver: Deriver[L],
      rightDeriver: Deriver[R]
  ): CustomDeriver[Left[L, R]] = eitherDeriver[L, R](leftDeriver, rightDeriver).contramap(v => v)

  implicit def leftDeriver[L](implicit leftDeriver: Deriver[L]): CustomDeriver[Left[L, Nothing]] =
    new CustomDeriver[Left[L, Nothing]] {
      def derive(value: Left[L, Nothing]) = Data.Result.Err(leftDeriver.derive(value.value), this.concept)
      def concept: Concept.Result         = Concept.Result(leftDeriver.concept, Concept.Nothing)
    }

  implicit def optionDeriver[T](implicit elementDeriver: Deriver[T]): CustomDeriver[Option[T]] =
    new CustomDeriver[Option[T]] {
      def derive(value: Option[T]) =
        value match {
          case Some(value) => Data.Optional.Some(elementDeriver.derive(value), elementDeriver.concept)
          case None        => Data.Optional.None(elementDeriver.concept)
        }
      def concept = Concept.Optional(elementDeriver.concept)
    }

  implicit def optionSomeDeriver[T](implicit elementDeriver: Deriver[T]): CustomDeriver[Some[T]] =
    new CustomDeriver[Some[T]] {
      def derive(value: Some[T]) = Data.Optional.Some(elementDeriver.derive(value.value), elementDeriver.concept)
      def concept                = Concept.Optional(elementDeriver.concept)
    }

  implicit val optionNoneDeriver: CustomDeriver[scala.None.type] = new CustomDeriver[scala.None.type] {
    def derive(value: scala.None.type) = Data.Optional.None(Concept.Nothing)
    def concept                        = Concept.Optional(Concept.Nothing)
  }

  implicit def listDeriver[T](implicit elementDeriver: Deriver[T]): CustomDeriver[List[T]] =
    new CustomDeriver[List[T]] {
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
  ): CustomDeriver[mutable.LinkedHashMap[K, V]] =
    new CustomDeriver[LinkedHashMap[K, V]] {
      def derive(value: LinkedHashMap[K, V]) = {
        def toData(value: (K, V)) = (keyDeriver.derive(value._1), valueDeriver.derive(value._2))
        Data.Map.copyFrom(value.map(toData(_)), Concept.Map(keyDeriver.concept, valueDeriver.concept))
      }

      def concept: Concept.Map = Concept.Map(keyDeriver.concept, valueDeriver.concept)
    }

  implicit def listMapDeriver[K, V](implicit
      keyDeriver: Deriver[K],
      valueDeriver: Deriver[V]
  ): CustomDeriver[ListMap[K, V]] =
    new CustomDeriver[ListMap[K, V]] {
      def derive(value: ListMap[K, V]): Data = linkedMapDeriver[K, V].derive(LinkedHashMap.from(value))
      def concept: Concept                   = linkedMapDeriver[K, V].concept
    }

  implicit def mapDeriver[K, V](implicit keyDeriver: Deriver[K], valueDeriver: Deriver[V]): CustomDeriver[Map[K, V]] =
    new CustomDeriver[Map[K, V]] {
      def derive(value: Map[K, V]): Data = linkedMapDeriver[K, V].derive(LinkedHashMap.from(value))
      def concept: Concept               = linkedMapDeriver[K, V].concept
    }

}
