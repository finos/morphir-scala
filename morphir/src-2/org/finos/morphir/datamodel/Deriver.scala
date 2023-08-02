package org.finos.morphir.datamodel

// Stub so Scala 2 can compile org.finos.morphir.datamodel package since it requires the Deriver trait
trait Deriver[A] {
  final def apply(value: A) = derive(value)
  def derive(value: A): Data
  def concept: Concept
}

object Deriver {
  def toData[A](value: A)(implicit deriver: Deriver[A]): Data =
    deriver.derive(value)

  implicit val booleanDeriver: Deriver[Boolean] = new Deriver[Boolean] {
    def derive(value: Boolean) = Data.Boolean(value)
    def concept                = Concept.Boolean
  }

  implicit val byteDeriver: Deriver[Byte] = new Deriver[Byte] {
    def derive(value: Byte) = Data.Byte(value)
    def concept             = Concept.Byte
  }

  implicit val bigDecimalDeriver: Deriver[BigDecimal] = new Deriver[BigDecimal] {
    def derive(value: BigDecimal) = Data.Decimal(value)
    def concept                   = Concept.Decimal
  }

  implicit val bigIntDeriver: Deriver[BigInt] = new Deriver[BigInt] {
    def derive(value: BigInt) = Data.Integer(value)
    def concept               = Concept.Integer
  }

  implicit val shortDeriver: Deriver[Short] = new Deriver[Short] {
    def derive(value: Short) = Data.Int16(value)
    def concept              = Concept.Int16
  }

  implicit val intDeriver: Deriver[Int] = new Deriver[Int] {
    def derive(value: Int) = Data.Int32(value)
    def concept            = Concept.Int32
  }

  implicit val stringDeriver: Deriver[String] = new Deriver[String] {
    def derive(value: String) = Data.String(value)
    def concept               = Concept.String
  }

  implicit val localDateDeriver: Deriver[java.time.LocalDate] = new Deriver[java.time.LocalDate] {
    def derive(value: java.time.LocalDate) = Data.LocalDate(value)
    def concept                            = Concept.LocalDate
  }

  implicit val monthDeriver: Deriver[java.time.Month] = new Deriver[java.time.Month] {
    def derive(value: java.time.Month) = Data.Month(value)
    def concept                        = Concept.Month
  }

  implicit val localTimeDeriver: Deriver[java.time.LocalTime] = new Deriver[java.time.LocalTime] {
    def derive(value: java.time.LocalTime) = Data.LocalTime(value)
    def concept                            = Concept.LocalTime
  }

  implicit val charDeriver: Deriver[Char] = new Deriver[Char] {
    def derive(value: Char) = Data.Char(value)
    def concept             = Concept.Char
  }

  implicit val unitDeriver: Deriver[Unit] = new Deriver[Unit] {
    def derive(value: Unit) = Data.Unit
    def concept             = Concept.Unit
  }

  implicit def optionDeriver[T](implicit elementDeriver: Deriver[T]): Deriver[Option[T]] =
    new Deriver[Option[T]] {
      def derive(value: Option[T]) =
        value match {
          case Some(value) => Data.Optional.Some(elementDeriver.derive(value), elementDeriver.concept)
          case None        => Data.Optional.None(elementDeriver.concept)
        }
      def concept = Concept.Optional(elementDeriver.concept)
    }

  implicit def optionSomeDeriver[T](implicit elementDeriver: Deriver[T]): Deriver[Some[T]] =
    new Deriver[Some[T]] {
      def derive(value: Some[T]) = Data.Optional.Some(elementDeriver.derive(value.value), elementDeriver.concept)
      def concept                = Concept.Optional(elementDeriver.concept)
    }

  implicit val optionNoneDeriver: Deriver[scala.None.type] = new Deriver[scala.None.type] {
    def derive(value: scala.None.type) = Data.Optional.None(Concept.Nothing)
    def concept                        = Concept.Optional(Concept.Nothing)
  }

  implicit def listDeriver[T](implicit elementDeriver: Deriver[T]): Deriver[List[T]] =
    new Deriver[List[T]] {
      def derive(value: List[T]) = Data.List(value.map(elementDeriver.derive), elementDeriver.concept)
      def concept                = Concept.List(elementDeriver.concept)
    }

}
