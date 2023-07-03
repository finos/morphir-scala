package org.finos.morphir.datamodel

trait DataDecoder[A] {
  import DataDecoder.DecodingError
  final def apply(value: Data): Either[DecodingError, A] = decode(value)
  def decode(value: Data): Either[DecodingError, A]
}

object DataDecoder {

  implicit val boolean: DataDecoder[Boolean] = (value: Data) =>
    value match {
      case Data.Boolean(value) => Right(value)
      case _                   => Left(DecodingError.InvalidConversion("Boolean"))
    }

  implicit val byte: DataDecoder[Byte] = (value: Data) =>
    value match {
      case Data.Byte(value) => Right(value)
      case _                => Left(DecodingError.InvalidConversion("Byte"))
    }

  implicit val decimal: DataDecoder[BigDecimal] = (value: Data) =>
    value match {
      case Data.Decimal(value) => Right(value)
      case _                   => Left(DecodingError.InvalidConversion("BigDecimal"))
    }

  implicit val string: DataDecoder[String] = (value: Data) =>
    value match {
      case Data.String(value) => Right(value)
      case _                  => Left(DecodingError.InvalidConversion("String"))
    }

  sealed abstract class DecodingError(msg: Option[String] = None, cause: Option[Throwable])
      extends Exception(msg.orNull, cause.orNull) with Serializable with Product
  object DecodingError {
    final case class InvalidConversion(typeName: String)
        extends DecodingError(Option(s"Conversion to type $typeName is invalid/not possible."), None)
  }
}
