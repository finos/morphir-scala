package org.finos.morphir.codec

import zio.Cause

import scala.util.control.NoStackTrace

sealed trait DecodeError extends Exception with NoStackTrace { self =>
  def message:String
  override def getMessage: String = message

  def and(that: DecodeError): DecodeError = DecodeError.And(self, that)

  def or(that: DecodeError): DecodeError = DecodeError.Or(self, that)
}

object DecodeError {
  final case class And(left: DecodeError, right: DecodeError) extends DecodeError {
    override def message: String = s"${left.message} and ${right.message}"
  }

  final case class Or(left: DecodeError, right: DecodeError) extends DecodeError {
    override def message: String = s"${left.message} or ${right.message}"
  }

  final case class ReadError(cause: Cause[Any], message: String) extends DecodeError
}
