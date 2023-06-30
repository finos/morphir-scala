package org.finos.morphir.datamodel

trait FromData[A] {
  import FromData.Error
  def apply(value: Data): Either[Error, A]
}

object FromData {
  type Error = String
}
