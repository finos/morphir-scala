package org.finos.morphir.datamodel

trait SpecificDataEncoder[T] extends DataEncoder[T] {
  def encode(value: T): Data
  def concept: Concept
}
