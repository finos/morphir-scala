package org.finos.morphir.datamodel

// Stub so Scala 2 can compile org.finos.morphir.datamodel package since it requires the Deriver trait
trait DataEncoder[T] {
  final def apply(value: T): Data = encode(value)
  def encode(value: T): Data
  def concept: Concept
}
