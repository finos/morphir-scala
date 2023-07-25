package org.finos.morphir.datamodel

// Stub so Scala 2 can compile org.finos.morphir.datamodel package since it requires the Deriver trait
trait Deriver[T] {
  def derive(value: T): Data
  def concept: Concept
}
