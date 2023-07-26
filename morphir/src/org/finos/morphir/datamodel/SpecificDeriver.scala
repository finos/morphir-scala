package org.finos.morphir.datamodel

trait SpecificDeriver[T] extends Deriver[T] {
  def derive(value: T): Data
  def concept: Concept
}
