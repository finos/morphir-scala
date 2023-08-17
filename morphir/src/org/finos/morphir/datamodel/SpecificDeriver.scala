package org.finos.morphir.datamodel

trait SpecificDeriver[T] extends Deriver[T] {
  def derive(value: T): Data
  def concept: Concept

  def contramap[R](f: R => T): SpecificDeriver[R] =
    SpecificDeriver.ofType(b => this.derive(f(b)), this.concept)

  def contramapWithConcept[R](concept0: Concept)(f: R => T): SpecificDeriver[R] =
    SpecificDeriver.ofType(b => this.derive(f(b)), concept0)
}

object SpecificDeriver {
  def ofType[A](f: A => Data, concept0: Concept): SpecificDeriver[A] =
    new SpecificDeriver[A] {
      override def derive(a: A): Data = f(a)
      override def concept: Concept   = concept0
    }
}
