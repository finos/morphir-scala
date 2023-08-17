package org.finos.morphir.datamodel

trait CustomDeriver[T] extends Deriver[T] {
  def derive(value: T): Data
  def concept: Concept

  def contramap[R](f: R => T): CustomDeriver[R] =
    CustomDeriver.ofType(b => this.derive(f(b)), this.concept)

  def contramapWithConcept[R](concept0: Concept)(f: R => T): CustomDeriver[R] =
    CustomDeriver.ofType(b => this.derive(f(b)), concept0)
}

object CustomDeriver {
  def ofType[A](f: A => Data, concept0: Concept): CustomDeriver[A] =
    new CustomDeriver[A] {
      override def derive(a: A): Data = f(a)
      override def concept: Concept   = concept0
    }
}
