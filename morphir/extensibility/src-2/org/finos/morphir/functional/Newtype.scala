package org.finos.morphir.functional
import org.finos.morphir.naming._
abstract class Newtype[A] extends HasId {
  // This encoding originally comes from this library:
  // https://github.com/alexknvl/newtypes#what-does-it-do
  type Base
  trait _Tag extends Any
  type Type <: Base with _Tag
  @inline final def apply(value: A): Type     = value.asInstanceOf
  @inline final def unwrap(instance: Type): A = instance.asInstanceOf[A]
  @inline final def value(instance: Type): A  = instance.asInstanceOf[A]

  def unapply(instance: Type): Option[A] = Some(instance.asInstanceOf[A])

  implicit final class Ops(val self: Type) {
    @inline final def value: A = Newtype.this.value(self)
  }
}

object Newtype {
  private[functional] trait Make[A, B] extends Bijection[A, B]
}
