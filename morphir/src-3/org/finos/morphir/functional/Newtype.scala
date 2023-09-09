package org.finos.morphir.functional
import org.finos.morphir.naming._
abstract class Newtype[A] extends HasId { self =>
  opaque type Type = A
  def apply(value: A): Type              = value
  extension (me: Type) def value: A      = me
  def unapply(instance: Type): Option[A] = Some(instance)

  implicit val asBijection: Bijection[A, Type] = new Newtype.Make[A, Type] {
    def to(a: A): Type = self.apply(a)

    def from(t: Type): A = value(t)
  }
}

object Newtype {
  private[functional] trait Make[A, B] extends Bijection[A, B]
}
