package org.finos.morphir.functional

private[functional] abstract class IdNewtypeEncoding {
  opaque type Type[+A] <: A = A
  def apply[A](value: A): Type[A]              = value
  extension [A](me: Type[A]) def value: A      = me
  def unapply[A](instance: Type[A]): Option[A] = Some(instance)

}
