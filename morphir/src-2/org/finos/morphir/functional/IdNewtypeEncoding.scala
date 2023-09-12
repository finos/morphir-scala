package org.finos.morphir.functional

private[functional] abstract class IdNewtypeEncoding {
  trait IdTag extends Any

  type Type[+A] = A with IdTag

  def apply[A](value: A): Type[A] = value.asInstanceOf[Type[A]]
  implicit final class Ops[A](val self: Type[A]) {
    @inline final def value: A = self.asInstanceOf[A]
  }

  def unapply[A](instance: Type[A]): Option[A] = Some(instance.asInstanceOf[A])
}
