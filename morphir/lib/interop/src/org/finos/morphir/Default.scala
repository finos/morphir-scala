package org.finos.morphir

class Default[+A](val default: A)

object Default extends DefaultLowerPriorityInstances {
  implicit object DefaultUnit extends Default[Unit](())

  def value[A](implicit value: Default[A]): A = value.default
}

trait DefaultLowerPriorityInstances {
  implicit def defaultNull[A <: AnyRef]: Default[A] = new Default(null.asInstanceOf[A])
}
