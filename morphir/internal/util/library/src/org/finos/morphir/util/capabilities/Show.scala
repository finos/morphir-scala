package org.finos.morphir.util.capabilities

trait Show[A] {
  def show(value: A): String
}

object Show extends ShowInstancesPriority0 {
  def apply[A](implicit ev: Show[A]): Show[A] = ev

  def fromToString[A]: Show[A] = _.toString()
}

private[util] trait ShowInstancesPriority0 {

  implicit def defaultInstance[A]: Show[A] = _.toString()
}
