package org.finos.morphir.universe.ir

import zio.prelude._

final case class Documented[+A](doc: String, value: A) {
  def map[B](f: A => B): Documented[B] = Documented(doc, f(value))

  def flatMap[B](f: A => Documented[B]): Documented[B] = f(value)

  def zip[B](that: Documented[B]): Documented[(A, B)] = Documented(doc, (value, that.value))
}

object Documented {
  implicit val CovariantDocumented: Covariant[Documented] = new Covariant[Documented] {
    def map[A, B](f: A => B): Documented[A] => Documented[B] = _.map(f)
  }
}
