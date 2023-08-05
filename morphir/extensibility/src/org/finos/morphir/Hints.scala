package org.finos.morphir

trait Hints {
  def isEmpty: Boolean
}

object Hints {
  val empty: Hints = Impl()

  final case class Impl() extends Hints {
    override def isEmpty: Boolean = true
  }
}


