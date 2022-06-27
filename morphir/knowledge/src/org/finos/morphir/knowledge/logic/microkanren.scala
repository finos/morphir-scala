package org.finos.morphir.knowledge.logic

object microkanren extends Kernel {

  implicit class MicrokanrenSyntax[A](val self: A) extends AnyVal {
    def ===[B](that: B): Goal = equal(self, that)
  }
}
