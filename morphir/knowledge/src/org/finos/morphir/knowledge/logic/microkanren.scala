package org.finos.morphir.knowledge.logic

object microkanren {
  type State = org.finos.morphir.knowledge.logic.core.State
  val State = org.finos.morphir.knowledge.logic.core.State
  type Goal = org.finos.morphir.knowledge.logic.core.Goal
  val Goal = org.finos.morphir.knowledge.logic.core.Goal

  /**
   * An alias for equal.
   */
  @inline def eq[A, B](a: A, b: B): Goal = equal(a, b)
  def equal[A, B](a: A, b: B): Goal      = Goal.equal(a, b)

  implicit class MicrokanrenSyntax[A](val self: A) extends AnyVal {
    def ===(that: A): Goal = equal(self, that)
  }
}
