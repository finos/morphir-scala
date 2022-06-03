package org.finos.morphir.knowledge.logic

object microkanren {
  type Field[A] = org.finos.morphir.knowledge.logic.core.Field[A]
  val Field = org.finos.morphir.knowledge.logic.core.Field
  type Goal = org.finos.morphir.knowledge.logic.core.Goal
  val Goal = org.finos.morphir.knowledge.logic.core.Goal
  type State = org.finos.morphir.knowledge.logic.core.State
  val State = org.finos.morphir.knowledge.logic.core.State

  def and(goals: List[Goal]): Goal = goals.foldLeft(Goal.succeed)((g, g2) => Goal.and(g, g2))
  def and(goals: Goal*): Goal      = and(goals.toList)

  /**
   * An alias for equal.
   */
  @inline def eq[A, B](a: A, b: B): Goal = equal(a, b)
  def equal[A, B](a: A, b: B): Goal      = Goal.equal(a, b)

  def or(goals: List[Goal]): Goal = goals.foldLeft(Goal.fail)(Goal.or(_, _))
  def or(goals: Goal*): Goal      = or(goals.toList)

  implicit class MicrokanrenSyntax[A](val self: A) extends AnyVal {
    def ===[B](that: B): Goal = equal(self, that)
  }
}
