package org.finos.morphir.knowledge.logic.core

sealed trait Goal extends Serializable {
  def apply(state: State): SStream
}

object Goal {

  val succeed: Goal = new Goal {
    override def apply(state: State): SStream = SStream.succeed(state)
  }

  val fail: Goal = new Goal {
    override def apply(state: State): SStream = SStream.empty
  }

  def equal[A, B](a: A, b: B): Goal = Equal(a, b)

  def and(g1: Goal, g2: Goal): Goal = Conjunction(g1, g2)
  def or(g1: Goal, g2: Goal): Goal = Disjunction(g1, g2)

  private final case class Disjunction(lhs: Goal, rhs: Goal) extends Goal {
    override def apply(state: State): SStream = lhs(state) <> rhs(state)
  }

  private final case class Conjunction(lhs: Goal, rhs: Goal) extends Goal {
    override def apply(state: State): SStream = lhs(state).flatMap(s => rhs(s))
  }
  private final case class Equal[A, B](a: A, b: B) extends Goal {
    def apply(state: State): SStream =
      state.unify(a, b) match {
        case Some(s) => SStream.succeed(s)
        case None    => SStream.empty
      }
  }

}
