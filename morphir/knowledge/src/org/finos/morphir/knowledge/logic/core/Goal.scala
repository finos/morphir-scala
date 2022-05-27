package org.finos.morphir.knowledge.logic.core

sealed trait Goal extends Serializable {
  def apply(state: State): SStream
}

object Goal {

  def equal[A, B](a: A, b: B): Goal = Equal(a, b)

  private final case class Equal[A, B](a: A, b: B) extends Goal {
    def apply(state: State): SStream = {
      val newState = state.unify(a, b)
      SStream.succeed(newState)
    }
  }
}
