package org.finos.morphir.knowledge.core

sealed trait Fact extends Serializable {
  def apply(state: State): SStream
}

object Fact {

  /**
   * An alias for equal.
   */
  @inline def eq[A, B](a: A, b: B): Fact = equal(a, b)
  def equal[A, B](a: A, b: B): Fact      = Equal(a, b)

  private final case class Equal[A, B](a: A, b: B) extends Fact {
    def apply(state: State): SStream = {
      val newState = state.unify(a, b)
      SStream.succeed(newState)
    }
  }
}
