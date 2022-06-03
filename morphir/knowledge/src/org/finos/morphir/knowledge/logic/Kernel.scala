package org.finos.morphir.knowledge.logic

trait Kernel {
  type Field[A] = org.finos.morphir.knowledge.logic.core.Field[A]
  val Field = org.finos.morphir.knowledge.logic.core.Field
  type Goal = org.finos.morphir.knowledge.logic.core.Goal
  val Goal = org.finos.morphir.knowledge.logic.core.Goal
  type State = org.finos.morphir.knowledge.logic.core.State
  val State = org.finos.morphir.knowledge.logic.core.State
  type SStream = org.finos.morphir.knowledge.logic.core.SStream
  val SStream = org.finos.morphir.knowledge.logic.core.SStream

  def append[A](s1: SStream, s2: SStream): SStream =
    // Note this is not correct, but it is a start.
    s1 <> s2
}
