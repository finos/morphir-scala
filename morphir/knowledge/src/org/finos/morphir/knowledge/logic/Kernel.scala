package org.finos.morphir.knowledge.logic

import org.finos.morphir.knowledge.logic.core.FieldConstraint
import org.finos.morphir.knowledge.logic.model.{Constraint, ConstraintModel}
import org.finos.morphir.knowledge.logic.converter.ConstraintConverter

trait Kernel {
  type Field[A] = org.finos.morphir.knowledge.logic.core.Field[A]
  val Field = org.finos.morphir.knowledge.logic.core.Field
  type Goal = org.finos.morphir.knowledge.logic.core.Goal
  val Goal = org.finos.morphir.knowledge.logic.core.Goal
  type State = org.finos.morphir.knowledge.logic.core.State
  val State = org.finos.morphir.knowledge.logic.core.State
  type SStream = org.finos.morphir.knowledge.logic.core.SStream
  val SStream = org.finos.morphir.knowledge.logic.core.SStream

  def constraintConverter: ConstraintConverter = ConstraintConverter.Default

  def and(goals: List[Goal]): Goal = goals.foldLeft(Goal.succeed)((g, g2) => Goal.and(g, g2))
  def and(goals: Goal*): Goal      = and(goals.toList)

  def constraint(constrainedFields: List[Field[_]], constraint: FieldConstraint): Goal =
    Goal.constraint(constrainedFields, constraint)

  /**
   * An alias for equal.
   */
  @inline def eq[A, B](a: A, b: B): Goal = equal(a, b)
  def equal[A, B](a: A, b: B): Goal      = Goal.equal(a, b)

  def or(goals: List[Goal]): Goal = goals.foldLeft(Goal.fail)(Goal.or(_, _))
  def or(goals: Goal*): Goal      = or(goals.toList)

  def run(constraint: Constraint, state: State): SStream = {
    val goal = constraintConverter.convertToGoal(ConstraintModel(constraint))
    goal(state)
  }

  def run(constraint: Constraint): SStream = run(constraint, State.empty)

  def run(goal: Goal, state: State): SStream = goal(state)
  def run(goal: Goal): SStream               = run(goal, State.empty)

}
