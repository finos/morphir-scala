package morphir.knowledge.logic

import morphir.knowledge.logic.core.FieldConstraint
import morphir.knowledge.logic.model.{Constraint, ConstraintModel}
import morphir.knowledge.logic.converter.ConstraintConverter

trait Kernel {
  type Field[A] = morphir.knowledge.logic.core.Field[A]
  val Field = morphir.knowledge.logic.core.Field
  type Goal = morphir.knowledge.logic.core.Goal
  val Goal = morphir.knowledge.logic.core.Goal
  type State = morphir.knowledge.logic.core.State
  val State = morphir.knowledge.logic.core.State
  type SStream = morphir.knowledge.logic.core.SStream
  val SStream = morphir.knowledge.logic.core.SStream

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
