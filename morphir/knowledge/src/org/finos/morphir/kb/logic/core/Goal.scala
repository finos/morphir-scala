package morphir.knowledge.logic.core

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

  def and(g1: Goal, g2: Goal): Goal = Conj(g1, g2)
  def constraint(constrainedFields: List[Field[_]], constraint: FieldConstraint): Goal =
    FromConstraint(constrainedFields, constraint)
  def equal[A, B](a: A, b: B): Goal = Equal(a, b)
  def or(g1: Goal, g2: Goal): Goal  = Disj(g1, g2)

  private final case class Conj(lhs: Goal, rhs: Goal) extends Goal {
    override def apply(state: State): SStream = lhs(state).flatMap(s => rhs(s))
  }
  private final case class Disj(lhs: Goal, rhs: Goal) extends Goal {
    override def apply(state: State): SStream = lhs(state) <> rhs(state)
  }

  private final case class Equal[A, B](a: A, b: B) extends Goal {
    def apply(state: State): SStream =
      state.unify(a, b) match {
        case Some(s) => SStream.succeed(s)
        case None    => SStream.empty
      }
  }

  private final case class FromConstraint(constrainedFields: List[Field[_]], fieldConstraint: FieldConstraint)
      extends Goal {

    override def apply(state: State): SStream = {

      val isEveryConstrainedFieldInitialized = constrainedFields.forall(state.hasValue(_))
      val stateWithConstraints               = constrainedFields.foldLeft(Option(state))(createConstraintRegister(_, _))

      (isEveryConstrainedFieldInitialized, stateWithConstraints) match {
        case (true, Some(s)) =>
          fieldConstraint.lift(s) match {
            case Some(s) => SStream.succeed(s)
            case None    => SStream.empty
          }
        case (_, Some(s)) => SStream.succeed(s)
        case (_, None)    => SStream.empty
      }
    }

    private def createConstraintRegister(state: Option[State], field: Field[_]): Option[State] =
      state.map(_.addConstraint(field, fieldConstraint))

  }

}
