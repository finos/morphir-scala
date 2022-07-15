package morphir.knowledge.logic.converter
import morphir.knowledge.logic.model.ConstraintModel
import morphir.knowledge.logic.core.Goal

private[knowledge] trait ConstraintConverter {
  def convertToGoal(constraintModel: ConstraintModel): Goal
}

private[knowledge] object ConstraintConverter {
  sealed abstract class ConstraintCombinatorType extends Product with Serializable

  object ConstraintCombinatorType {
    case object None extends ConstraintCombinatorType
    case object And  extends ConstraintCombinatorType
    case object Or   extends ConstraintCombinatorType
  }

  object Default extends ConstraintConverter {
    def convertToGoal(constraintModel: ConstraintModel): Goal =
      ???
  }
}
