package morphir.knowledge.logic.converter
import morphir.knowledge.logic.core.Value
import morphir.knowledge.logic.model.ConstraintValue
import morphir.knowledge.logic.core.Field

private[knowledge] trait ValueConverter {
  def convertToConstraintValue(v: Value): ConstraintValue
  def convertToValue(constraintValue: ConstraintValue): Value
}

private[knowledge] object ValueConverter {
  object Default extends ValueConverter {
    def convertToConstraintValue(v: Value): ConstraintValue = v match {
      case Field(name, tpe) => ConstraintValue.Field(name, tpe)
      case _                => ConstraintValue.Value(v)
    }

    def convertToValue(constraintValue: ConstraintValue): Value = constraintValue match {
      case ConstraintValue.Field(name, tpe) => Field(name, tpe)
      case ConstraintValue.Value(v)         => v
    }
  }
}
