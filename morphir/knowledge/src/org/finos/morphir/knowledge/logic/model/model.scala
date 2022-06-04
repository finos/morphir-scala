package org.finos.morphir.knowledge.logic.model
import org.finos.morphir.knowledge.logic.core.{Goal, Name, Value => LValue}
import scala.reflect.ClassTag

private[knowledge] final case class ConstraintModel(constraint: Constraint)

private[knowledge] sealed trait ConstraintValue
private[knowledge] object ConstraintValue {

  private[knowledge] final case class Value(value: LValue)                extends ConstraintValue
  private[knowledge] final case class Field(name: Name, tpe: ClassTag[_]) extends ConstraintValue
}

private[knowledge] sealed trait Constraint
object Constraint {
  private[knowledge] final case class And(constraints: List[ConstraintModel])              extends Constraint
  private[knowledge] final case class Equal(left: ConstraintValue, right: ConstraintValue) extends Constraint
  private[knowledge] final case class Or(constraints: List[ConstraintModel])               extends Constraint
  private[knowledge] final case class Relation(goal: Goal)                                 extends Constraint
}
