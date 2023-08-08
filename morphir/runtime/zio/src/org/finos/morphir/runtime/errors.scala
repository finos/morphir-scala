package org.finos.morphir
package runtime

import org.finos.morphir.naming.*

sealed abstract class KernelError(message: Option[String])
    extends Exception(message.orNull)
    with Product
    with Serializable

sealed abstract class EvaluationError(message: Option[String]) extends KernelError(message)

object EvaluationError {
  final case class VariableNotFound(name: Name)
      extends EvaluationError(Some(s"Evaluation Error: Could not find variable $name"))
  final case class VariableResolutionError(name: Name, msg: String)
      extends EvaluationError(Some(s"Evaluation Error: Error occurred while resolving variable $name. $msg"))
  final case class UnsupportedTupleArity(value: Any, arity: Int)
      extends EvaluationError(Some(s"Evaluation Error: Unsupported tuple arity of $arity for value: $value"))
}
