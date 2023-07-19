package org.finos.morphir.runtime.quick

sealed abstract class EvaluationError(message: String) extends Exception(message)

case class MissingField(message: String)              extends EvaluationError(message)
case class UnexpectedType(message: String)            extends EvaluationError(message)
case class UnmatchedPattern(message: String)          extends EvaluationError(message)
case class FunctionWithoutParameters(message: String) extends EvaluationError(message)
case class VariableNotFound(message: String)          extends EvaluationError(message)
case class DefinitionNotFound(message: String)        extends EvaluationError(message)
case class ConstructorNotFound(message: String)       extends EvaluationError(message)

case class ResultDoesNotMatchType(message: String)     extends EvaluationError(message)
case class FunctionReturnedToTopLevel(message: String) extends EvaluationError(message)
case class UnsupportedTypeParameter(message: String)   extends EvaluationError(message)
case class UnsupportedType(message: String)            extends EvaluationError(message)
