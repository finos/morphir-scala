package org.finos.morphir.runtime

sealed abstract class RTError(message: String) extends Exception(message)
object RTError {
  final case class DerivationError(message: String) extends RTError(message)

  final case class DatamodelToIrError(message: String) extends RTError(message)

  final case class MorphirIRDecodingError(message: String) extends RTError(message)

  sealed abstract class EvaluationError(message: String) extends RTError(message)

  sealed abstract class TypeError(message: String) extends RTError(message)

  sealed abstract class VariableAccessError(message: String) extends EvaluationError(message)

  final case class UnsupportedType(message: String) extends TypeError(message)

  final case class TooManyArgs(message: String) extends TypeError(message)

  final case class IrToDatamodelError(message: String) extends EvaluationError(message)

  final case class MissingField(message: String) extends EvaluationError(message)

  final case class UnexpectedType(message: String) extends EvaluationError(message)

  final case class UnmatchedPattern(message: String) extends EvaluationError(message)

  final case class FunctionWithoutParameters(message: String) extends EvaluationError(message)

  final case class VariableNotFound(message: String) extends VariableAccessError(message)

  final case class DefinitionNotFound(message: String) extends EvaluationError(message)

  final case class SpecificationNotFound(message: String) extends EvaluationError(message)

  final case class ConstructorNotFound(message: String) extends EvaluationError(message)

  // final case class TypeNotFound(message: String)               extends EvaluationError(message)
  final case class ResultDoesNotMatchType(message: String) extends EvaluationError(message)

  final case class FunctionReturnedToTopLevel(message: String) extends EvaluationError(message)

  final case class UnsupportedTypeParameter(message: String) extends EvaluationError(message)

  final case class NotImplemented(message: String) extends EvaluationError(message)
}
