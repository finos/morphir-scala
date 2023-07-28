package org.finos.morphir.runtime


sealed abstract class MorphirRuntimeError(message: String) extends Exception(message)


case class DerivationError(message: String)              extends MorphirRuntimeError(message)
case class DatamodelToIrError(message: String)              extends MorphirRuntimeError(message)
sealed abstract class EvaluationError(message: String)              extends MorphirRuntimeError(message)


case class IrToDatamodelError(message: String)              extends EvaluationError(message)
case class MissingField(message: String)              extends EvaluationError(message)
case class UnexpectedType(message: String)            extends EvaluationError(message)
case class UnmatchedPattern(message: String)          extends EvaluationError(message)
case class FunctionWithoutParameters(message: String) extends EvaluationError(message)
case class VariableNotFound(message: String)          extends EvaluationError(message)
case class DefinitionNotFound(message: String)        extends EvaluationError(message)
case class ConstructorNotFound(message: String)       extends EvaluationError(message)
case class TypeNotFound(message: String)              extends EvaluationError(message)
case class ResultDoesNotMatchType(message: String)     extends EvaluationError(message)
case class FunctionReturnedToTopLevel(message: String) extends EvaluationError(message)
case class UnsupportedTypeParameter(message: String)   extends EvaluationError(message)
case class UnsupportedType(message: String)            extends EvaluationError(message)
case class NotImplemented(message: String)            extends EvaluationError(message)
