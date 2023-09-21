package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec}
import org.finos.morphir.ir.Type.{Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.Field
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.printing.{DetailLevel, PrintIR}
import zio.Chunk
import org.finos.morphir.runtime.ErrorInterpolator.{*, given}

sealed abstract class MorphirRuntimeError(message: String) extends Exception(message)

final case class DerivationError(message: String)        extends MorphirRuntimeError(message)
final case class DatamodelToIrError(message: String)     extends MorphirRuntimeError(message)
final case class MorphirIRDecodingError(message: String) extends MorphirRuntimeError(message)

sealed abstract class EvaluationError(message: String) extends MorphirRuntimeError(message)

final case class IrToDatamodelError(message: String)        extends EvaluationError(message)
final case class MissingField(message: String)              extends EvaluationError(message)
final case class UnexpectedType(message: String)            extends EvaluationError(message)
final case class IllegalValue(message: String)              extends EvaluationError(message)
final case class UnmatchedPattern(message: String)          extends EvaluationError(message)
final case class FunctionWithoutParameters(message: String) extends EvaluationError(message)
final case class VariableNotFound(message: String)          extends EvaluationError(message)
final case class DefinitionNotFound(message: String)        extends EvaluationError(message)
final case class SpecificationNotFound(message: String)     extends EvaluationError(message)
final case class ConstructorNotFound(message: String)       extends EvaluationError(message)
final case class ResultDoesNotMatchType(message: String)    extends EvaluationError(message)

final case class VariableAccessError(message: String)        extends EvaluationError(message)
final case class FunctionReturnedToTopLevel(message: String) extends EvaluationError(message)
final case class UnsupportedType(message: String)            extends EvaluationError(message)
final case class UnsupportedTypeParameter(message: String)   extends EvaluationError(message)
final case class NotImplemented(message: String)             extends EvaluationError(message)

//TODO: This should be a separate error class, but interface changes required to make that happen
abstract class TypeError(msg: String) extends MorphirRuntimeError(msg) {
  def getMsg: String = msg
}
object TypeError {
  def succinct(any: Any): String = PrintIR(any, detailLevel = DetailLevel.BirdsEye).toString

  final case class TypesMismatch(tpe1: UType, tpe2: UType, msg: String)
      extends TypeError(err"$msg: $tpe1 vs $tpe2")

  final case class ApplyToNonFunction(nonFunction: TypedValue, arg: TypedValue) extends TypeError(
        err"Tried to apply $arg to $nonFunction of type ${nonFunction.attributes}, which is not a function"
      )

  final case class LiteralTypeMismatch(lit: Lit, tpe: UType)
      extends TypeError(s"Literal $lit is not of type ${succinct(tpe)}")

  final case class ImproperType(tpe: UType, msg: String) extends TypeError(s"$msg. Found: ${succinct(tpe)}")
  final case class ImproperTypeSpec(fqn: FQName, spec: UTypeSpec, msg: String)
      extends TypeError(s"$msg. $fqn points to: ${succinct(spec)}")

  final case class CannotDealias(err: LookupError, msg: String = "Cannot dealias type")
      extends TypeError(s"$msg: ${err.getMsg}")
  final case class TypeVariableMissing(name: Name) extends TypeError(s"Missing type variable $name.toTitleCase")
  final case class DefinitionMissing(err: LookupError)
      extends TypeError(s"Cannot find definition: ${err.getMsg}")
  final case class TypeMissing(fqn: FQName, err: LookupError) extends TypeError(s"Cannot find $fqn: ${err.getMsg}")

  final case class TypeLacksField(tpe: UType, field: Name, msg: String)
      extends TypeError(s"${succinct(tpe)} lacks field ${field.toCamelCase}. $msg")
  final case class TypeHasExtraField(tpe: UType, contract: UType, field: Name) extends TypeError(
        s"${succinct(tpe)} has field ${field.toCamelCase}, which is not included in ${succinct(contract)}"
      )
  final case class ValueLacksField(value: TypedValue, contract: UType, field: Name) extends TypeError(
        s"${succinct(value)} lacks field ${field.toCamelCase}, which is required by ${succinct(contract)}"
      )
  final case class ValueHasExtraField(value: TypedValue, contract: UType, field: Name) extends TypeError(
        s"${succinct(value)} has field ${field.toCamelCase}, which is not included in ${succinct(contract)}"
      )
  final case class TypeHasDifferentFieldType(
      first: UType,
      second: UType,
      field: Name,
      firstTpe: UType,
      secondTpe: UType
  ) extends TypeError(
        s"tpe for field ${field.toCamelCase} is ${succinct(firstTpe)} in ${succinct(first)} but ${succinct(secondTpe)} in ${succinct(second)}"
      )
  final case class ConstructorMissing(err: LookupError, fqn: FQName)
      extends TypeError(s"Cannot find constructor $fqn: ${err.getMsg}")

  class SizeMismatch(first: Int, second: Int, msg: String)
      extends TypeError(s"$msg: ($first vs $second)")
  final case class ArgNumberMismatch(first: Int, second: Int, msg: String)
      extends SizeMismatch(first: Int, second: Int, msg: String)
  final case class InferenceConflict(msg: String) extends TypeError(msg)
  final case class UnimplementedType(msg: String) extends TypeError(msg)
  final case class OtherTypeError(msg: String)    extends TypeError(msg)
  final case class ManyTypeErrors(errors: List[TypeError])
      extends TypeError("\n" + errors.map(_.toString).mkString("\n"))
}
