package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec}
import org.finos.morphir.ir.Type.{Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.Field
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.Literal.Lit
import zio.Chunk

sealed abstract class MorphirRuntimeError(message: String) extends Exception(message)

final case class DerivationError(message: String)        extends MorphirRuntimeError(message)
final case class DatamodelToIrError(message: String)     extends MorphirRuntimeError(message)
final case class MorphirIRDecodingError(message: String) extends MorphirRuntimeError(message)

sealed abstract class EvaluationError(message: String) extends MorphirRuntimeError(message)

final case class IrToDatamodelError(message: String)         extends EvaluationError(message)
final case class MissingField(message: String)               extends EvaluationError(message)
final case class UnexpectedType(message: String)             extends EvaluationError(message)
final case class UnmatchedPattern(message: String)           extends EvaluationError(message)
final case class FunctionWithoutParameters(message: String)  extends EvaluationError(message)
final case class VariableNotFound(message: String)           extends EvaluationError(message)
final case class DefinitionNotFound(message: String)         extends EvaluationError(message)
final case class SpecificationNotFound(message: String)      extends EvaluationError(message)
final case class ConstructorNotFound(message: String)        extends EvaluationError(message)
final case class ResultDoesNotMatchType(message: String)     extends EvaluationError(message)
final case class FunctionReturnedToTopLevel(message: String) extends EvaluationError(message)
final case class UnsupportedType(message: String)            extends EvaluationError(message)
final case class UnsupportedTypeParameter(message: String)   extends EvaluationError(message)
final case class NotImplemented(message: String)             extends EvaluationError(message)

//TODO: This should be a separate error class, but interface changes required to make that happen
abstract class TypeError(msg: String) extends EvaluationError(msg) {
  def getMsg: String = msg
}
object TypeError {
  def succinct[TA, VA](value: Value[TA, VA]): String  = Succinct.Value(value)
  def succinct[TA](tpe: Type[TA]): String             = Succinct.Type(tpe)
  def succinct[TA](spec: T.Specification[TA]): String = Succinct.TypeSpec(spec)

  final case class TypesMismatch(tpe1: UType, tpe2: UType, msg: String)
      extends TypeError(s"$msg: ${succinct(tpe1)} vs ${succinct(tpe2)}")

  final case class ArgumentDoesNotMatchParameter(arg: TypedValue, param: UType) extends TypeError(
        s"Argument ${succinct(arg)}  of type ${succinct(arg.attributes)} does not match parameter ${succinct(param)}"
      )

  final case class ApplyToNonFunction(nonFunction: TypedValue, arg: TypedValue) extends TypeError(
        s"Tried to apply ${succinct(arg)} to ${succinct(nonFunction)} of type ${succinct(nonFunction.attributes)}, which is not a function"
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
  final case class TypeMissing(err: LookupError) extends TypeError(s"Cannot find type: ${err.getMsg}")

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
      extends EvaluationError("\n" + errors.map(_.toString).mkString("\n"))
}
