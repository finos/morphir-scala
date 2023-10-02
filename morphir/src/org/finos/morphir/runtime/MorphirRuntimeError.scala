package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec}
import org.finos.morphir.ir.Type.{Field, Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.printing.{DetailLevel, PrintIR}
import zio.Chunk
import org.finos.morphir.runtime.ErrorUtils.ErrorInterpolator
import org.finos.morphir.datamodel.{Concept, Data, EnumLabel, Label}

sealed trait MorphirRuntimeError extends Throwable {
  def message: String
  override def getMessage = message

}

object MorphirRuntimeError {

  final case class MorphirIRDecodingError(message: String) extends MorphirRuntimeError

  sealed trait EvaluationError extends MorphirRuntimeError

  final case class MissingField(value: RTValue.Record, field: Name) extends EvaluationError {
    def message = err"Record $value does not contain field ${field.toCamelCase}"
  }

  sealed trait RTValueToMDMError extends MorphirRuntimeError
  object RTValueToMDMError {
    final case class MissingField(value: RTValue.Record, field: Label) extends EvaluationError {
      def message = err"Record $value appeared in result without expected field $field"
    }
  }

  final case class UnexpectedType(expected: String, found: RTValue, hint: String = "") extends EvaluationError {
    def message = err"Expected $expected but found $found. ${if (hint != "") "Hint: " + hint else ""}"
  }
  final case class FailedCoercion(message: String) extends EvaluationError

  final case class IllegalValue(message: String) extends EvaluationError

  final case class WrongNumberOfArguments(function: RTValue.NativeFunctionResult, applied: Int)
      extends EvaluationError {
    def message =
      err"Applied wrong number of args. Needed ${function.arguments} args but got $applied when applying the function $function}"
  }

  final case class UnmatchedPattern(message: String) extends EvaluationError

  final case class FunctionWithoutParameters(message: String) extends EvaluationError

  final case class VariableNotFound(message: String) extends EvaluationError

  final case class DefinitionNotFound(message: String) extends EvaluationError

  final case class SpecificationNotFound(message: String) extends EvaluationError

  final case class ConstructorNotFound(message: String) extends EvaluationError

  final case class ResultDoesNotMatchType(message: String) extends EvaluationError

  final case class VariableAccessError(message: String) extends EvaluationError

  final case class FunctionReturnedToTopLevel(message: String) extends EvaluationError

  final case class UnsupportedType(message: String) extends EvaluationError

  final case class UnsupportedTypeParameter(message: String) extends EvaluationError

  final case class NotImplemented(message: String) extends EvaluationError

  // TODO: This should be a separate error class, but interface changes required to make that happen
  sealed trait TypeError extends MorphirRuntimeError {
    def getMsg: String = message
  }

  object TypeError {

    final case class TypesMismatch(tpe1: UType, tpe2: UType, msg: String)
        extends TypeError {
      def message = (err"$msg: $tpe1 vs $tpe2")
    }
    final case class ApplyToNonFunction(nonFunction: TypedValue, arg: TypedValue) extends TypeError {
      def message = err"Tried to apply $arg to $nonFunction of type ${nonFunction.attributes}, which is not a function"
    }

    final case class LiteralTypeMismatch(lit: Lit, tpe: UType)
        extends TypeError {
      def message = err"Literal $lit is not of type $tpe"
    }
    final case class ImproperType(tpe: UType, msg: String) extends TypeError {
      def message = (err"$msg. Found: $tpe")
    }
    final case class ImproperTypeSpec(fqn: FQName, spec: UTypeSpec, msg: String)
        extends TypeError {
      def message = err"$msg. $fqn points to: $spec"
    }
    final case class CannotDealias(err: LookupError, msg: String = "Cannot dealias type")
        extends TypeError {
      def message = err"$msg: ${err.getMsg}"
    }
    final case class TypeVariableMissing(name: Name) extends TypeError {
      def message = err"Missing type variable ${name.toTitleCase}"
    }
    final case class DefinitionMissing(err: LookupError)
        extends TypeError {
      def message = err"Cannot find definition: ${err.getMsg}"
    }
    final case class TypeMissing(fqn: FQName, err: LookupError) extends TypeError {
      def message = err"Cannot find $fqn: ${err.getMsg}"
    }
    final case class TypeLacksField(tpe: UType, field: Name, msg: String)
        extends TypeError {
      def message = err"$tpe lacks field <${field.toCamelCase}>. $msg"
    }
    final case class TypeHasExtraField(tpe: UType, contract: UType, field: Name) extends TypeError {
      def message = err"$tpe has field <${field.toCamelCase}>, which is not included in $contract"
    }

    final case class ValueLacksField(value: TypedValue, contract: UType, field: Name) extends TypeError {
      def message = err"$value lacks field <${field.toCamelCase}>, which is required by $contract"
    }

    final case class ValueHasExtraField(value: TypedValue, contract: UType, field: Name) extends TypeError {
      def message = err"$value has field <${field.toCamelCase}>, which is not included in $contract"
    }

    //  final case class TypeHasDifferentFieldType(
    //      first: UType,
    //      second: UType,
    //      field: Name,
    //      firstTpe: UType,
    //      secondTpe: UType
    //  ) extends TypeError(
    //        s"tpe for field ${field.toCamelCase} is ${succinct(firstTpe)} in ${succinct(first)} but ${succinct(secondTpe)} in ${succinct(second)}"
    //      )
    final case class ConstructorMissing(err: LookupError, fqn: FQName)
        extends TypeError {
      def message = err"Cannot find constructor $fqn: ${err.getMsg}"
    }
    class SizeMismatch(first: Int, second: Int, msg: String)
        extends TypeError {
      def message = err"$msg: ($first vs $second)"
    }
    final case class ArgNumberMismatch(first: Int, second: Int, msg: String)
        extends SizeMismatch(first: Int, second: Int, msg: String)

    final case class InferenceConflict(message: String) extends TypeError

    final case class UnimplementedType(message: String) extends TypeError

    final case class OtherTypeError(message: String) extends TypeError

    final case class ManyTypeErrors(errors: List[TypeError])
        extends TypeError {
      def message = ("\n" + errors.map(err =>
        s"""
             |${err.getClass.getName.split(".").lastOption.getOrElse(err.getClass.getName)}:
             |${err.getMsg}
           """
      ).mkString("\n"))
    }
  }
}
