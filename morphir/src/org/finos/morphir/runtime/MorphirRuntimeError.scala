package org.finos.morphir.runtime

import org.finos.morphir.naming.*
import org.finos.morphir.naming.*
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Pattern, TypedValue, Value, USpecification as UValueSpec}
import org.finos.morphir.ir.Type.{Field, Type, UType, USpecification as UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.printing.{DetailLevel, PrintIR}
import zio.Chunk
import org.finos.morphir.runtime.ErrorUtils.ErrorInterpolator
import org.finos.morphir.datamodel.{Concept, Data, EnumLabel, Label}

import scala.collection.immutable.{AbstractSeq, LinearSeq}

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

  final case class UnexpectedType(expected: String, found: RTValue, hint: String = "") extends EvaluationError {
    def message = err"Expected $expected but found $found. ${if (hint != "") "Hint: " + hint else ""}"
  }
  final case class FailedCoercion(message: String) extends EvaluationError

  final case class IllegalValue(cause: String, context: String = "") extends EvaluationError {
    def message                         = s"$cause . $context"
    def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
  }

  final case class WrongNumberOfArguments(function: RTValue.NativeFunctionResult, applied: Int)
      extends EvaluationError {
    def message =
      err"Applied wrong number of args. Needed ${function.arguments} args but got $applied when applying the function $function}"
  }

  final case class UnmatchedPattern(value: RTValue, node: Any, patterns: Pattern[UType]*) extends EvaluationError {
    def message = err"Failed to match $value to any pattern from $patterns in node $node"
  }

  final case class VariableNotFound(name: Name) extends EvaluationError {
    def message = err"Variable ${name.toCamelCase} not found in store."
  }

  // TODO: Message definition should live in this class, but requires visibility of Utils functions not present here.
  // TODO: Ideally these would fall under "Lookup Errors", but they do not come from the Distributions packet or equivalent structure
  final case class DefinitionNotFound(message: String) extends EvaluationError

  final case class ConstructorNotFound(message: String) extends EvaluationError

  final case class WrongArgumentTypes(msg: String, args: RTValue*) extends EvaluationError {
    def message = args.toList match {
      case List(argOne)                   => err"Wrong argument type passed: $argOne. $msg"
      case List(argOne, argTwo)           => err"Wrong argument types passed: $argOne, $argTwo. $msg"
      case List(argOne, argTwo, argThree) => err"Wrong argument types passed: $argOne, $argTwo, $argThree. $msg"
      case other                          => err"Wrong argument types passed:$other. $msg"
    }
  }

  final case class VariableAccessError(message: String) extends EvaluationError

  final case class UnsupportedType(tpe: UType, reason: String) extends EvaluationError {
    def message = err"Type $tpe not supported. $reason"
  }

  final case class UnsupportedTypeSpecification(spec: UTypeSpec, reason: String) extends EvaluationError {
    def message = err"Type Specification $spec not supported. $reason"
  }

  final case class InvalidState(context: String) extends EvaluationError {
    def message = s"$context (This should not be reachable, and indicates an evaluator bug.)"
  }
  final case class NotImplemented(message: String) extends EvaluationError

  // LookupErrors are a generic form of error that can occur at different points
  sealed trait LookupError extends EvaluationError with TypeError {
    // it is often useful to be able to attach context when a lookup failed from called code.
    def withContext(newContext: String): LookupError
  }
  object LookupError {
    case class MissingPackage(pkgName: PackageName, context: String = "") extends LookupError {
      def message                         = s"Package ${pkgName.toString} not found. $context"
      def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
    }
    case class MissingModule(pkgName: PackageName, modName: ModuleName, context: String = "")
        extends LookupError {
      def message = s"Package ${pkgName.toString} does not contain module ${modName.toString}. $context"

      def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
    }

    case class MissingType(pkgName: PackageName, modName: ModuleName, typeName: Name, context: String = "")
        extends LookupError {
      def message =
        (s"Module ${pkgName.toString}:${modName.toString} has no type named ${typeName.toTitleCase}. $context")

      def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
    }
    case class MissingDefinition(pkgName: PackageName, modName: ModuleName, defName: Name, context: String = "")
        extends LookupError {
      def message =
        s"Module ${pkgName.toString}:${modName.toString} has no definition named ${defName.toCamelCase}. $context"

      def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
    }
  }

  sealed trait RTValueToMDMError extends MorphirRuntimeError
  object RTValueToMDMError {
    final case class MissingField(value: RTValue.Record, field: Label) extends RTValueToMDMError {
      def message = err"Record $value appeared in result without expected field $field"
    }

    final case class ResultTypeMismatch(result: RTValue, concept: Concept, explanation: String)
        extends EvaluationError {
      def message =
        err"""Result $result cannot be matched to type $concept. $explanation.
             (This type was derived from the entry point. These may be nested within broader result/type trees.""".stripMargin
    }
  }

  sealed trait TypeError extends MorphirRuntimeError
  object TypeError {

    final case class TypesMismatch(tpe1: UType, tpe2: UType, msg: String)
        extends TypeError {
      def message = (err"$msg: $tpe1 vs $tpe2")
    }
    final case class ApplyToNonFunction(applyNode: TypedValue, nonFunction: TypedValue, arg: TypedValue)
        extends TypeError {
      def message =
        err"$applyNode tried to apply $arg to $nonFunction of type ${nonFunction.attributes}, which is not a function"
    }

    final case class LiteralTypeMismatch(lit: Lit, tpe: UType)
        extends TypeError {
      def message = err"Literal $lit is not of type $tpe"
    }
    final case class ImproperType(tpe: UType, explanation: String) extends TypeError {
      def message = (err"Improper Type: $explanation. Found: $tpe")
    }
    final case class ImproperTypeSpec(fqn: FQName, spec: UTypeSpec, explanation: String)
        extends TypeError {
      def message = err"Improper Type Specification found: $explanation. $fqn points to: $spec"
    }
    final case class CannotDealias(err: LookupError, xplanation: String = "Cannot dealias type")
        extends TypeError {
      def message = err"$xplanation: ${err.message}"
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
    class SizeMismatch(first: Int, second: Int, msg: String)
        extends TypeError {
      def message = err"$msg: ($first vs $second)"
    }
    final case class ArgNumberMismatch(first: Int, second: Int, msg: String)
        extends SizeMismatch(first: Int, second: Int, msg: String)

    final case class InferenceConflict(older: UType, newer: UType, name: Name) extends TypeError {
      def message =
        err"While trying to bind the type variables of the entry point function, the input matched type variable ${name.toCamelCase} with $older and then also $newer, which are not the same."
    }

    final case class UnknownTypeMismatch(tpe1: UType, tpe2: UType, hint: String = "") extends TypeError {
      def message =
        err"Could not match $tpe1 to $tpe2, but it is unclear why. ${if (hint != "") "Hint: " + hint else ""}"
    }

    final case class UnsupportedType(tpe: UType, hint: String = "") extends TypeError {
      def message = err"$tpe is not currently supported.  ${if (hint != "") "Hint: " + hint else ""}"
    }
    final case class OtherTypeError(message: String) extends TypeError

    final case class ManyTypeErrors(errors: List[TypeError])
        extends TypeError {
      def message = ("\n" + errors.map(err =>
        s"""
             |${err.getClass.getName.split(".").lastOption.getOrElse(err.getClass.getName)}:
             |${err.message}
           """
      ).mkString("\n"))
    }
  }
}
