package org.finos.morphir.runtime

import org.finos.morphir.naming.*
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Pattern, TypedValue, Value, USpecification as UValueSpec}
import org.finos.morphir.ir.Type.{Field, Type, UType, USpecification as UTypeSpec, UDefinition as UTypeDef}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.printing.{DetailLevel, PrintIR}
import org.finos.morphir.ir.distribution.Distribution
import zio.Chunk
import org.finos.morphir.runtime.ErrorUtils.ErrorInterpolator
import org.finos.morphir.datamodel.{Concept, Data, EnumLabel, Label}

import scala.collection.immutable.{AbstractSeq, LinearSeq}

sealed trait MorphirRuntimeError extends Throwable {
  def message: String

  override def getMessage = message
}

sealed trait AttachedLocation {
  def location: Option[CodeLocation]
}

object MorphirRuntimeError {

  final case class MorphirIRDecodingError(message: String) extends MorphirRuntimeError

  final case class OtherError(cause: String, stuff: Any*) extends MorphirRuntimeError {
    def message = {
      val l = stuff.toList
      if (l.length == 0) err"$cause"
      else if (l.length == 1) err"$cause: ${l(0)}"
      else if (l.length == 2) err"$cause: ${l(0)} ${l(1)}"
      else err"$cause: $l"
    }
  }

  final case class TopLevelError(
      entryPoint: FQName,
      dists: Map[PackageName, Distribution.Lib],
      inner: MorphirRuntimeError
  ) extends MorphirRuntimeError {
    def message = {
      val basicClause =
        s"${inner.getClass.getSimpleName} : ${inner.message}"
      val entryNameClause = s"While evaluating entry point${entryPoint.toString}"
      val distsClause     = s"With known distributions:\n\t ${dists.keys.mkString("\n\t")}"
      s"$basicClause\n$entryNameClause\n$distsClause"
    }
  }

  sealed trait EvaluationError extends MorphirRuntimeError {
    def stack(frame: CodeLocation): EvaluationError = CodeLocatedError(this, frame :: Nil, None)
    def source(code: String): EvaluationError       = CodeLocatedError(this, Nil, Some((s">>>$code<<<", code)))
  }

  /**
   * This class lets us track the source location from which errors are thrown Private because its fields are tied to
   * implementation; create this thru EvaluationError.source and .stack
   *
   * @param inner
   *   The error being wrapped
   * @param stack
   *   A list of code locations corresponding to function calls
   * @param sourceTaggedUntagged
   *   an optional pair of (a reconstruction of) the source doe this came from, with and without tags on the specific
   *   clause that generated the error
   */
  private final case class CodeLocatedError(
      inner: EvaluationError,
      stack: List[CodeLocation],
      sourceTaggedUntagged: Option[(String, String)]
  ) extends EvaluationError with AttachedLocation {
    def message = {
      val sourceString = sourceTaggedUntagged match {
        case Some((tagged, _)) => s"Thrown from: $tagged\n\t"
        case None              => ""
      }
      val stackStrings = stack.map(loc => s"at morphir: $loc")
      val stackString =
        if (stack.length <= 10) stackStrings.mkString("\n\t")
        else {
          val (first, rest)   = stackStrings.splitAt(5)
          val (middle, tail)  = rest.splitAt((rest.length - 5))
          val (common, count) = middle.groupBy(identity).map { case (loc, repeats) => (loc, repeats.size) }.maxBy(_._2)
          val middleString    = s"${middle.length} more of which $count are: \n\t\t $common"
          (first ++ (middleString :: tail)).mkString("\n\t")
        }
      s"${inner.getClass.getSimpleName} : ${inner.message} \n\t" + sourceString + stackString + "\n"

    }
    override def stack(frame: CodeLocation): EvaluationError =
      this.copy(stack = stack :+ frame)
    override def source(code: String): EvaluationError = {
      def countMatches(inner: String, outer: String) = outer.sliding(inner.length).count(_ == inner)
      if (!stack.isEmpty) this // Only include the detailed error at the top of the stack
      else sourceTaggedUntagged match {
        case Some((tagged, untagged)) if countMatches(untagged, code) == 1 =>
          val newTagged   = code.replace(untagged, tagged)
          val newUntagged = code
          this.copy(sourceTaggedUntagged = Some((newTagged, newUntagged)))
        case _ =>
          this.copy(sourceTaggedUntagged =
            Some((s">>>$code<<<", code))
          ) // We don't know exactly where the error is, so we'll just tag the whole thing
      }
    }

    val location: Option[CodeLocation] = stack.headOption
  }

  final case class ExternalError(error: Throwable, location: Option[CodeLocation] = None) extends EvaluationError
      with AttachedLocation {
    def message = s"External error: ${error.getMessage}"
  }

  final case class MissingField(value: RTValue.Record, field: Name, location: Option[CodeLocation] = None)
      extends EvaluationError with AttachedLocation {
    def message = err"Record $value does not contain field ${field.toCamelCase}"
  }

  final case class UnexpectedType(
      expected: String,
      found: RTValue,
      hint: String = "",
      location: Option[CodeLocation] = None
  ) extends EvaluationError with AttachedLocation {
    def message = err"Expected $expected but found $found. ${if (hint != "") "Hint: " + hint else ""}"
  }
  final case class UnexpectedTypeWithIR(
      expected: String,
      found: RTValue,
      ir: TypedValue,
      hint: String = "",
      location: Option[CodeLocation] = None
  ) extends EvaluationError with AttachedLocation {
    def message = err"Expected $expected but found $found from IR $ir. ${if (hint != "") "Hint: " + hint else ""}"
  }
  final case class FailedCoercion(message: String, location: Option[CodeLocation] = None) extends EvaluationError
      with AttachedLocation

  final case class IllegalValue(cause: String, context: String = "", location: Option[CodeLocation] = None)
      extends EvaluationError with AttachedLocation {
    def message                         = s"$cause . $context"
    def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
  }

  final case class WrongNumberOfArguments(
      function: RTValue.NativeFunctionResult,
      applied: Int,
      location: Option[CodeLocation] = None
  ) extends EvaluationError with AttachedLocation {
    def message =
      err"Applied wrong number of args. Needed ${function.arguments} args but got $applied when applying the function $function}"
  }

  final case class UnmatchedPattern(
      value: RTValue,
      node: Any,
      location: Option[CodeLocation],
      patterns: Pattern[UType]*
  ) extends EvaluationError with AttachedLocation {
    def message = err"Failed to match $value to any pattern from $patterns in node $node"
  }

  final case class VariableNotFound(name: Name, location: Option[CodeLocation] = None) extends EvaluationError
      with AttachedLocation {
    def message = err"Variable ${name.toCamelCase} not found in store."
  }

  // TODO: Message definition should live in this class, but requires visibility of Utils functions not present here.
  // TODO: Ideally these would fall under "Lookup Errors", but they do not come from the Distributions packet or equivalent structure
  final case class DefinitionNotFound(message: String, location: Option[CodeLocation] = None) extends EvaluationError
      with AttachedLocation

  final case class ConstructorNotFound(message: String, location: Option[CodeLocation] = None) extends EvaluationError
      with AttachedLocation

  final case class WrongArgumentTypes(msg: String, args: RTValue*) extends EvaluationError {
    def message = args.toList match {
      case List(argOne)                   => err"Wrong argument type passed: $argOne. $msg"
      case List(argOne, argTwo)           => err"Wrong argument types passed: $argOne, $argTwo. $msg"
      case List(argOne, argTwo, argThree) => err"Wrong argument types passed: $argOne, $argTwo, $argThree. $msg"
      case other                          => err"Wrong argument types passed:$other. $msg"
    }
  }

  final case class VariableAccessError(message: String, location: Option[CodeLocation] = None) extends EvaluationError
      with AttachedLocation

  final case class UnsupportedType(tpe: UType, reason: String, location: Option[CodeLocation] = None)
      extends EvaluationError with AttachedLocation {
    def message = err"Type $tpe not supported. $reason"
  }

  final case class UnsupportedTypeSpecification(spec: UTypeSpec, reason: String, location: Option[CodeLocation] = None)
      extends EvaluationError with AttachedLocation {
    def message = err"Type Specification $spec not supported. $reason"
  }

  final case class UnsupportedTypeDefinition(spec: UTypeDef, reason: String, location: Option[CodeLocation] = None)
      extends EvaluationError with AttachedLocation {
    def message = err"Type Definition $spec not supported. $reason"
  }

  final case class InvalidState(context: String, location: Option[CodeLocation], vals: RTValue*) extends EvaluationError
      with AttachedLocation {
    def message =
      if (vals.length == 0) err"$context (This should not be reachable, and indicates an evaluator bug.)"
      else if (vals.length == 1)
        err"$context ${vals(0)} (This should not be reachable, and indicates an evaluator bug.)"
      else if (vals.length == 2)
        err"$context ${vals(0)}, ${vals(1)} (This should not be reachable, and indicates an evaluator bug.)"
      else err"$context $vals (This should not be reachable, and indicates an evaluator bug.)"
  }
  final case class NotImplemented(message: String, location: Option[CodeLocation] = None) extends EvaluationError
      with AttachedLocation

  // LookupErrors are a generic form of error that can occur at different points
  sealed trait LookupError extends EvaluationError with TypeError {
    // it is often useful to be able to attach context when a lookup failed from called code.
    def withContext(newContext: String): LookupError
  }
  object LookupError {
    case class MissingPackage(pkgName: PackageName, context: String = "", location: Option[CodeLocation] = None)
        extends LookupError {
      def message                         = s"Package ${pkgName.toString} not found. $context"
      def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
    }
    case class MissingModule(
        pkgName: PackageName,
        modName: ModuleName,
        context: String = "",
        location: Option[CodeLocation] = None
    ) extends LookupError {
      def message = s"Package ${pkgName.toString} does not contain module ${modName.toString}. $context"

      def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
    }

    case class MissingType(
        pkgName: PackageName,
        modName: ModuleName,
        typeName: Name,
        context: String = "",
        location: Option[CodeLocation] = None
    ) extends LookupError {
      def message =
        (s"Module ${pkgName.toString}:${modName.toString} has no type named ${typeName.toTitleCase}. $context")

      def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
    }
    case class MissingDefinition(
        pkgName: PackageName,
        modName: ModuleName,
        defName: Name,
        context: String = "",
        location: Option[CodeLocation] = None
    ) extends LookupError {
      def message =
        s"Module ${pkgName.toString}:${modName.toString} has no definition named ${defName.toCamelCase}. $context"

      def withContext(newContext: String) = this.copy(context = context + "\n" + newContext)
    }
  }

  sealed trait RTValueToMDMError extends MorphirRuntimeError with AttachedLocation
  object RTValueToMDMError {
    final case class MissingField(value: RTValue.Record, field: Label, location: Option[CodeLocation] = None)
        extends RTValueToMDMError {
      def message = err"Record $value appeared in result without expected field $field"
    }

    final case class ResultTypeMismatch(
        result: RTValue,
        concept: Concept,
        explanation: String,
        location: Option[CodeLocation] = None
    ) extends EvaluationError {
      def message =
        err"""Result $result cannot be matched to type $concept. $explanation.
             (This type was derived from the entry point. These may be nested within broader result/type trees.""".stripMargin
    }
  }

  sealed trait TypeError extends MorphirRuntimeError with AttachedLocation
  object TypeError {

    final case class TypesMismatch(tpe1: UType, tpe2: UType, msg: String, location: Option[CodeLocation] = None)
        extends TypeError {
      def message = (err"$msg: $tpe1 vs $tpe2")
    }
    final case class ApplyToNonFunction(
        applyNode: TypedValue,
        nonFunction: TypedValue,
        arg: TypedValue,
        location: Option[CodeLocation] = None
    ) extends TypeError {
      def message =
        err"$applyNode tried to apply $arg to $nonFunction of type ${nonFunction.attributes}, which is not a function"
    }

    final case class LiteralTypeMismatch(lit: Lit, tpe: UType, location: Option[CodeLocation] = None)
        extends TypeError {
      def message = err"Literal $lit is not of type $tpe"
    }
    final case class ImproperType(tpe: UType, explanation: String, location: Option[CodeLocation] = None)
        extends TypeError {
      def message = (err"Improper Type: $explanation. Found: $tpe")
    }
    final case class ImproperTypeSpec(
        fqn: FQName,
        spec: UTypeSpec,
        explanation: String,
        location: Option[CodeLocation] = None
    ) extends TypeError {
      def message = err"Improper Type Specification found: $explanation. $fqn points to: $spec"
    }
    final case class ImproperTypeDef(
        fqn: FQName,
        defn: UTypeDef,
        explanation: String,
        location: Option[CodeLocation] = None
    ) extends TypeError {
      def message = err"Improper Type Definition found: $explanation. $fqn points to: $defn"
    }
    final case class CannotDealias(
        err: LookupError,
        xplanation: String = "Cannot dealias type",
        location: Option[CodeLocation] = None
    ) extends TypeError {
      def message = err"$xplanation: ${err.message}"
    }
    final case class TypeLacksField(tpe: UType, field: Name, msg: String, location: Option[CodeLocation] = None)
        extends TypeError {
      def message = err"$tpe lacks field <${field.toCamelCase}>. $msg"
    }
    final case class TypeHasExtraField(tpe: UType, contract: UType, field: Name, location: Option[CodeLocation] = None)
        extends TypeError {
      def message = err"$tpe has field <${field.toCamelCase}>, which is not included in $contract"
    }

    final case class ValueLacksField(
        value: TypedValue,
        contract: UType,
        field: Name,
        location: Option[CodeLocation] = None
    ) extends TypeError {
      def message = err"$value lacks field <${field.toCamelCase}>, which is required by $contract"
    }

    final case class ValueHasExtraField(
        value: TypedValue,
        contract: UType,
        field: Name,
        location: Option[CodeLocation] = None
    ) extends TypeError {
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

    class SizeMismatch(first: Int, second: Int, msg: String, val location: Option[CodeLocation] = None)
        extends TypeError {
      def message = err"$msg: ($first vs $second)"
    }

    final case class ArgNumberMismatch(
        first: Int,
        second: Int,
        msg: String,
        override val location: Option[CodeLocation] = None
    ) extends SizeMismatch(first: Int, second: Int, msg: String, location)

    final case class InferenceConflict(older: UType, newer: UType, name: Name, location: Option[CodeLocation] = None)
        extends TypeError {
      def message =
        err"While trying to bind the type variables of the entry point function, the input matched type variable ${name
            .toCamelCase} with $older and then also $newer, which are not the same."
    }

    final case class UnknownTypeMismatch(
        tpe1: UType,
        tpe2: UType,
        hint: String = "",
        location: Option[CodeLocation] = None
    ) extends TypeError {
      def message =
        err"Could not match $tpe1 to $tpe2, but it is unclear why. ${if (hint != "") "Hint: " + hint else ""}"
    }

    final case class UnsupportedType(tpe: UType, hint: String = "", location: Option[CodeLocation] = None)
        extends TypeError {
      def message = err"$tpe is not currently supported.  ${if (hint != "") "Hint: " + hint else ""}"
    }
    final case class OtherTypeError(message: String, location: Option[CodeLocation] = None) extends TypeError

    final case class ManyTypeErrors(errors: List[TypeError])
        extends TypeError {
      val location = None
      def message = ("\n" + errors.map(err =>
        s"""
             |${err.getClass.getName.split(".").lastOption.getOrElse(err.getClass.getName)}:
             |${err.message}
           """
      ).mkString("\n"))
    }
  }
}
