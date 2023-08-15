package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec, TypedDefinition => TypedValueDef}
import org.finos.morphir.ir.Type.{Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.Field
import org.finos.morphir.runtime.exports.*
import zio.Chunk

object TypeChecker {
  type TypeCheckerResult = List[MorphirTypeError]
  case class Context(
      bindings: Map[Name, UType],
      depth: Int,
      prefix: String
  ) {
    def withBindings(bindings: Map[Name, UType]) = this.copy(bindings = bindings)
    def withDepth(depth: Int)                    = this.copy(depth = depth)
    def withPrefix(prefix: String)               = this.copy(prefix = prefix)
  }
  object Context {
    def empty = Context(Map(), 0, "")
  }
  def helper(condition: Boolean, error: MorphirTypeError) = if (condition) List(error) else List()
}

class TypeChecker(dists: Distributions) {
  import TypeChecker.*
  private val functionOnion                                                               = Extractors.Types.FunctionOnion(dists)
  private def nameThatMismatch(tpe1: UType, tpe2: UType): String                          = {
    import Extractors.Types.*
    (tpe1, tpe2) match {
      case (NonNativeRef(fqn1, args1), NonNativeRef(fqn2, args2)) if fqn1 == fqn2 =>
        s"Refs to $fqn1 have different type args"
      case (NonNativeRef(fqn1, _), NonNativeRef(fqn2, _)) =>
        val (pack1, mod1, loc1) = (fqn1.packagePath, fqn1.modulePath, fqn1.localName)
        val (pack2, mod2, loc2) = (fqn2.packagePath, fqn2.modulePath, fqn2.localName)
        val packPart = if (pack1 != pack2) s"{$pack1 </=/> $pack2}" else pack1
        val modPart = if (mod1 != mod2) s"{$mod1 </=/> $mod2}" else mod1
        val locPart = if (loc1 != loc2) s"{${loc1.toTitleCase} </=/> ${loc2.toTitleCase}" else loc1.toTitleCase
        s"$packPart:$modPart:$locPart"
      case _ => s"(${pretty(tpe1, 2)} vs ${pretty(tpe2, 2)})"
    }
  }
  private def nameMissingValue(value: TypedValue, dists: Distributions): MorphirTypeError = {???}
  private def nameMissingType(fqn: FQName, dists: Distributions): MorphirTypeError        = {???}
  private def nameMissingConstructor(fqn: FQName, tpe: UType, dists: Distributions): MorphirTypeError = {???}
  private def pretty(tpe: UType, depthBudget: Int): String                                            = {???}
  private def pretty(tpe: UType): String                                                              = pretty(tpe, 2)
  def check(suspect: TypedValue): TypeCheckerResult =
    check(suspect, Context.empty)
  def check(suspect: TypedValue, parentContext: Context) = {
    import Value.{Unit as UnitValue, List as ListValue, Field as FieldValue, *}
    val context = parentContext.withDepth(parentContext.depth + 1)
    suspect match {
      case Literal(tpe, lit)              => handleLiteral(tpe, lit, context)
      case Apply(tpe, function, argument) => handleApply(tpe, function, argument, context)
      case Destructure(tpe, pattern, valueToDestruct, inValue) =>
        handleDestructure(tpe, pattern, valueToDestruct, inValue, context)
      case Constructor(tpe, name)             => handleConstructor(tpe, name, context)
      case FieldValue(tpe, recordValue, name) => handleFieldValue(tpe, recordValue, name, context)
      case FieldFunction(tpe, name)           => handleFieldFunction(tpe, name, context)
      case IfThenElse(tpe, condition, thenValue, elseValue) =>
        handleIfThenElse(tpe, condition, thenValue, elseValue, context)
      case Lambda(tpe, pattern, body) => handleLambda(tpe, pattern, body, context)
      case LetDefinition(tpe, name, definition, inValue) =>
        handleLetDefinition(tpe, name, definition, inValue, context)
      case LetRecursion(tpe, definitions, inValue)  => handleLetRecursion(tpe, definitions, inValue, context)
      case ListValue(tpe, elements)                 => handleListValue(tpe, elements.toList, context)
      case PatternMatch(tpe, value, cases)          => handlePatternMatch(tpe, value, cases.toList, context)
      case Record(tpe, fields)                      => handleRecord(tpe, fields.toList, context)
      case Reference(tpe, name)                     => handleReference(tpe, name, context)
      case Tuple(tpe, elements)                     => handleTuple(tpe, elements.toList, context)
      case UnitValue(va)                            => handleUnitValue(va, context)
      case UpdateRecord(tpe, valueToUpdate, fields) => handleUpdateRecord(tpe, valueToUpdate, fields, context)
      case Variable(tpe, name)                      => handleVariable(tpe, name, store)
    }
    def handleLiteral(tpe: UType, literal : Lit, context: Context): TypeCheckerResult =
      List()
    def handleApply(tpe: UType, function: TypedValue, argument: TypedValue, context: Context): TypeCheckerResult =
      List()
    def handleDestructure(tpe: UType, pattern: Pattern[UType], value : TypedValue, inValue : TypedValue, context: Context): TypeCheckerResult =
      List()
    def handleConstructor(tpe: UType, fqn : FQName, context: Context): TypeCheckerResult =
      List()
    def handleFieldValue(tpe: UType, recordValue: TypedValue, name: Name, context: Context): TypeCheckerResult =
      List()
    def handleFieldFunction(tpe: UType, name: Name, context: Context): TypeCheckerResult =
      List()
    def handleIfThenElse(
        tpe: UType,
        condition: TypedValue,
        thenValue: TypedValue,
        elseValue: TypedValue,
        context: Context
    ): TypeCheckerResult =
      List()
    def handleLambda(tpe: UType, pattern: Pattern[UType], body: TypedValue, context: Context): TypeCheckerResult =
      List()
    def handleLetDefinition(
        tpe: UType,
        name: Name,
        definition: TypedValueDef,
        inValue: TypedValue,
        context: Context
    ): TypeCheckerResult =
      List()
    def handleLetRecursion(
        tpe: UType,
        definitions: Map[Name, TypedValueDef],
        inValue: TypedValue,
        context: Context
    ): TypeCheckerResult =
      List()
    def handleListValue(tpe: UType, elements: List[TypedValue], context: Context): TypeCheckerResult =
      List()
    def handlePatternMatch(
        tpe: UType,
        value: TypedValue,
        cases: List[(Pattern[UType], TypedValue)],
        context: Context
    ): TypeCheckerResult =
      List()
    def handleRecord(tpe: UType, fields: List[(Name, TypedValue)], context: Context): TypeCheckerResult =
      List()
    def handleReference(tpe: UType, fqn: FQName, context: Context): TypeCheckerResult =
      List()
    def handleTuple(tpe: UType, elements: List[TypedValue], context: Context): TypeCheckerResult =
      List()
    def handleUnitValue(tpe: UType, context: Context): TypeCheckerResult =
      List()
    def handleUpdateRecord(
        tpe: UType,
        valueToUpdate: Name,
        fields: Map[Name, TypedValue],
        context: Context
    ): TypeCheckerResult =
      List()
    def handleVariable(tpe: UType, name: Name, context: Context): TypeCheckerResult =
      List()
  }

}
