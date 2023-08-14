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
import zio.Chunk

object TypeChecker {
  type TypeCheckerResult = List[GoodTypeError]
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
  def helper(condition: Boolean, error: GoodTypeError) = if (condition) List(error) else List()
}

class TypeChecker(dists: Distributions) {
  private val functionOnion                                                               = FunctionOnion(dists)
  private def nameThatMismatch(tpe1: UType, tpe2: UType): String                          = {}
  private def nameMissingValue(value: TypedValue, dists: Distributions): MorphirTypeError = {}
  private def nameMissingType(fqn: FQName, dists: Distributions): MorphirTypeError        = {}
  private def nameMissingConstructor(fqn: FQName, tpe: UType, dists: Distributions): MorphirTypeError = {}
  private def pretty(tpe: UType, depthBudget: Int): String                                            = {}
  private def pretty(tpe: UType): String                                                              = pretty(tpe, 2)
  def check(suspect: TypedValue): TypeCheckerResult =
    check(suspect, Context.empty)
  def check(suspect: TypedValue, parentContext: Context) = {
    import Value.{Unit as UnitValue, List as ListValue, Field as FieldValue, *}
    val context = parentContext.withDepth(parentContext.depth + 1)
    suspect match{
      case Literal(tpe, lit)              => handleLiteral(tpe, lit, context)
      case Apply(tpe, function, argument) => handleApply(tpe, function, argument, store, context)
      case Destructure(tpe, pattern, valueToDestruct, inValue) =>
        handleDestructure(tpe, pattern, valueToDestruct, inValue, store, context)
      case Constructor(tpe, name)        => handleConstructor(tpe, name, store, context)
      case FieldValue(tpe, recordValue, name) => handleField(tpe, recordValue, name, store, context)
      case FieldFunction(tpe, name)      => handleFieldFunction(tpe, name, context)
      case IfThenElse(tpe, condition, thenValue, elseValue) =>
        handleIfThenElse(tpe, condition, thenValue, elseValue, store, context)
      case Lambda(tpe, pattern, body) => handleLambda(tpe, pattern, body, store, context)
      case LetDefinition(tpe, name, definition, inValue) =>
        handleLetDefinition(tpe, name, definition, inValue, store, context)
      case LetRecursion(tpe, definitions, inValue)  => handleLetRecursion(tpe, definitions, inValue, store, context)
      case ListValue(tpe, elements)                 => handleListValue(tpe, elements.toList, store, context)
      case PatternMatch(tpe, value, cases)          => handlePatternMatch(tpe, value, cases.toList, store, context)
      case Record(tpe, fields)                      => handleRecord(tpe, fields.toList, store, context)
      case Reference(tpe, name)                     => handleReference(tpe, name, store, context)
      case Tuple(tpe, elements)                     => handleTuple(tpe, elements.toList, store, context)
      case UnitValue(va)                                => handleUnit(va, context)
      case UpdateRecord(tpe, valueToUpdate, fields) => handleUpdateRecord(tpe, valueToUpdate, fields, store, context)
      case Variable(tpe, name)                      => handleVariable(tpe, name, store)
    }
    def handleLiteral(tpe : UType, function: TypedValue, argument : TypedValue, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleApply(tpe : UType, function: TypedValue, argument : TypedValue, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleDestructure(tpe : UType, function: TypedValue, argument : TypedValue, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleConstructor(tpe : UType, function: TypedValue, argument : TypedValue, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleFieldValue(tpe : UType, recordValue : TypedValue, name : Name, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleFieldFunction(tpe : UType, name : Name, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleIfThenElse(tpe : UType, condition : TypedValue, thenValue : TypedValue, elseValue : TypedValue, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleLambda(tpe : UType, pattern, body : TypedValue, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleLetDefinition(tpe : UType, name : Name, definition, inValue : TypedValue, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleLetRecursion(tpe : UType, definitions, inValue : TypedValue, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleListValue(tpe : UType, elements, context : Context) : TypeCheckerResult = {
        List()
    }
    def handlePatternMatch(tpe : UType, value : TypedValue, cases, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleRecord(tpe : UType, fields, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleReference(tpe : UType, fqn : FQName, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleTuple(tpe : UType, elements, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleUnitValue(tpe : UType, context : Context) : TypeCheckerResult = {
        List()
    }
    def handlepdateRecord(tpe : UType, valueToUpdate, fields, context : Context) : TypeCheckerResult = {
        List()
    }
    def handleVariable(tpe : UType, name : Name, context : Context) : TypeCheckerResult = {
        List()
    }
  }

}
