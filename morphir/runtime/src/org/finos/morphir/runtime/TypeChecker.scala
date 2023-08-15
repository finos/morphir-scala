package org.finos.morphir.runtime

import org.finos.morphir.naming.*
import org.finos.morphir.naming.*
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.{
  Pattern,
  TypedValue,
  Value,
  TypedDefinition as TypedValueDef,
  USpecification as UValueSpec
}
import org.finos.morphir.ir.Type.{Type, UType, USpecification as UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.Field
import org.finos.morphir.runtime.MorphirTypeError.CannotDealias
import org.finos.morphir.runtime.exports.*
import MorphirTypeError.*

object TypeChecker {
  type TypeCheckerResult = List[MorphirTypeError]
  case class Context(
      typeBindings: Map[Name, UType],
      depth: Int,
      prefix: String
  ) {
    def withTypeBindings(typeBindings: Map[Name, UType]) = this.copy(typeBindings = typeBindings)
    def withDepth(depth: Int)                    = this.copy(depth = depth)
    def withPrefix(prefix: String)               = this.copy(prefix = prefix)

    def getTypeVariable(name: Name): Option[UType] = typeBindings.get(name)
  }
  object Context {
    def empty = Context(Map(), 0, "")
  }
  def helper(condition: Boolean, error: MorphirTypeError) = if (condition) List(error) else List()
}

class TypeChecker(dists: Distributions) {
  import TypeChecker.*
  private val functionOnion = Extractors.Types.FunctionOnion(dists)
  private def nameThatMismatch(tpe1: UType, tpe2: UType): String = {
    import Extractors.Types.*
    (tpe1, tpe2) match {
      case (NonNativeRef(fqn1, args1), NonNativeRef(fqn2, args2)) if fqn1 == fqn2 =>
        s"Refs to $fqn1 have different type args"
      case (NonNativeRef(fqn1, _), NonNativeRef(fqn2, _)) =>
        val (pack1, mod1, loc1) = (fqn1.packagePath, fqn1.modulePath, fqn1.localName)
        val (pack2, mod2, loc2) = (fqn2.packagePath, fqn2.modulePath, fqn2.localName)
        val packPart            = if (pack1 != pack2) s"{$pack1 </=/> $pack2}" else pack1
        val modPart             = if (mod1 != mod2) s"{$mod1 </=/> $mod2}" else mod1
        val locPart = if (loc1 != loc2) s"{${loc1.toTitleCase} </=/> ${loc2.toTitleCase}" else loc1.toTitleCase
        s"$packPart:$modPart:$locPart"
      case _ => s"(${Succinct.Type(tpe1, 2)} vs ${Succinct.Type(tpe2, 2)})"
    }
  }
  private def nameMissingValue(value: TypedValue, dists: Distributions): MorphirTypeError             = ???
  private def nameMissingType(fqn: FQName, dists: Distributions): MorphirTypeError                    = ???
  private def nameMissingConstructor(fqn: FQName, tpe: UType, dists: Distributions): MorphirTypeError = ???

  def dealias(tpe: UType, context: Context): Either[MorphirTypeError, UType] = {
    def loop(tpe: UType, original_fqn: Option[FQName], context: Context): Either[MorphirTypeError, UType] =
      tpe match {
        case ref @ Extractors.Types.NativeRef() => Right(ref) // TODO: Bindings
        case Type.Reference(_, typeName, typeArgs) =>
          val lookedUp = dists.lookupTypeSpecification(typeName.packagePath, typeName.modulePath, typeName.localName)
          lookedUp match {
            case Right(T.Specification.TypeAliasSpecification(typeParams, expr)) =>
              val newBindings = typeParams.zip(typeArgs).toMap
              loop(expr, original_fqn.orElse(Some(typeName)), context.withTypeBindings(newBindings))
            case Right(_) => Right(tpe) // TODO: Bindings
            case Left(lookupErr) =>
              original_fqn match {
                case Some(original) =>
                  Left(new CannotDealias(lookupErr, s"Unable to dealias (nested beneath $original"))
                case None =>
                  Left(new CannotDealias(lookupErr))
              }
          }
        case other => Right(other) // TODO: Bindings
      }
    loop(tpe, None, context)
  }

  def conformsTo(valueType: UType, declaredType: UType): List[MorphirTypeError] = conformsTo(valueType, declaredType, Context.empty)
  def conformsTo(valueType : UType, declaredType : UType, context : Context) : List[MorphirTypeError] = {
    import Extractors.Types.*
    (valueType, declaredType) match {
      case (_, Type.Variable(_, name)) => context.getTypeVariable(name) match {
        case None => List(new TypeVariableMissing(name))
        case Some(lookedUp) => conformsTo(valueType, lookedUp, context) //TODO: Type parameter wrangling
      }
      case (Type.Variable(_, name), _) => context.getTypeVariable(name) match {
        case None => List(new TypeVariableMissing(name))
        case Some(lookedUp) => conformsTo(lookedUp, declaredType, context)
      }
      case (left @LeafType(), right @ LeafType()) => {
        if (left == right) List() else List(TypesMismatch(left, right, "Value type does not match declared type"))
      }
      case (Type.Function(_, valueArg, valueRet), Type.Function(_, declaredArg, declaredRet)) => {
          //Note reversed order - covariance vs. contravariance.
          conformsTo(valueRet, declaredRet, context) ++ conformsTo(declaredArg, valueArg, context)
        }

    }
  }

  def check(suspect: TypedValue): TypeCheckerResult =
    check(suspect, Context.empty)
  def check(suspect: TypedValue, parentContext: Context): TypeCheckerResult = {
    import Value.{Unit as UnitValue, List as ListValue, Field as FieldValue, *}
    val context = parentContext.withDepth(parentContext.depth + 1)
    dealias(suspect.attributes, context) match{
      case Right(tpe) => suspect match {
        case Literal(_, lit) => handleLiteral(tpe, lit, context)
        case Apply(_, function, argument) => handleApply(tpe, function, argument, context)
        case Destructure(_, pattern, valueToDestruct, inValue) =>
          handleDestructure(tpe, pattern, valueToDestruct, inValue, context)
        case Constructor(_, name) => handleConstructor(tpe, name, context)
        case FieldValue(_, recordValue, name) => handleFieldValue(tpe, recordValue, name, context)
        case FieldFunction(_, name) => handleFieldFunction(tpe, name, context)
        case IfThenElse(_, condition, thenValue, elseValue) =>
          handleIfThenElse(tpe, condition, thenValue, elseValue, context)
        case Lambda(_, pattern, body) => handleLambda(tpe, pattern, body, context)
        case LetDefinition(_, name, definition, inValue) =>
          handleLetDefinition(tpe, name, definition, inValue, context)
        case LetRecursion(_, definitions, inValue) => handleLetRecursion(tpe, definitions, inValue, context)
        case ListValue(_, elements) => handleListValue(tpe, elements.toList, context)
        case PatternMatch(_, value, cases) => handlePatternMatch(tpe, value, cases.toList, context)
        case Record(_, fields) => handleRecord(tpe, fields.toList, context)
        case Reference(_, name) => handleReference(tpe, name, context)
        case Tuple(_, elements) => handleTuple(tpe, elements.toList, context)
        case UnitValue(_) => handleUnitValue(tpe, context)
        case UpdateRecord(_, valueToUpdate, fields) => handleUpdateRecord(tpe, valueToUpdate, fields, context)
        case Variable(_, name) => handleVariable(tpe, name, context)
      }
      case Left(err) => List(err)
    }
  }
  def handleLiteral(tpe: UType, literal: Lit, context: Context): TypeCheckerResult = {
    import Extractors.Types.*
    import Lit.*
    val fromChildren = List()
    val matchErrors = (literal, tpe) match {
      case (StringLiteral (_), StringRef () ) => List ()
      case (FloatLiteral (_), FloatRef () ) => List ()
      case (CharLiteral (_), CharRef () ) => List ()
      case (BoolLiteral (_), BoolRef () ) => List ()
      case (WholeNumberLiteral (_), IntRef () ) => List () //TODO: "WholeNumberRef" extractor
      case (DecimalLiteral (_), DecimalRef () ) => List ()
      case (otherLit, otherTpe) => List (new LiteralTypeMismatch(otherLit, otherTpe))
    }
    fromChildren ++ matchErrors
  }

  def handleApply(tpe: UType, function: TypedValue, argument: TypedValue, context: Context): TypeCheckerResult = {
    val fromChildren = check(function, context) ++ check(argument, context)
    val fromTpe =
      dealias(function.attributes, context) match {
        case Right(Type.Function(_, paramType, returnType)) =>
          conformsTo(argument.attributes, paramType, context) ++ conformsTo(returnType, tpe, context) //TODO: Useful context lost here
        case Right(other) => List(new ApplyToNonFunction(function, argument))
        case Left(err)    => List(err)
      }
    fromChildren ++ fromTpe
  }

  def handleDestructure(
      tpe: UType,
      pattern: Pattern[UType],
      value: TypedValue,
      inValue: TypedValue,
      context: Context
  ): TypeCheckerResult = {
    val fromChildren = check(value, context) ++ check(inValue, context)
    // TODO: Check pattern can be value
    // TODO: Check value must be pattern
    val fromTpe = conformsTo(inValue.attributes, tpe, context)
    fromTpe ++ fromChildren
  }
  def handleConstructor(tpe: UType, fqn: FQName, context: Context): TypeCheckerResult = {
    val fromChildren = List()
    val (ret, args) = Utils.uncurryFunctionType(tpe) //TODO: Interleaved function type w/ aliases.
    val fromTpe = ret match{
      case Type.Referene(_, name, typeArgs) => dists.lookupTypeSpecification(name) match {
        case Some(T.Specification.CustomTypeSpecification(typeParams, ctors)) =>
          // (typeArgs.zip(typeParams)).flatMap{case (arg, param) => checkTypesAgree(arg, param, context)}
          List()
        // TODO: Constructor specific checks
        case Some(other) =>
          List(new ImproperTypeSpec(other, s"${pretty(tpe, 2)} should have produced a type union"))
        case None => List(new ConstructorMissing(fqn, wrapped, dists))
      }
    }
    // TODO: Check it's a function onion for a type with that constructor
    fromChildren
  }
  def handleFieldValue(tpe: UType, recordValue: TypedValue, name: Name, context: Context): TypeCheckerResult = {
    val fromChildren = check(recordValue, context)
    // TODO: Check the value dealiases to a record which has that name
    fromChildren
  }
  def handleFieldFunction(tpe: UType, name: Name, context: Context): TypeCheckerResult = {
    val fromChildren = List()
    // TODO: Uh... Nothing.
    fromChildren
  }
  def handleIfThenElse(
      tpe: UType,
      condition: TypedValue,
      thenValue: TypedValue,
      elseValue: TypedValue,
      context: Context
  ): TypeCheckerResult = {
    val fromChildren = check(condition, context) ++ check(thenValue, context) ++ check(elseValue, context)
    // TODO: Check condition is boolean and branches agree withe ach other/tpe
    fromChildren
  }
  def handleLambda(tpe: UType, pattern: Pattern[UType], body: TypedValue, context: Context): TypeCheckerResult = {
    val fromChildren = check(body, context)
    // TODO: Check tpe is a function
    // TODO: Check tpe's argument matches (strictly) with pattern
    // TODO: Figure out variable bindings
    fromChildren
  }
  def handleLetDefinition(
      tpe: UType,
      name: Name,
      definition: TypedValueDef,
      inValue: TypedValue,
      context: Context
  ): TypeCheckerResult = {
    val fromChildren = check(inValue, context)
    // TODO: Manage Store
    // TODO: Check definition body
    // TODO: Check definition body w/ argument types added to store
    fromChildren
  }
  def handleLetRecursion(
      tpe: UType,
      definitions: Map[Name, TypedValueDef],
      inValue: TypedValue,
      context: Context
  ): TypeCheckerResult = {
    val fromChildren = check(inValue, context)
    // TODO: Manage store
    // TODO: Check definition types and add to stores
    fromChildren
  }
  def handleListValue(tpe: UType, elements: List[TypedValue], context: Context): TypeCheckerResult = {
    val fromChildren = elements.flatMap(check(_, context))
    // TODO: Check tpe is a list, check children types agree w/ parent type (probably only report one mismatch, but inspect all values
    fromChildren
  }
  def handlePatternMatch(
      tpe: UType,
      value: TypedValue,
      cases: List[(Pattern[UType], TypedValue)],
      context: Context
  ): TypeCheckerResult = {
    val fromChildren = check(value, context)
    // TODO: Check values from each case
    // TODO: Manage store
    // TODO: Check each case's pattern can be it's value
    // TODO: Check value must be one of the patterns
    fromChildren
  }
  def handleRecord(tpe: UType, fields: List[(Name, TypedValue)], context: Context): TypeCheckerResult = {
    val fromChildren = fields.flatMap { case (_, value) => check(value, context) }
    // TODO: Check tpe dealises to a record
    // TODO: Check each field agrees with the type from the name
    fromChildren
  }
  def handleReference(tpe: UType, fqn: FQName, context: Context): TypeCheckerResult = {
    val fromChildren = List()
    val fromType = dists.lookupValueSpecification(fqn) match{
      case Left(err) => List(new DefinitionMissing(err))
      case Right(spec) => {
        val curried = Utils.curryTypeFunction(spec.output, spec.inputs)
        conformsTo(curried, tpe, context)
      }
    }
    fromChildren ++ fromType
  }
  def handleTuple(tpe: UType, elements: List[TypedValue], context: Context): TypeCheckerResult = {
    val fromChildren = elements.flatMap(check(_, context))
    // TODO: Check tpe dealiases to a tuple
    // TODO: Check tuple types vs. nested value types
    fromChildren
  }
  def handleUnitValue(tpe: UType, context: Context): TypeCheckerResult =
    List() // Pass
  def handleUpdateRecord(
      tpe: UType,
      valueToUpdate: TypedValue,
      fields: Map[Name, TypedValue],
      context: Context
  ): TypeCheckerResult = {
    val fromChildren = check(valueToUpdate, context) ++ fields.flatMap { case (_, value) => check(value, context) }
    // TODO: Check the value dealiases to a record which has that name
    fromChildren
  }
  def handleVariable(tpe: UType, name: Name, context: Context): TypeCheckerResult =
    // TODO: Keep that in the context
    // TODONT: Do not re-check the body - only make sure that the tpe matches this
    List()

}
