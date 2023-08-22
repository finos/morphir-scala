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
    def withDepth(depth: Int)                            = this.copy(depth = depth)
    def withPrefix(prefix: String)                       = this.copy(prefix = prefix)

    def getTypeVariable(name: Name): Option[UType] = typeBindings.get(name)
  }
  object Context {
    def empty = Context(Map(), 0, "")
  }
  def helper(condition: Boolean, error: MorphirTypeError) = if (condition) List(error) else List()
}

class TypeChecker(dists: Distributions) {
  import TypeChecker.*
  private val functionOnion = new Extractors.Types.FunctionOnion(dists)
  private val dealiased     = new Extractors.Types.Dealiased(dists)
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
  def checkList(argList : List[UType], paramList : List[UType], value : TypedValue, contract : UType, context : Context) = { //Maybe that should be in context?
    if (argList.size != paramList.size)
      new ArgNumberMismatch(argList.size, paramList.size, s"Incorrect arity between ")
  }
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

  def conformsTo(valueType: UType, declaredType: UType): List[MorphirTypeError] =
    conformsTo(valueType, declaredType, Context.empty)
  def conformsTo(valueType: UType, declaredType: UType, context: Context): List[MorphirTypeError] = {
    import Extractors.Types.*
    (valueType, declaredType) match {
      case (_, Type.Variable(_, name)) => context.getTypeVariable(name) match {
          case None           => List(new TypeVariableMissing(name))
          case Some(lookedUp) => conformsTo(valueType, lookedUp, context) // TODO: Type parameter wrangling
        }
      case (Type.Variable(_, name), _) => context.getTypeVariable(name) match {
          case None           => List(new TypeVariableMissing(name))
          case Some(lookedUp) => conformsTo(lookedUp, declaredType, context)
        }
      case (left @ LeafType(), right @ LeafType()) =>
        if (left == right) List() else List(TypesMismatch(left, right, "Value type does not match declared type"))
      case (Type.Function(_, valueArg, valueRet), Type.Function(_, declaredArg, declaredRet)) =>
        // Note reversed order - covariance vs. contravariance.
        conformsTo(valueRet, declaredRet, context) ++ conformsTo(declaredArg, valueArg, context)
      case (value @ Type.Tuple(_, valueElements), declared @ Type.Tuple(_, declaredElements)) =>
        if (valueElements.length != declaredElements.length) {
          List(new TypesMismatch(
            value,
            declared,
            s"Tuple sizes differ (${valueElements.length} vs ${declaredElements.length})"
          ))
        } else {
          valueElements.toList.zip(declaredElements).flatMap {
            case (value, declared) =>
              conformsTo(value, declared, context)
          }
        }
      case (valueTpe @ Type.Record(_, valueFields), declaredTpe @ Type.Record(_, declaredFields)) =>
        val valueFieldSet: Set[Name]    = valueFields.map(_.name).toSet
        val declaredFieldSet: Set[Name] = declaredFields.map(_.name).toSet
        val missingFromValue = valueFieldSet
          .diff(declaredFieldSet).toList.map(missing =>
            TypeLacksField(valueTpe, missing, s"Required by ${Succinct.Type(declaredTpe)}")
          )
        val missingFromDeclared = declaredFieldSet
          .diff(valueFieldSet).toList.map(bonus => TypeHasExtraField(valueTpe, declaredTpe, bonus))
        val sharedFields                       = valueFieldSet.intersect(declaredFieldSet)
        val valueFieldMap: Map[Name, UType]    = valueFields.map(field => field.name -> field.data).toMap
        val declaredFieldMap: Map[Name, UType] = declaredFields.map(field => field.name -> field.data).toMap
        val conflicts =
          sharedFields.flatMap(field => conformsTo(valueFieldMap(field), declaredFieldMap(field), context))
        missingFromValue ++ missingFromDeclared ++ conflicts
      // TODO: Consider covariance/contravariance
      case (DictRef(valueKey, valueValue), DictRef(declaredKey, declaredValue)) =>
        conformsTo(valueKey, declaredKey, context) ++ conformsTo(valueValue, declaredValue, context)
      case (ResultRef(valueErr, valueOk), ResultRef(declaredErr, declaredOk)) =>
        conformsTo(valueErr, declaredErr, context) ++ conformsTo(valueOk, declaredOk, context)
      case (ListRef(valueElement), ListRef(declaredElement)) => conformsTo(valueElement, declaredElement, context)
      case (MaybeRef(valueElement), MaybeRef(declaredElement)) =>
        conformsTo(valueElement, declaredElement, context)
      case (Type.Reference(_, valueName, valueArgs), Type.Reference(_, declaredName, declaredArgs))
          if valueName == declaredName =>
        if (valueArgs.length != declaredArgs.length)
          List(new OtherTypeError(
            s"Reference $valueName has different number of parameters (${valueArgs.length} vs ${declaredArgs.length}"
          ))
        else
          valueArgs.zip(declaredArgs).toList.flatMap { case (value, declared) => conformsTo(value, declared, context) }
      case (dealiased(value, valueArgs), declared) =>
        conformsTo(value, declared, context) // TODO: Bindings, left side only!
      case (value, dealiased(declared, declaredArgs)) =>
        conformsTo(value, declared, context) // TODO: Bindings, right side only!
      case (valueOther, declaredOther) if valueOther.getClass == declaredOther.getClass =>
        List(
          new Unimplemented(s"No matching support for ${Succinct.Type(valueOther)} vs ${Succinct.Type(declaredOther)}")
        )
      case (valueOther, declaredOther) => List(new TypesMismatch(valueOther, declaredOther, "Different types"))
    }
  }

  def check(suspect: TypedValue): TypeCheckerResult =
    check(suspect, Context.empty)
  def check(suspect: TypedValue, parentContext: Context): TypeCheckerResult = {
    import Value.{Unit as UnitValue, List as ListValue, Field as FieldValue, *}
    val context = parentContext.withDepth(parentContext.depth + 1)
    dealias(suspect.attributes, context) match {
      case Right(tpe) => suspect match {
          case Literal(_, lit)              => handleLiteral(tpe, lit, context)
          case Apply(_, function, argument) => handleApply(tpe, function, argument, context)
          case Destructure(_, pattern, valueToDestruct, inValue) =>
            handleDestructure(tpe, pattern, valueToDestruct, inValue, context)
          case Constructor(_, name)             => handleConstructor(tpe, name, context)
          case FieldValue(_, recordValue, name) => handleFieldValue(tpe, recordValue, name, context)
          case FieldFunction(_, name)           => handleFieldFunction(tpe, name, context)
          case IfThenElse(_, condition, thenValue, elseValue) =>
            handleIfThenElse(tpe, condition, thenValue, elseValue, context)
          case Lambda(_, pattern, body) => handleLambda(tpe, pattern, body, context)
          case LetDefinition(_, name, definition, inValue) =>
            handleLetDefinition(tpe, name, definition, inValue, context)
          case LetRecursion(_, definitions, inValue)  => handleLetRecursion(tpe, definitions, inValue, context)
          case ListValue(_, elements)                 => handleListValue(tpe, elements.toList, context)
          case PatternMatch(_, value, cases)          => handlePatternMatch(tpe, value, cases.toList, context)
          case recordValue @ Record(_, fields)        => handleRecord(tpe, fields.toList, recordValue, context)
          case Reference(_, name)                     => handleReference(tpe, name, context)
          case Tuple(_, elements)                     => handleTuple(tpe, elements.toList, context)
          case UnitValue(_)                           => handleUnitValue(tpe, context)
          case UpdateRecord(_, valueToUpdate, fields) => handleUpdateRecord(tpe, valueToUpdate, fields, context)
          case Variable(_, name)                      => handleVariable(tpe, name, context)
        }
      case Left(err) => List(err)
    }
  }
  def handleLiteral(tpe: UType, literal: Lit, context: Context): TypeCheckerResult = {
    import Extractors.Types.*
    import Lit.*
    val fromChildren = List()
    val matchErrors = (literal, tpe) match {
      case (StringLiteral(_), StringRef())   => List()
      case (FloatLiteral(_), FloatRef())     => List()
      case (CharLiteral(_), CharRef())       => List()
      case (BoolLiteral(_), BoolRef())       => List()
      case (WholeNumberLiteral(_), IntRef()) => List() // TODO: "WholeNumberRef" extractor
      case (DecimalLiteral(_), DecimalRef()) => List()
      case (otherLit, otherTpe)              => List(new LiteralTypeMismatch(otherLit, otherTpe))
    }
    fromChildren ++ matchErrors
  }

  def handleApply(tpe: UType, function: TypedValue, argument: TypedValue, context: Context): TypeCheckerResult = {
    val fromChildren = check(function, context) ++ check(argument, context)
    val fromTpe =
      dealias(function.attributes, context) match {
        case Right(Type.Function(_, paramType, returnType)) =>
          conformsTo(argument.attributes, paramType, context) ++ conformsTo(
            returnType,
            tpe,
            context
          ) // TODO: Useful context lost here
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
    val (ret, args)  = Utils.uncurryFunctionType(tpe) // TODO: Interleaved function type w/ aliases.
    val fromTpe = ret match {
      case Type.Reference(_, name, typeArgs) => dists.lookupTypeSpecification(name) match {
          case Right(T.Specification.CustomTypeSpecification(typeParams, ctors)) =>
            val missedName = helper(
              fqn.packagePath != name.packagePath || fqn.modulePath != name.modulePath,
              new OtherTypeError(s"Constructor $fqn does not match type name $name")
            )
            val fromCtor = ctors.toMap.get(fqn.localName) match {
              case Some(ctorArgs) => List()//helper(args.len != ctorArgs.len, new )

              case None =>
                List(new OtherTypeError(s"Constructor type $name exists, but does not have arm for ${fqn.localName}"))
            }
            missedName ++ fromCtor
          // TODO: Bindings
          case Right(other) =>
            List(new ImproperTypeSpec(name, other, s"Type union expected"))
          case Left(err) => List(new TypeMissing(err))
        }
      case other => List(new ImproperType(other, s"Reference to type union expected"))
    }
    fromChildren
  }
  def handleFieldValue(tpe: UType, recordValue: TypedValue, name: Name, context: Context): TypeCheckerResult = {
    val fromChildren = check(recordValue, context)
    val fromTpe = recordValue.attributes match {
      case Type.Record(_, fields) =>
        fields.map(field => field.name -> field.data).toMap.get(name) match {
          case None           => List(new TypeLacksField(tpe, name, "Referenced by field none"))
          case Some(fieldTpe) => conformsTo(fieldTpe, tpe, context)
        }
    }
    fromChildren ++ fromTpe
  }
  def handleFieldFunction(tpe: UType, name: Name, context: Context): TypeCheckerResult = {
    val fromChildren = List()
    // TODO: tpe should be... function from extensible record type to ???
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
    val internal = conformsTo(thenValue.attributes, tpe, context) ++ conformsTo(
      elseValue.attributes,
      tpe,
      context
    ) ++ conformsTo(condition.attributes, Basics.boolType, context)
    fromChildren ++ internal
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
    // We expect declared element types to be the same, but values may differ; so we only grab the first set of errors at the type level, but fully explore all elements.
    val fromChildren = elements.flatMap(check(_, context))
    val fromTpe = tpe match {
      case Extractors.Types.ListRef(elementType) =>
        elements.foldLeft(List(): List[MorphirTypeError]) { (acc, next) =>
          acc ++ conformsTo(elementType, next.attributes, context)
          if (acc.size != 0) acc
          else {
            conformsTo(
              elementType,
              next.attributes,
              context
            ) // Check each element vs. the declared element type (only keep first errors)
          }
        }
      case other => List(ImproperType(other, s"Expected list"))
    }
    fromChildren ++ fromTpe
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
  def handleRecord(
      tpe: UType,
      valFields: List[(Name, TypedValue)],
      value: Value.Record[Unit, UType],
      context: Context
  ): TypeCheckerResult = {
    val fromChildren = valFields.flatMap { case (_, value) => check(value, context) }

    val fromTpe = tpe match {
      case recordTpe @ Type.Record(_, tpeFields) =>
        val tpeFieldSet: Set[Name] = tpeFields.map(_.name).toSet
        val valFieldSet: Set[Name] = valFields.map(_._1).toSet
        val missingFromTpe = tpeFieldSet
          .diff(valFieldSet).toList.map(missing => ValueLacksField(value, recordTpe, missing))
        val missingFromValue = valFieldSet
          .diff(tpeFieldSet).toList.map(bonus => ValueHasExtraField(value, recordTpe, bonus))
        val valFieldMap: Map[Name, TypedValue] = valFields.toMap
        val tpeFieldMap: Map[Name, UType]      = tpeFields.map(field => field.name -> field.data).toMap
        val conflicts = tpeFieldSet.intersect(valFieldSet).flatMap(name =>
          conformsTo(valFieldMap(name).attributes, tpeFieldMap(name), context)
        )
        missingFromTpe ++ missingFromValue ++ conflicts
      case other => List(ImproperType(other, "Value is of type Record"))
    }
    fromChildren ++ fromTpe
  }
  def handleReference(tpe: UType, fqn: FQName, context: Context): TypeCheckerResult = {
    val fromChildren = List()
    val fromType = if (!Utils.isNative(fqn)) {
      dists.lookupValueSpecification(fqn) match {
        case Left(err) => List(new DefinitionMissing(err))
        case Right(spec) =>
          val curried = Utils.curryTypeFunction(spec.output, spec.inputs)
          conformsTo(curried, tpe, context)
      }
    } else List() // TODO: Handle native type references
    fromChildren ++ fromType
  }
  def handleTuple(tpe: UType, elements: List[TypedValue], context: Context): TypeCheckerResult = {
    val fromTpe = tpe match {
      case tupleTpe @ Type.Tuple(_, elementTypes) =>
        helper(
          elementTypes.length != elements.length,
          new OtherTypeError(
            s"Value typed as tuple with ${elementTypes.length} elements has ${elements.length} elements."
          )
        ) ++
          (elements.map(_.attributes)).zip(elementTypes).flatMap { case (actual, declared) =>
            conformsTo(actual, declared, context)
          }
      case other => List(new ImproperType(other, "Tuple expected"))
    }
    val fromChildren = elements.flatMap(check(_, context))
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
