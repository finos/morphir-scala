package org.finos.morphir.runtime

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
import org.finos.morphir.ir.Type.{Field, Type, UType, USpecification as UTypeSpec}
import org.finos.morphir.ir.AccessControlled
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.printing.{DetailLevel, PrintIR}
import org.finos.morphir.runtime.quick.GatherReferences
import zio.Chunk
import org.finos.morphir.runtime.MorphirRuntimeError.*
import TypeError.*

object TypeChecker {
  type TypeCheckerResult = List[TypeError]
  // Object to carry data for making tracking local type bindings and context information for better error messages
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
  def helper(condition: Boolean, error: TypeError) = if (condition) List(error) else List()
}

//TODO: This is final because references to ValueDefinition are private, and thus letDefinition and letRecursion handlers cannot be overridden. There may be a better way to handle this.
final class TypeChecker(dists: Distributions, location: Option[CodeLocation] = None) {
  import TypeChecker.*
  private val dealiased = new Extractors.Types.Dealiased(dists)
  // TODO: Use or remove
  // String utility to improve visibility of names when types don't match
  def nameThatMismatch(tpe1: UType, tpe2: UType): String = {
    import Extractors.Types.*
    (tpe1, tpe2) match {
      case (NonNativeRef(fqn1, _), NonNativeRef(fqn2, _)) if fqn1 == fqn2 =>
        s"Refs to $fqn1 have different type args" // This method shouldn't be called otehrwise
      case (NonNativeRef(fqn1, _), NonNativeRef(fqn2, _)) =>
        val (pack1, mod1, loc1) = (fqn1.packagePath, fqn1.modulePath, fqn1.localName)
        val (pack2, mod2, loc2) = (fqn2.packagePath, fqn2.modulePath, fqn2.localName)
        val packPart            = if (pack1 != pack2) s"{$pack1 </=/> $pack2}" else pack1
        val modPart             = if (mod1 != mod2) s"{$mod1 </=/> $mod2}" else mod1
        val locPart = if (loc1 != loc2) s"{${loc1.toTitleCase} </=/> ${loc2.toTitleCase}" else loc1.toTitleCase
        s"$packPart:$modPart:$locPart"
      case _ => s"(${PrintIR(tpe1)} vs ${PrintIR(tpe2)})"
    }
  }
  // Helper to check that two lists of types (such as tuple or arguments) match, both in length and contents
  def checkList(
      argList: List[UType],
      paramList: List[UType],
      context: Context
  ) = // Maybe that should be in context?
    if (argList.size != paramList.size)
      List(ArgNumberMismatch(
        argList.size,
        paramList.size,
        s"${context.prefix} : Different arities: ",
        location = location
      ))
    else
      argList.zip(paramList).flatMap { case (arg, param) => conformsTo(arg, param, context) }

  def uncurryFunctionType(functionTpe: UType, context: Context): Either[TypeError, (UType, Chunk[UType])] =
    for {
      // Since the function-type could be curried in the middle e.g.
      // type alias Foo = Int -> Int
      // foofoo: Int -> Foo
      // which need to be applied as foofoo: Int -> Int -> Int
      // so we need to delias in the middle of it.
      // Therefore before each uncurry step we need to dealias first.
      dealiased <- dealias(functionTpe, context)
      output <- dealiased match {
        case Type.Function(_, arg, innerFunction) =>
          uncurryFunctionType(innerFunction, context).map { case (ret, args) =>
            (ret, arg +: args)
          }
        case other =>
          Right((other, Chunk()))
      }
    } yield output

  // Fully dealises a type. (Note that it dos not dealias branching types, such as if a tuple has an aliased member
  def dealias(tpe: UType, context: Context): Either[TypeError, UType] = {
    def loop(tpe: UType, original_fqn: Option[FQName], context: Context): Either[TypeError, UType] =
      tpe match {
        case ref @ Extractors.Types.NativeRef(_, _) => Right(ref)
        case Type.Reference(_, typeName, typeArgs) =>
          val lookedUp = dists.lookupTypeDefinition(typeName.packagePath, typeName.modulePath, typeName.localName)
          lookedUp match {
            case Right(T.Definition.TypeAlias(typeParams, expr)) =>
              val newBindings         = typeParams.zip(typeArgs).toMap
              val withBindingsApplied = Utils.applyBindings(expr, newBindings)
              loop(withBindingsApplied, original_fqn.orElse(Some(typeName)), context)
            case Right(_) =>
              Right(tpe)
            case Left(lookupErr) =>
              original_fqn match {
                case Some(original) =>
                  Left(CannotDealias(lookupErr, s"Unable to dealias (nested beneath $original", location = location))
                case None =>
                  Left(CannotDealias(lookupErr, location = location))
              }
          }
        case other => Right(other)
      }
    loop(tpe, None, context)
  }

  def checkAllDefinitions(): List[TypeError] =
    GatherReferences.fromDistributionLibs(dists.getDists).definitions.toList.filter(
      !Utils.isNative(_)
    ).flatMap(checkDefinitionBody(_))

  def checkDefinitionBody(fqn: FQName): List[TypeError] = {
    val maybeDefinition = dists.lookupValueDefinition(fqn)
    maybeDefinition match {
      case Left(error)       => List(error)
      case Right(definition) => check(definition.body)
    }
  }

  def conformsTo(valueType: UType, declaredType: UType): List[TypeError] =
    conformsTo(valueType, declaredType, Context.empty)

  def conformsTo(valueType: UType, declaredType: UType, context: Context): List[TypeError] = {
    import Extractors.Types.*
    (valueType, declaredType) match {
      // Dealias everything first! You need to do this before the (LeafValue, LeafValue) check
      // and possibly the Type.Reference check (See elm error w/ unused type argument)
      case (dealiased(value), declared) =>
        conformsTo(value, declared, context)
      case (value, dealiased(declared)) =>
        conformsTo(value, declared, context)

      // TODO: Make variables fail if missing when binding support is up to the task
      case (_, Type.Variable(_, name)) => context.getTypeVariable(name) match {
          case None           => List() // List(new TypeVariableMissing(name))
          case Some(lookedUp) => conformsTo(valueType, lookedUp, context)
        }
      case (Type.Variable(_, name), _) => context.getTypeVariable(name) match {
          case None           => List() // List(new TypeVariableMissing(name))
          case Some(lookedUp) => conformsTo(lookedUp, declaredType, context)
        }
      case (Type.Function(_, valueArg, valueRet), Type.Function(_, declaredArg, declaredRet)) =>
        conformsTo(valueRet, declaredRet, context) ++ conformsTo(declaredArg, valueArg, context)
      case (Type.Tuple(_, valueElements), Type.Tuple(_, declaredElements)) =>
        checkList(valueElements.toList, declaredElements.toList, context.withPrefix("Comparing Tuples:"))
      case (valueTpe @ Type.Record(_, valueFields), declaredTpe @ Type.Record(_, declaredFields)) =>
        // Map both to sets
        val valueFieldSet: Set[Name]    = valueFields.map(_.name).toSet
        val declaredFieldSet: Set[Name] = declaredFields.map(_.name).toSet
        // Use .diff to find if some are entirely absent from the other
        val missingFromValue = valueFieldSet
          .diff(declaredFieldSet).toList.map(missing =>
            TypeLacksField(valueTpe, missing, s"Required by ${PrintIR(declaredTpe)}")
          )
        val missingFromDeclared = declaredFieldSet
          .diff(valueFieldSet)
          .toList.map(bonus => TypeHasExtraField(valueTpe, declaredTpe, bonus))
        // For everything shared, lookup types in both and ensure they match
        val sharedFields                       = valueFieldSet.intersect(declaredFieldSet)
        val valueFieldMap: Map[Name, UType]    = valueFields.map(field => field.name -> field.data).toMap
        val declaredFieldMap: Map[Name, UType] = declaredFields.map(field => field.name -> field.data).toMap
        val conflicts =
          sharedFields.flatMap(field => conformsTo(valueFieldMap(field), declaredFieldMap(field), context))
        missingFromValue ++ missingFromDeclared ++ conflicts
      case (DictRef(valueKey, valueValue), DictRef(declaredKey, declaredValue)) =>
        conformsTo(valueKey, declaredKey, context) ++ conformsTo(valueValue, declaredValue, context)
      case (ResultRef(valueErr, valueOk), ResultRef(declaredErr, declaredOk)) =>
        conformsTo(valueErr, declaredErr, context) ++ conformsTo(valueOk, declaredOk, context)
      case (ListRef(valueElement), ListRef(declaredElement)) => conformsTo(valueElement, declaredElement, context)
      case (MaybeRef(valueElement), MaybeRef(declaredElement)) =>
        conformsTo(valueElement, declaredElement, context)
      case (Type.Reference(_, valueName, valueArgs), Type.Reference(_, declaredName, declaredArgs))
          if valueName == declaredName =>
        checkList(
          valueArgs.toList,
          declaredArgs.toList,
          context.withPrefix(s"Comparing arguments on reference $valueName")
        )
      // TODO Perhaps leaf-type checking needs to be changed or removed?
      case (left @ LeafType(), right @ LeafType()) =>
        if (left == right) List() else List(TypesMismatch(left, right, "Value type does not match declared type"))
      case (valueOther, declaredOther) if valueOther.getClass == declaredOther.getClass =>
        List(
          UnknownTypeMismatch(valueOther, declaredOther, location = location)
        )
      case (valueOther, declaredOther) =>
        List(TypesMismatch(valueOther, declaredOther, "Different types of type", location = location))
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
          case Apply(_, function, argument) => handleApply(suspect, tpe, function, argument, context)
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
      case (_, Type.Variable(_, _))          => List() // TODO: Type variable handling
      case (otherLit, otherTpe)              => List(LiteralTypeMismatch(otherLit, otherTpe, location = location))
    }
    fromChildren ++ matchErrors
  }

  def handleApply(
      node: TypedValue,
      tpe: UType,
      function: TypedValue,
      argument: TypedValue,
      context: Context
  ): TypeCheckerResult = {
    val fromChildren = check(function, context) ++ check(argument, context)
    val fromTpe =
      dealias(function.attributes, context) match {
        case Right(Type.Function(_, paramType, returnType)) =>
          conformsTo(argument.attributes, paramType, context) ++ conformsTo(
            returnType,
            tpe,
            context
          ) // TODO: Useful context lost here
        case Right(_)  => List(ApplyToNonFunction(node, function, argument, location = location))
        case Left(err) => List(err)
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
    import Extractors.Types.*
    val fromChildren = List[TypeError]()
    uncurryFunctionType(tpe, context) match {
      // if there are uncurrying errors, do not proceed to typecheck the constructor, just return the uncurrying errors
      case Left(error) => List[TypeError](error)
      // otherwise run the constructor and return the errors from that
      case Right((ret, args)) =>
        ret match {
          case NonNativeRef(name, typeArgs) => dists.lookupTypeDefinition(name) match {
              case Right(T.Definition.CustomType(typeParams, AccessControlled(_, ctors))) =>
                val newBindings = typeParams.toList.zip(typeArgs.toList).toMap
                val missedName = helper(
                  fqn.packagePath != name.packagePath || fqn.modulePath != name.modulePath,
                  OtherTypeError(s"Constructor $fqn does not match type name $name", location = location)
                )
                val fromCtor = ctors.toMap.get(fqn.localName) match {
                  case Some(ctorArgs) =>
                    checkList(
                      args.toList,
                      ctorArgs.toList.map(_._2).map(Utils.applyBindings(_, newBindings)),
                      context.withPrefix(s"Comparing $fqn constructor value to looked up type ${PrintIR(tpe)}")
                    )
                  case None =>
                    List(
                      OtherTypeError(
                        s"Constructor type $name exists, but does not have arm for ${fqn.localName.toCamelCase}",
                        location = location
                      )
                    )
                }
                missedName ++ fromCtor
              case Right(other) =>
                List(ImproperTypeDef(name, other, s"Type union expected", location = location))
              case Left(err) =>
                List(err.withContext(s"Needed looking for implicit constructor ${fqn.toStringTitleCase}"))
            }
          // The following is for implicit record constructors
          case Type.Record(_, fields) =>
            val argErrors = checkList(
              args.toList,
              fields.map(_.data),
              context.withPrefix(s"Comparing $fqn implicit constructor value to looked up type ${PrintIR(tpe)}")
            )
            val nameMismatch = dists.lookupTypeDefinition(fqn) match {
              case Right(T.Definition.TypeAlias(_, aliasedRecordType)) => conformsTo(aliasedRecordType, ret, context)
              case Right(other) => List(ImproperTypeDef(
                  fqn,
                  other,
                  s"I think this is an implicit record constructor because the return type is a record, but the function points to something else",
                  location = location
                ))
              case Left(err) =>
                List(err.withContext(s"Needed looking for implicit constructor ${fqn.toStringTitleCase}"))
            }
            argErrors ++ nameMismatch

          case NativeRef(_, _) => List() // TODO: check native constructor calls
          case other =>
            List(ImproperType(
              other,
              s"Reference to type union or implicit record constructor expected",
              location = location
            ))
        }
    }
  }
  def handleFieldValue(tpe: UType, recordValue: TypedValue, name: Name, context: Context): TypeCheckerResult = {
    val fromChildren = check(recordValue, context)
    val fromTpe = dealias(recordValue.attributes, context) match {
      case Right(rTpe @ Type.Record(_, fields)) =>
        fields.map(field => field.name -> field.data).toMap.get(name) match {
          case None           => List(TypeLacksField(rTpe, name, "Referenced by field node", location = location))
          case Some(fieldTpe) => conformsTo(fieldTpe, tpe, context)
        }
      case Right(other) => List(ImproperType(other, s"Record type expected", location = location))
      case Left(err)    => List(err)
    }
    fromChildren ++ fromTpe
  }
  def handleFieldFunction(tpe: UType, name: Name, context: Context): TypeCheckerResult = {
    val fromChildren = List()
    val fromTpe = tpe match {
      case Type.Function(_, _, _) => List()
      case other => List(ImproperType(other, "Field function should be function:", location = location))
    }
    // TODO: tpe should be... function from extensible record type to ???
    fromChildren ++ fromTpe
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
    val fromTpe = tpe match {
      case Type.Function(_, arg, ret) =>
        conformsTo(ret, body.attributes, context) ++ conformsTo(pattern.attributes, arg, context)
      case other => List(ImproperType(other, "Field function should be function:", location = location))
    }
    // TODO: Check tpe's argument matches (strictly) with pattern
    // TODO: Figure out variable bindings
    fromChildren ++ fromTpe
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
    fromChildren ++ conformsTo(inValue.attributes, tpe)
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
    fromChildren ++ conformsTo(inValue.attributes, tpe)
  }
  def handleListValue(tpe: UType, elements: List[TypedValue], context: Context): TypeCheckerResult = {
    // We expect declared element types to be the same, but values may differ; so we only grab the first set of errors at the type level, but fully explore all elements.
    val fromChildren = elements.flatMap(check(_, context))
    val fromTpe = tpe match {
      case Extractors.Types.ListRef(elementType) =>
        elements.foldLeft(List(): List[TypeError]) { (acc, next) =>
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
      case other => List(ImproperType(other, s"Expected list", location = location))
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
    val casesMatch = cases.flatMap { case (pattern, caseValue) =>
      conformsTo(value.attributes, pattern.attributes, context.withPrefix("Checking Pattern:")) ++
        conformsTo(caseValue.attributes, tpe, context)
    }
    // TODO: Check values from each case
    // TODO: Manage store
    // TODO: Check each case's pattern can be its value
    // TODO: Check value must be one of the patterns
    fromChildren ++ casesMatch
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
          .diff(valFieldSet)
          .toList.map(missing => ValueLacksField(value, recordTpe, missing))
        val missingFromValue = valFieldSet
          .diff(tpeFieldSet)
          .toList.map(bonus => ValueHasExtraField(value, recordTpe, bonus))
        val valFieldMap: Map[Name, TypedValue] = valFields.toMap
        val tpeFieldMap: Map[Name, UType]      = tpeFields.map(field => field.name -> field.data).toMap
        val conflicts = tpeFieldSet.intersect(valFieldSet).flatMap(name =>
          conformsTo(valFieldMap(name).attributes, tpeFieldMap(name), context)
        )
        missingFromTpe ++ missingFromValue ++ conflicts
      case other => List(ImproperType(other, "Value is of type Record", location = location))
    }
    fromChildren ++ fromTpe
  }
  def handleReference(tpe: UType, fqn: FQName, context: Context): TypeCheckerResult = {
    val fromChildren = List()
    val fromType: List[TypeError] = if (!Utils.isNative(fqn)) {
      dists.lookupValueDefinition(fqn) match {
        case Left(err) => List(err.withContext(
            "Needed looking for non-native IR reference. (Usually this inidcates a user-defined function)"
          ))
        case Right(definition) =>
          val spec    = definition.toSpecification
          val curried = Utils.curryTypeFunction(spec.output, spec.inputs)
          conformsTo(curried, tpe, context)

      }
    } else List() // TODO: Handle native type references
    fromChildren ++ fromType
  }
  def handleTuple(tpe: UType, elements: List[TypedValue], context: Context): TypeCheckerResult = {
    val fromTpe = tpe match {
      case Type.Tuple(_, elementTypes) =>
        checkList(elements.map(_.attributes), elementTypes.toList, context)
      case other => List(ImproperType(other, "Tuple expected", location = location))
    }
    val fromChildren = elements.flatMap(check(_, context))
    fromChildren ++ fromTpe
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
    val fromTpe = tpe match {
      case Type.Record(_, tpeFields) =>
        val fieldMap = tpeFields.map(field => field.name -> field.data).toMap
        conformsTo(valueToUpdate.attributes, tpe) ++ fields.flatMap { field =>
          fieldMap.get(field._1) match {
            case None => List(TypeLacksField(
                tpe,
                field._1,
                "Tried to update record field which is not present",
                location = location
              ))
            case Some(found) => conformsTo(field._2.attributes, found, context)
          }
        }
      case other => List(ImproperType(other, "Record type expected", location = location))
    }
    fromChildren ++ fromTpe
  }
  def handleVariable(tpe: UType, name: Name, context: Context): TypeCheckerResult =
    // TODO: Keep that in the context
    // TODONT: Do not re-check the body - only make sure that the tpe matches this
    List()

}
