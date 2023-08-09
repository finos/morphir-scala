package org.finos.morphir.runtime.quick

import org.finos.morphir.datamodel.{Concept, Data, EnumLabel, Label}
import org.finos.morphir.extensibility.*
import org.finos.morphir.extensibility.SdkModuleDescriptors.*
import org.finos.morphir.ir.Type.Type as TT
import org.finos.morphir.ir.Type
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.Extractors.*
import org.finos.morphir.runtime.Distributions
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.services.sdk.*
import org.finos.morphir.runtime.{EvaluationError, MissingField, ResultDoesNotMatchType, UnsupportedType}
import zio.Chunk

import scala.collection.mutable

object EvaluatorQuick {

  type IntType   = morphir.sdk.Basics.Int
  type FloatType = Double

  private[runtime] def evalAction(
      value: Value[Unit, T.UType],
      store: Store[Unit, T.UType],
      dists: Distributions
  ): RTAction[MorphirEnv, EvaluationError, Data] =
    RTAction.environmentWithPure[MorphirSdk] { env =>
      val basics = env.get[BasicsModule]
      // HACK: To work out
      val modBy = Morphir.SDK.Basics.modBy

      def newValue = fromNative[Unit, T.UType](modBy)
      def newStore = Store(store.definitions + (modBy.name -> newValue), store.ctors, store.callStack)
      RTAction.succeed(EvaluatorQuick.eval(value, newStore, dists))
    }

  private[runtime] def eval(value: Value[Unit, T.UType], store: Store[Unit, T.UType], dists: Distributions): Data = {
    val result = Loop.loop(value, store)
    resultToMDM(result, value.attributes, dists)
  }

  def unwrap[TA, VA](res: Result[TA, VA]): Any =
    res match {
      case Result.Unit()               => ()                      // Ever used?
      case Result.Primitive(value)     => value                   // Duh
      case Result.ListResult(elements) => elements.map(unwrap(_)) // Needed for non-higher-order head, presumably others
      case Result.Tuple(elements) => // Needed for tuple.first, possibly others
        val listed = Helpers.tupleToList(elements).getOrElse(throw new Exception("Invalid tuple returned to top level"))
        val mapped = listed.map(unwrap(_))
        Helpers.listToTuple(mapped)
      case Result.MapResult(elements) => elements.map { case (key, value) =>
          unwrap(key) -> unwrap(value)
        } // Needed for non-higher order sized, others
      // TODO: Option, Result, LocalDate
      // case constructor: Result.ConstructorResult[TA, VA] => constructor //Special cases?
      // case record: Result.Record => record //I don't think we ever use these?
      case other => other // Anything can be passed through a generic function
    }
  def wrap[TA, VA](value: Any): Result[TA, VA] =
    value match {
      case r: Result[_, _] => r.asInstanceOf[Result[TA, VA]]
      case ()              => Result.Unit()
      case m: Map[_, _] => Result.MapResult(m.toSeq.map { case (key, value) =>
          (wrap[TA, VA](key), wrap[TA, VA](value))
        }.toMap)
      case l: List[_]             => Result.ListResult(l.map(wrap(_)))
      case (first, second)        => Result.Tuple((wrap(first), wrap(second)))
      case (first, second, third) => Result.Tuple((wrap(first), wrap(second), wrap(third)))
      // TODO: Option, Result, LocalDate
      case primitive => Result.Primitive(primitive) // TODO: Handle each case for safety's sake
    }

  def fromNative[TA, VA](native: NativeFunction): SDKValue[TA, VA] =
    native match {
      case fn: DynamicNativeFunction2[_, _, _] =>
        val f = (arg1: Result[Unit, T.UType], arg2: Result[Unit, T.UType]) => {
          val unwrappedArg1 = unwrap(arg1)
          val unwrappedArg2 = unwrap(arg2)
          val res           = fn.invokeDynamic(unwrappedArg1, unwrappedArg2)
          wrap(res)
        }
        SDKValue.SDKNativeFunction(fn.arity, f)
      case nf: NativeFunction2[_, _, _] =>
        val f = (arg1: Result[Unit, T.UType], arg2: Result[Unit, T.UType]) => {
          val unwrappedArg1 = unwrap(arg1)
          val unwrappedArg2 = unwrap(arg2)
          val res           = nf.invokeDynamic(unwrappedArg1, unwrappedArg2)
          wrap(res)
        }
        SDKValue.SDKNativeFunction(nf.arity, f)
    }

  def typeToConcept(tpe: Type.Type[Unit], dists: Distributions, boundTypes: Map[Name, Concept]): Concept =
    tpe match {
      case TT.ExtensibleRecord(attributes, name, fields) =>
        throw UnsupportedType("Extensible records not supported for DDL")
      case TT.Function(attributes, argumentType, returnType) =>
        throw UnsupportedType("Functiom types not supported for DDL")
      case TT.Record(attributes, fields) => Concept.Struct(fields.map(field =>
          (Label(field.name.toCamelCase), typeToConcept(field.data, dists, boundTypes))
        ).toList)
      case IntRef()    => Concept.Int32
      case Int32Ref()  => Concept.Int32
      case StringRef() => Concept.String
      case BoolRef()   => Concept.Boolean
      case CharRef()   => Concept.Char
      case FloatRef()  => Concept.Decimal
      case ResultRef(errType, okType) =>
        Concept.Result(typeToConcept(errType, dists, boundTypes), typeToConcept(okType, dists, boundTypes))
      case ListRef(elementType) =>
        Concept.List(typeToConcept(elementType, dists, boundTypes))
      case MaybeRef(elementType) =>
        Concept.Optional(typeToConcept(elementType, dists, boundTypes))
      case DictRef(keyType, valType) =>
        Concept.Map(typeToConcept(keyType, dists, boundTypes), typeToConcept(valType, dists, boundTypes))
      case TT.Reference(attributes, typeName, typeArgs) =>
        val lookedUp    = dists.lookupTypeSpecification(typeName.packagePath, typeName.modulePath, typeName.localName)
        val conceptArgs = typeArgs.map(typeToConcept(_, dists, boundTypes))
        lookedUp.getOrElse(throw new Exception(s"Could not find spec for $typeName")) match {
          case Type.Specification.TypeAliasSpecification(typeParams, expr) =>
            val newBindings = typeParams.zip(conceptArgs).toMap
            typeToConcept(expr, dists, newBindings) match {
              case Concept.Struct(fields) => Concept.Record(typeName, fields)
              case other                  => Concept.Alias(typeName, other)
            }
          case Type.Specification.CustomTypeSpecification(typeParams, ctors) =>
            val newBindings = typeParams.zip(conceptArgs).toMap
            val cases = ctors.toMap.toList.map { case (caseName, args) =>
              val argTuples = args.map { case (argName: Name, argType: Type.UType) =>
                (EnumLabel.Named(argName.toCamelCase), typeToConcept(argType, dists, newBindings))
              }
              val conceptName: String                  = caseName.toCamelCase
              val concepts: List[(EnumLabel, Concept)] = argTuples.toList
              Concept.Enum.Case(Label(conceptName), concepts)
            }
            Concept.Enum(typeName, cases)
          case other => throw UnsupportedType(s"$other is not a recognized type")
        }
      case TT.Tuple(attributes, elements) =>
        Concept.Tuple(elements.map(element => typeToConcept(element, dists, boundTypes)).toList)
      case TT.Unit(attributes)           => Concept.Unit
      case TT.Variable(attributes, name) => boundTypes(name)
    }

  def resultAndConceptToData(result: Result[Unit, Type.UType], concept: Concept): Data =
    (concept, result) match {
      case (Concept.Struct(fields), Result.Record(elements)) =>
        if (fields.length != elements.size) {
          throw ResultDoesNotMatchType(s"$fields has different number of elements than $elements")
        } else {
          val tuples = fields.map { case (Label(name), innerConcept) =>
            val value =
              elements.getOrElse(Name(name), throw new MissingField(s"Type expected $name but not found in $elements"))
            (Label(name), resultAndConceptToData(value, innerConcept))
          }
          Data.Struct(tuples.toList)
        }
      case (Concept.Record(qName, fields), Result.Record(elements)) =>
        if (fields.length != elements.size) {
          throw ResultDoesNotMatchType(s"$fields has different number of elements than $elements")
        } else {
          val tuples = fields.map { case (Label(name), innerConcept) =>
            val value =
              elements.getOrElse(Name(name), throw new MissingField(s"Type expected $name but not found in $elements"))
            (Label(name), resultAndConceptToData(value, innerConcept))
          }
          Data.Record(qName, tuples.toList)
        }
      case (Concept.Int32, Result.Primitive(value: Long)) =>
        Data.Int32(value.toInt)
      case (Concept.Int32, Result.Primitive(value: IntType)) =>
        Data.Int32(value.toInt)
      case (Concept.String, Result.Primitive(value: String)) =>
        Data.String(value)
      case (Concept.Boolean, Result.Primitive(value: Boolean)) =>
        Data.Boolean(value)
      case (Concept.Char, Result.Primitive(value: Char)) =>
        Data.Char(value)
      case (Concept.LocalDate, Result.LocalDate(value: java.time.LocalDate)) =>
        Data.LocalDate(value)
      case (Concept.LocalTime, Result.LocalTime(value: java.time.LocalTate)) =>
        Data.LocalTime(value)
      case (Concept.Decimal, Result.Primitive(value: Double)) =>
        Data.Decimal(scala.BigDecimal(value))
      case (alias: Concept.Alias, result) => Data.Aliased(resultAndConceptToData(result, alias.value), alias)
      case (Concept.List(elementConcept), Result.ListResult(elements)) =>
        val inners = elements.map(element => resultAndConceptToData(element, elementConcept))
        Data.List(inners, elementConcept)
      case (Concept.Optional(elementShape), Result.ConstructorResult(FQString("Morphir.SDK:Maybe:nothing"), List())) =>
        Data.Optional.None(elementShape)
      case (
            shape @ Concept.Result(_, okType),
            Result.ConstructorResult(FQString("Morphir.SDK:Result:ok"), List(value))
          ) =>
        Data.Result.Ok(resultAndConceptToData(value, okType), shape)
      case (
            shape @ Concept.Result(errType, _),
            Result.ConstructorResult(FQString("Morphir.SDK:Result:err"), List(value))
          ) =>
        Data.Result.Err(resultAndConceptToData(value, errType), shape)
      case (
            Concept.Optional(elementShape),
            Result.ConstructorResult(
              FQString("Morphir.SDK:Maybe:just"),
              List(value)
            )
          ) => Data.Optional.Some(resultAndConceptToData(value, elementShape))
      case (mapConcept @ Concept.Map(keyConcept, valConcept), Result.MapResult(elements)) =>
        val inners = elements.toList.map { case (key, value) =>
          (resultAndConceptToData(key, keyConcept), resultAndConceptToData(value, valConcept))
        }
        Data.Map.copyFrom(mutable.LinkedHashMap.from(inners), mapConcept)
      case (enumConcept @ Concept.Enum(name, cases), Result.ConstructorResult(fqName, args)) =>
        val fieldMap = cases.map { case Concept.Enum.Case(Label(string), fields) => string -> fields }.toMap
        val fields = fieldMap.getOrElse(
          fqName.localName.toCamelCase,
          throw new ResultDoesNotMatchType(s"Failed to find constructor ${fqName.localName} among ${fieldMap.keys}")
        )
        if (args.length != fields.length)
          throw new ResultDoesNotMatchType(s"Parameter lengths differ between $args and $fields")
        else {
          val zipped = args.zip(fields)
          val argData = zipped.map { case (innerResult, (argName, argConcept)) =>
            (argName, resultAndConceptToData(innerResult, argConcept))
          }
          Data.Case(argData, fqName.toString, enumConcept)
        }
      case (Concept.Tuple(conceptElements), Result.Tuple(resultElements)) =>
        val listed = Helpers.tupleToList[Unit, Type.UType](resultElements).get
        if (conceptElements.length != listed.length) {
          throw new ResultDoesNotMatchType(
            s"Tuple type elements $conceptElements of different length than result $resultElements"
          )
        } else {
          val inners = conceptElements.zip(listed).map { case (innerConcept, innerResult) =>
            resultAndConceptToData(innerResult, innerConcept)
          }
          Data.Tuple(inners)
        }
      case (Concept.Unit, Result.Unit()) => Data.Unit
      case (badType, badResult @ Result.Primitive(value)) =>
        throw new ResultDoesNotMatchType(
          s"Could not match type $badType with result $badResult. The value was $value which is of type ${value.getClass()}}"
        )
      case (badType, badResult) =>
        throw new ResultDoesNotMatchType(s"Could not match type $badType with result $badResult")
    }

  def resultToMDM(result: Result[Unit, Type.UType], tpe: Type.Type[Unit], dists: Distributions): Data = {
    val concept = typeToConcept(tpe, dists, Map())
    resultAndConceptToData(result, concept)
  }

  case class Record(values: Map[String, Any])
  object Record {
    def apply(args: (String, Any)*): Record = Record(args.toMap)
  }
  case class Constructor(name: String, arguments: List[Any])
  def curry(f: RawValue, args: List[RawValue]): RawValue =
    args match {
      case Nil          => f
      case head :: tail => curry(V.apply(f, head), tail)
    }
  def scalaToIR(value: Any): RawValue =
    value match {
      case ()                => V.unit
      case x: Int            => V.int(x)
      case s: String         => V.string(s)
      case b: Boolean        => V.boolean(b)
      case elements: List[_] => V.list(elements.map(scalaToIR(_)))
      case values: Map[_, _] =>
        val tuples = values.map { case (key, value) => V.tuple(scalaToIR(key), scalaToIR(value)) }.toSeq
        V.apply(V.reference(FQName.fromString("Morphir.SDK:Dict:fromList")), V.list(tuples: _*))
      case Record(values) =>
        val fields = values.map { case (name, value) => (name, scalaToIR(value)) }.toSeq
        V.recordRaw(fields: _*)
      case Constructor(name, args) =>
        val mappedArgs  = args.map(scalaToIR(_))
        val constructor = V.constructor(FQName.fromString(name))
        curry(constructor, mappedArgs)
      case (a, b)    => V.tuple(Chunk(scalaToIR(a), scalaToIR(b)))
      case (a, b, c) => V.tuple(Chunk(scalaToIR(a), scalaToIR(b), scalaToIR(c)))
      case other     => throw new Exception(s"I don't know how to decompose $other")
    }

}
