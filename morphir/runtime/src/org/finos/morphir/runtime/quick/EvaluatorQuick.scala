package org.finos.morphir.runtime.quick

import org.finos.morphir.datamodel.{Concept, Data, EnumLabel, Label}
import org.finos.morphir.ir.Type.{Type => TT}
import org.finos.morphir.ir.Type
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.Extractors.*
import org.finos.morphir.runtime.Extractors.Types.*
import org.finos.morphir.runtime.TypedMorphirRuntimeDefs.{TypeAttribs, ValueAttribs}
import org.finos.morphir.runtime.{
  Distributions,
  EvaluationError,
  MissingField,
  RTValue,
  ResultDoesNotMatchType,
  UnexpectedType,
  UnmatchedPattern,
  UnsupportedType
}
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.services.sdk.*
import zio.Chunk

import scala.collection.mutable

object EvaluatorQuick {
  type FloatType = Double

  private[runtime] def evalAction(
      value: Value[TypeAttribs, ValueAttribs],
      globals: GlobalDefs,
      dists: Distributions
  ): RTAction[MorphirEnv, EvaluationError, Data] =
    RTAction.environmentWithPure[MorphirSdk] { env =>
      def newStore = GlobalDefs(globals.definitions, globals.ctors)
      RTAction.succeed(EvaluatorQuick.eval(value, newStore, dists))
    }

  private[runtime] def eval(
      value: Value[TypeAttribs, ValueAttribs],
      globals: GlobalDefs,
      dists: Distributions
  ): Data = {
    // Does type-checking when producing MDM concept so want to catch typechecking first
    val concept = typeToConcept(value.attributes, dists, Map())
    // Run the evaluation loop
    val result = Loop(globals).loop(value, Store.empty)
    resultToMDM(result, concept)
  }

  def typeToConcept(tpe: Type.Type[Unit], dists: Distributions, boundTypes: Map[Name, Concept]): Concept =
    tpe match {
      case TT.ExtensibleRecord(_, _, _) =>
        throw UnsupportedType("Extensible records not supported for Morphir data model")
      case TT.Function(_, _, _) =>
        throw UnsupportedType("Functiom types not supported for Morphir data model")
      case TT.Record(_, fields) => Concept.Struct(fields.map(field =>
          (Label(field.name.toCamelCase), typeToConcept(field.data, dists, boundTypes))
        ).toList)
      case IntRef()       => Concept.Int32
      case Int16Ref()     => Concept.Int16
      case Int32Ref()     => Concept.Int32
      case Int64Ref()     => Concept.Int64
      case StringRef()    => Concept.String
      case BoolRef()      => Concept.Boolean
      case CharRef()      => Concept.Char
      case FloatRef()     => Concept.Float
      case DecimalRef()   => Concept.Decimal
      case LocalDateRef() => Concept.LocalDate
      case LocalTimeRef() => Concept.LocalTime

      case ResultRef(errType, okType) =>
        Concept.Result(typeToConcept(errType, dists, boundTypes), typeToConcept(okType, dists, boundTypes))
      case ListRef(elementType) =>
        Concept.List(typeToConcept(elementType, dists, boundTypes))
      case MaybeRef(elementType) =>
        Concept.Optional(typeToConcept(elementType, dists, boundTypes))
      case DictRef(keyType, valType) =>
        Concept.Map(typeToConcept(keyType, dists, boundTypes), typeToConcept(valType, dists, boundTypes))
      case SetRef(elementType) =>
        Concept.Set(typeToConcept(elementType, dists, boundTypes))
      case TT.Reference(_, typeName, typeArgs) =>
        val lookedUp    = dists.lookupTypeSpecification(typeName.packagePath, typeName.modulePath, typeName.localName)
        val conceptArgs = typeArgs.map(typeToConcept(_, dists, boundTypes))
        lookedUp.getOrElse(throw new UnexpectedType(s"Could not find spec for $typeName")) match {
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
              val conceptName: String                  = caseName.toTitleCase
              val concepts: List[(EnumLabel, Concept)] = argTuples.toList
              Concept.Enum.Case(Label(conceptName), concepts)
            }
            Concept.Enum(typeName, cases)
          case other => throw UnsupportedType(s"$other is not a recognized type")
        }
      case TT.Tuple(_, elements) =>
        Concept.Tuple(elements.map(element => typeToConcept(element, dists, boundTypes)).toList)
      case TT.Unit(_)           => Concept.Unit
      case TT.Variable(_, name) => boundTypes(name)
    }
  def resultAndConceptToData(result: RTValue, concept: Concept): Data =
    (concept, result) match {
      case (Concept.Struct(fields), RTValue.Record(elements)) =>
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
      case (Concept.Record(qName, fields), RTValue.Record(elements)) =>
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

      case (Concept.Int16, RTValue.Primitive.Int(value)) =>
        Data.Int16(value.toShort)
      case (Concept.Int32, RTValue.Primitive.Int(value)) =>
        Data.Int32(value.toInt)
      case (Concept.Int64, RTValue.Primitive.Int(value)) =>
        Data.Int64(value.toLong)

      case (Concept.String, RTValue.Primitive.String(value)) =>
        Data.String(value)
      case (Concept.Boolean, RTValue.Primitive.Boolean(value)) =>
        Data.Boolean(value)
      case (Concept.Char, RTValue.Primitive.Char(value)) =>
        Data.Char(value)
      case (Concept.LocalDate, RTValue.LocalDate(value: java.time.LocalDate)) =>
        Data.LocalDate(value)
      case (Concept.LocalTime, RTValue.LocalTime(value: java.time.LocalTime)) =>
        Data.LocalTime(value)

      case (Concept.Float, RTValue.Primitive.Float(value)) =>
        Data.Float(value.toDouble)
      case (Concept.Float, RTValue.Primitive.BigDecimal(value)) =>
        Data.Float(value.toDouble)

      // handles Primitive.Double/Float/Int/Long/BigDecimal
      case (Concept.Decimal, RTValue.Primitive.DecimalBounded(value)) =>
        Data.Decimal(value)

      case (alias: Concept.Alias, result) => Data.Aliased(resultAndConceptToData(result, alias.value), alias)
      case (Concept.List(elementConcept), RTValue.List(elements)) =>
        val inners = elements.map(element => resultAndConceptToData(element, elementConcept))
        Data.List(inners, elementConcept)
      case (Concept.Set(elementConcept), RTValue.Set(elements)) =>
        val inners = elements.map(element => resultAndConceptToData(element, elementConcept))
        Data.Set(inners, elementConcept)
      case (Concept.Optional(elementShape), RTValue.ConstructorResult(FQString("Morphir.SDK:Maybe:nothing"), List())) =>
        Data.Optional.None(elementShape)
      case (
            shape @ Concept.Result(_, okType),
            RTValue.ConstructorResult(FQString("Morphir.SDK:Result:ok"), List(value))
          ) =>
        Data.Result.Ok(resultAndConceptToData(value, okType), shape)
      case (
            shape @ Concept.Result(errType, _),
            RTValue.ConstructorResult(FQString("Morphir.SDK:Result:err"), List(value))
          ) =>
        Data.Result.Err(resultAndConceptToData(value, errType), shape)
      case (
            Concept.Optional(elementShape),
            RTValue.ConstructorResult(
              FQString("Morphir.SDK:Maybe:just"),
              List(value)
            )
          ) => Data.Optional.Some(resultAndConceptToData(value, elementShape))
      case (mapConcept @ Concept.Map(keyConcept, valConcept), RTValue.Map(elements)) =>
        val inners = elements.map { case (key, value) =>
          (resultAndConceptToData(key, keyConcept), resultAndConceptToData(value, valConcept))
        }
        Data.Map.copyFrom(inners, mapConcept)
      case (enumConcept @ Concept.Enum(_, cases), RTValue.ConstructorResult(fqName, args)) =>
        val fieldMap = cases.map { case Concept.Enum.Case(Label(string), fields) => string -> fields }.toMap
        val fields = fieldMap.getOrElse(
          fqName.localName.toTitleCase,
          throw new ResultDoesNotMatchType(s"Failed to find constructor ${fqName.localName} among ${fieldMap.keys}")
        )
        if (args.length != fields.length)
          throw new ResultDoesNotMatchType(s"Parameter lengths differ between $args and $fields")
        else {
          val zipped = args.zip(fields)
          val argData = zipped.map { case (innerResult, (argName, argConcept)) =>
            (argName, resultAndConceptToData(innerResult, argConcept))
          }
          Data.Case(argData, fqName.localName.toTitleCase, enumConcept)
        }
      case (Concept.Tuple(conceptElements), RTValue.Tuple(resultElements)) =>
        val listed = resultElements.toList
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
      case (Concept.Unit, RTValue.Unit()) => Data.Unit
      case (badType, badResult @ RTValue.Primitive(value)) =>
        throw new ResultDoesNotMatchType(
          s"Could not match type $badType with result $badResult. The value was $value which is of type ${value.getClass()}}"
        )
      case (badType, badResult) =>
        throw new ResultDoesNotMatchType(s"Could not match type $badType with result $badResult")
    }

  def resultToMDM(result: RTValue, concept: Concept): Data =
    resultAndConceptToData(result, concept)

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
      case elements: List[t] => V.list(elements.map(scalaToIR(_)): _*)
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
      case other     => throw new UnmatchedPattern(s"I don't know how to decompose $other")
    }

}
