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
import org.finos.morphir.runtime.{Distributions, RTValue}
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.services.sdk.*
import zio.Chunk

import scala.collection.mutable

object EvaluatorQuick {
  type FloatType = Double

  private[runtime] def evalAction(
      value: TypedValue,
      globals: GlobalDefs,
      dists: Distributions
  ): RTAction[MorphirEnv, EvaluationError, Data] =
    RTAction.environmentWithPure[MorphirSdk] { env =>
      def newStore = GlobalDefs(globals.definitions, globals.ctors)
      RTAction.attempt(EvaluatorQuick.eval(value, newStore, dists)).refineToOrDie[EvaluationError]
    }

  private[runtime] def eval(
      value: TypedValue,
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
        throw NotImplemented("Extensible records not supported for Morphir data model.")
      case TT.Function(_, _, _) =>
        throw UnsupportedType(
          tpe,
          """Function type found in return from entry point.
            |Was there a function nested beneath the type your entry point returns?
            |""".stripMargin
        )
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
      case OrderRef()     => Concept.Order
      case MonthRef()     => Concept.Month
      case DayOfWeekRef() => Concept.DayOfWeek

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
        lookedUp match {
          case Right(Type.Specification.TypeAliasSpecification(typeParams, expr)) =>
            val newBindings = typeParams.zip(conceptArgs).toMap
            typeToConcept(expr, dists, newBindings) match {
              case Concept.Struct(fields) => Concept.Record(typeName, fields)
              case other                  => Concept.Alias(typeName, other)
            }
          case Right(Type.Specification.CustomTypeSpecification(typeParams, ctors)) =>
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
          case Right(other) => throw UnsupportedTypeSpecification(
              other,
              "This specification was found as part of the return type from the entry point. Cannot generate Concept for MDM return."
            )
          case Left(err) => throw err.withContext("Error finding type specification defined in entry point function")
        }
      case TT.Tuple(_, elements) =>
        Concept.Tuple(elements.map(element => typeToConcept(element, dists, boundTypes)).toList)
      case TT.Unit(_)           => Concept.Unit
      case TT.Variable(_, name) => boundTypes(name)
    }
  def resultAndConceptToData(result: RTValue, concept: Concept): Data = {
    import RTValueToMDMError.*
    (concept, result) match {
      case (Concept.Struct(fields), record @ RTValue.Record(elements)) =>
        if (fields.length != elements.size) {
          throw ResultTypeMismatch(
            result,
            concept,
            s"Concept.Struct and RTValue.Record both indicate records, but different number of fields (${fields.length} vs ${elements.size}"
          )
        } else {
          val tuples = fields.map { case (label, innerConcept) =>
            val value =
              elements.getOrElse(Name(label.value), throw new MissingField(record, label))
            (label, resultAndConceptToData(value, innerConcept))
          }
          Data.Struct(tuples.toList)
        }
      case (Concept.Record(qName, fields), record @ RTValue.Record(elements)) =>
        if (fields.length != elements.size) {
          throw ResultTypeMismatch(
            result,
            concept,
            s"Concept.Record and RTValue.Record both indicate records, but different number of fields (${fields.length} vs ${elements.size}"
          )
        } else {
          val tuples = fields.map { case (label, innerConcept) =>
            val value =
              elements.getOrElse(Name(label.value), throw new MissingField(record, label))
            (label, resultAndConceptToData(value, innerConcept))
          }
          Data.Record(qName, tuples.toList)
        }

      case (Concept.Int16, RTValue.Primitive.Int(value)) =>
        Data.Int16(value.toShort)
      case (Concept.Int32, RTValue.Primitive.Int(value)) =>
        Data.Int32(value.toInt)
      case (Concept.Int64, RTValue.Primitive.Int(value)) =>
        Data.Int64(value.toLong)
      case (Concept.Order, RTValue.ConstructorResult(FQStringTitleCase("Morphir.SDK:Basics:GT"), List())) =>
        Data.Order(1)
      case (Concept.Order, RTValue.ConstructorResult(FQStringTitleCase("Morphir.SDK:Basics:LT"), List())) =>
        Data.Order(-1)
      case (Concept.Order, RTValue.ConstructorResult(FQStringTitleCase("Morphir.SDK:Basics:EQ"), List())) =>
        Data.Order(0)
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
      case (Concept.Month, RTValue.Month(value: java.time.Month)) =>
        Data.Month(value)
      case (Concept.DayOfWeek, RTValue.DayOfWeek(value: java.time.DayOfWeek)) =>
        Data.DayOfWeek(value)

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
      case (
            Concept.Optional(elementShape),
            RTValue.ConstructorResult(FQStringTitleCase("Morphir.SDK:Maybe:Nothing"), List())
          ) =>
        Data.Optional.None(elementShape)
      case (
            shape @ Concept.Result(_, okType),
            RTValue.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Ok"), List(value))
          ) =>
        Data.Result.Ok(resultAndConceptToData(value, okType), shape)
      case (
            shape @ Concept.Result(errType, _),
            RTValue.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Err"), List(value))
          ) =>
        Data.Result.Err(resultAndConceptToData(value, errType), shape)
      case (
            Concept.Optional(elementShape),
            RTValue.ConstructorResult(
              FQStringTitleCase("Morphir.SDK:Maybe:Just"),
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
          throw new ResultTypeMismatch(
            result,
            concept,
            s"Concept Enum has no branch for ${fqName.localName.toTitleCase}"
          )
        )
        if (args.length != fields.length)
          throw new ResultTypeMismatch(
            result,
            concept,
            s"Concept enum expects ${fields.length} argument, but result has ${args.length}"
          )
        else {
          val zipped = args.zip(fields)
          val argData = zipped.map { case (innerResult, (argName, argConcept)) =>
            (argName, resultAndConceptToData(innerResult, argConcept))
          }
          Data.Case(argData, fqName.localName.toTitleCase, enumConcept)
        }
      case (Concept.Tuple(conceptElements), RTValue.Tuple(resultElements)) =>
        val listed = resultElements
        if (conceptElements.length != listed.length) {
          throw new ResultTypeMismatch(
            result,
            concept,
            s"Concept expects ${conceptElements.length}-Tuple, but result is ${listed.length}-Tuple"
          )
        } else {
          val inners = conceptElements.zip(listed).map { case (innerConcept, innerResult) =>
            resultAndConceptToData(innerResult, innerConcept)
          }
          Data.Tuple(inners)
        }
      case (Concept.Unit, RTValue.Unit()) => Data.Unit
      case _ =>
        throw new ResultTypeMismatch(result, concept, s"(No matching arm for this combination of concept and result.)")
    }
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

}
