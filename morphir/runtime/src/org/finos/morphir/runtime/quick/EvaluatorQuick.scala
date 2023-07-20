package org.finos.morphir.runtime.quick

import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import V.*
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.Type.Type as TT
import org.finos.morphir.ir.{FQName, Module, Name, QName, Type, Field}
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.datamodel.{Data, Concept, Label, EnumLabel}
import SDKValue.{SDKNativeFunction, SDKNativeValue}
import org.finos.morphir.ir.Distribution.Distribution.Library
import zio.Chunk
import scala.collection.mutable

object FQString {
  def unapply(fqName: FQName): Option[String] = Some(fqName.toString)
}

object EvaluatorQuick {
  type IntType = Long

  def evaluate[TA, VA](ir: Value[TA, VA], store: Store[TA, VA]): Any = Result.unwrap(Loop.loop(ir, store))

  def evalFunction(entryFQName: FQName, store: Store[Unit, Type.UType], input: Any): Any = {
    val ir        = scalaToIR(input)
    val applyNode = V.apply(V.reference(entryFQName), ir) :> T.unit // lies but I don't think we check?
    evaluate[Unit, Type.UType](applyNode, store)
  }

  def evalFunctionToDDL(entryFQName: FQName, store: Store[Unit, Type.UType], input: Any, dist: Library): Data = {
    val inputIR   = scalaToIR(input);
    val applyNode = V.apply(V.reference(entryFQName), inputIR) :> T.unit // lies but I don't think we check?
    val result    = Loop.loop(applyNode, store);
    val tpe: Type.Type[Unit] = store.getDefinition(entryFQName)
      .get
      .asInstanceOf[SDKValue.SDKValueDefinition[Unit, Type.UType]]
      .definition
      .outputType
      .asInstanceOf[TT.Function[Unit]]
      .returnType
    resultToDDL(result, tpe, dist)
  }

  def typeToConcept(tpe: Type.Type[Unit], dist: Library): Concept =
    tpe match {
      case TT.ExtensibleRecord(attributes, name, fields) =>
        throw new UnsupportedType("Extensible records not supported for DDL")
      case TT.Function(attributes, argumentType, returnType) =>
        throw new UnsupportedType("Functiom types not supported for DDL")
      case TT.Record(attributes, fields) =>
        Concept.Struct(fields.map(field => (Label(field.name.toCamelCase), typeToConcept(field.data, dist))).toList)
      case TT.Reference(attributes, FQString("Morphir.SDK:Basics:int"), _)    => Concept.Int32
      case TT.Reference(attributes, FQString("Morphir.SDK:String:string"), _) => Concept.String
      case TT.Reference(attributes, FQString("Morphir.SDK:Basics:bool"), _)   => Concept.Boolean
      case TT.Reference(attributes, FQString("Morphir.SDK:Char:char"), _)     => Concept.Char
      case TT.Reference(attributes, FQString("Morphir.SDK:Basics:float"), _)  => Concept.Decimal
      case TT.Reference(attributes, FQString("Morphir.SDK:List:list"), Chunk(elementType)) =>
        Concept.List(typeToConcept(elementType, dist))
      case TT.Reference(attributes, FQString("Morphir.SDK:Maybe:maybe"), Chunk(elementType)) =>
        Concept.Optional(typeToConcept(elementType, dist))
      case TT.Reference(attributes, FQString("Morphir.SDK:Dict:dict"), Chunk(keyType, valType)) =>
        Concept.Map(typeToConcept(keyType, dist), typeToConcept(valType, dist))
      case TT.Reference(attributes, typeName, typeParams) =>
        val lookedUp = dist.lookupTypeSpecification(typeName.packagePath, typeName.modulePath, typeName.localName)
        lookedUp.getOrElse(throw new Exception(s"Could not find spec for $typeName")) match {
          case Type.Specification.TypeAliasSpecification(params, expr) =>
            Concept.Alias(typeName.toQualifiedName, typeToConcept(expr, dist))
          case Type.Specification.CustomTypeSpecification(params, ctors) =>
            val cases = ctors.toMap.toList.map { case (caseName, args) =>
              val argTuples = args.map { case (argName: Name, argType: Type.UType) =>
                (EnumLabel.Named(argName.toCamelCase), typeToConcept(argType, dist))
              }
              val conceptName: String                  = caseName.toCamelCase
              val concepts: List[(EnumLabel, Concept)] = argTuples.toList
              Concept.Enum.Case(Label(conceptName), concepts)
            }
            Concept.Enum(typeName.toQualifiedName, cases)
          case other => throw new Exception(s"$other is unknown to me")
        }
      case TT.Tuple(attributes, elements) => Concept.Tuple(elements.map(element => typeToConcept(element, dist)).toList)
      case TT.Unit(attributes)            => Concept.Unit
      case TT.Variable(attributes, name)  => throw new UnsupportedTypeParameter("Tried to convert type with parameter")
    }
  def resultAndConceptToData(result: Result[Unit, Type.UType], concept: Concept): Data =
    (concept, result) match {
      case (Concept.Record(qname, fields), Result.Record(elements)) =>
        if (fields.length != elements.size) {
          throw new ResultDoesNotMatchType(s"$fields has different number of elements than $elements")
        } else {
          val tuples = fields.map { case (Label(name), innerConcept) =>
            val value =
              elements.getOrElse(Name(name), throw new MissingField(s"Type expected $name but not found in $elements"))
            (Label(name), resultAndConceptToData(value, innerConcept))
          }
          Data.Record(qname, tuples.toList)
        }
      case (Concept.Int32, Result.Primitive(value: IntType)) =>
        Data.Int(value.toInt)
      case (Concept.String, Result.Primitive(value: String)) =>
        Data.String(value)
      case (Concept.Boolean, Result.Primitive(value: Boolean)) =>
        Data.Boolean(value)
      case (Concept.Char, Result.Primitive(value: Char)) =>
        Data.Char(value)
      case (Concept.Decimal, Result.Primitive(value: Double)) =>
        Data.Decimal(scala.BigDecimal(value))
      case (alias: Concept.Alias, result) => Data.Aliased(resultAndConceptToData(result, alias.value), alias)
      case (Concept.List(elementConcept), Result.ListResult(elements)) =>
        val inners = elements.map(element => resultAndConceptToData(element, elementConcept))
        Data.List(inners, elementConcept)
      case (Concept.Optional(elementShape), Result.ConstructorResult(FQString("Morphir.SDK:Maybe:nothing"), List())) =>
        Data.Optional.None(elementShape)
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
      case (badType, badResult) =>
        throw new ResultDoesNotMatchType(s"Could not match type $badType with result $badResult")
    }

  def resultToDDL(result: Result[Unit, Type.UType], tpe: Type.Type[Unit], dist: Library): Data = {
    val concept = typeToConcept(tpe, dist)
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
      case other => throw new Exception(s"I don't know how to decompose $other")
    }

}
