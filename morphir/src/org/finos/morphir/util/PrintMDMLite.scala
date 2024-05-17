package org.finos.morphir.util

import zio.json._
import zio.json.ast._
import org.finos.morphir.naming._
import org.finos.morphir.datamodel._
import org.finos.morphir.datamodel.Data._
import org.finos.morphir.datamodel.Concept._

object PrintMDMLite {
  implicit val nameEncoder: JsonEncoder[Name] = JsonEncoder[String].contramap(_.toString)
  implicit val nameDecoder: JsonDecoder[Name] = JsonDecoder[String].map(Name(_))

  implicit val fqNameEncoder: JsonEncoder[FQName] = JsonEncoder[String].contramap(_.toString)
  implicit val fqNameDecoder: JsonDecoder[FQName] = JsonDecoder[String].map(FQName.fromString)

  implicit val labelEncoder: JsonEncoder[Label] = JsonEncoder[String].contramap(_.value)
  implicit val labelDecoder: JsonDecoder[Label] = JsonDecoder[String].map(Label(_))

  implicit val enumLabelEncoder: JsonEncoder[EnumLabel] = DeriveJsonEncoder.gen[EnumLabel]
  implicit val enumLabelDecoder: JsonDecoder[EnumLabel] = DeriveJsonDecoder.gen[EnumLabel]

  implicit val dataEncoder: JsonEncoder[Data] = DeriveJsonEncoder.gen[Data]
  implicit val dataDecoder: JsonDecoder[Data] = DeriveJsonDecoder.gen[Data]

  implicit val conceptEncoder: JsonEncoder[Concept] = DeriveJsonEncoder.gen[Concept]
  implicit val conceptDecoder: JsonDecoder[Concept] = DeriveJsonDecoder.gen[Concept]

  def toJson(any: Any, detailLevel: DetailLevel = DetailLevel.BirdsEye, fieldNames: FieldNames = FieldNames.Hide): Json = {
    any match {
      case name: Name => name.toJsonAST.getOrElse(Json.Null)
      case fqName: FQName => fqName.toJsonAST.getOrElse(Json.Null)
      case label: Label => label.toJsonAST.getOrElse(Json.Null)
      case data: Data => dataToJson(data, detailLevel)
      case concept: Concept => conceptToJson(concept, detailLevel)
      case other => Json.Str(other.toString)
    }
  }

  def fromJson[T: JsonDecoder](json: String): Either[String, T] = {
    json.fromJson[T]
  }

  private def dataToJson(data: Data, detailLevel: DetailLevel): Json = {
    data match {
      case v: Data.Basic[_] => Json.Str(v.toString)
      case v: Data.Case => Json.Obj(
        "enumLabel" -> Json.Str(v.enumLabel.toString),
        "values" -> Json.Arr(v.values.map {
          case (enumLabel, data) => Json.Obj(enumLabel.toString -> dataToJson(data, detailLevel))
        }: _*)
      )
      case v: Data.Tuple => Json.Arr(v.values.map(dataToJson(_, detailLevel)): _*)
      case v: Data.Struct => Json.Obj(v.values.map { case (k, v) => k.value -> dataToJson(v, detailLevel) }: _*)
      case v: Data.Record => Json.Obj(v.values.map { case (k, v) => k.value -> dataToJson(v, detailLevel) }: _*)
      case v: Data.Optional => v match {
        case Optional.Some(data, _) => dataToJson(data, detailLevel)
        case Optional.None(_) => Json.Null
      }
      case v: Data.Result => v match {
        case Result.Ok(data, _) => dataToJson(data, detailLevel)
        case Result.Err(data, _) => dataToJson(data, detailLevel)
      }
      case v: Data.List => Json.Arr(v.values.map(dataToJson(_, detailLevel)): _*)
      case v: Data.Map => Json.Obj(v.values.map { case (k, v) => k.toString -> dataToJson(v, detailLevel) }: _*)
      case v: Data.Set => Json.Arr(v.values.toList.map(dataToJson(_, detailLevel)): _*)
      case v: Data.Union => dataToJson(v.value, detailLevel)
      case v: Data.Aliased => dataToJson(v.data, detailLevel)
    }
  }

  private def conceptToJson(concept: Concept, detailLevel: DetailLevel): Json = {
    concept match {
      case v: Concept.Basic[_] => Json.Str(printName(v))
      case v @ Concept.Any => Json.Str(printName(v))
      case v: Concept.Record => Json.Obj(v.fields.map {
        case (label, value) => label.value -> (if (detailLevel.hideInnerConcepts) Json.Str(label.value) else conceptToJson(value, detailLevel))
      }: _*)
      case v: Concept.Struct => Json.Arr(v.fields.map(_._1.value).map(Json.Str.apply): _*)
      case v: Concept.Alias => Json.Obj(
        "name" -> Json.Str(printName(v)),
        "value" -> conceptToJson(v.value, detailLevel)
      )
      case v: Concept.List => conceptToJson(v.elementType, detailLevel)
      case v: Concept.Map => Json.Obj(
        "keyType" -> conceptToJson(v.keyType, detailLevel),
        "valueType" -> conceptToJson(v.valueType, detailLevel)
      )
      case v: Concept.Set => conceptToJson(v.elementType, detailLevel)
      case v: Concept.Tuple => Json.Arr(v.values.map(conceptToJson(_, detailLevel)): _*)
      case v: Concept.Optional => conceptToJson(v.elementType, detailLevel)
      case v: Concept.Result => Json.Obj(
        "errType" -> conceptToJson(v.errType, detailLevel),
        "okType" -> conceptToJson(v.okType, detailLevel)
      )
      case v: Concept.Enum => Json.Arr(v.cases.map { enumCase =>
        Json.Obj(
          "label" -> Json.Str(enumCase.label.value),
          "fields" -> Json.Obj(enumCase.fields.map {
            case (enumLabel, data) => enumLabel.toString -> dataToJson(data, detailLevel)
          }.toList: _*)
        )
      }.toList: _*)
      case v: Concept.Union => Json.Arr(v.cases.map(conceptToJson(_, detailLevel)).toList: _*)
    }
  }

  private def printName(concept: Concept): String = concept match {
    case v: Concept.Basic[_] => v.getClass.getSimpleName
    case v @ Concept.Any => "Any"
    case v: Concept.Record => v.getClass.getSimpleName
    case v: Concept.Struct => v.getClass.getSimpleName
    case v: Concept.Alias => v.name.toString
    case v: Concept.List => v.getClass.getSimpleName
    case v: Concept.Map => v.getClass.getSimpleName
    case v: Concept.Set => v.getClass.getSimpleName
    case v: Concept.Tuple => v.getClass.getSimpleName
    case v: Concept.Optional => v.getClass.getSimpleName
    case v: Concept.Result => v.getClass.getSimpleName
    case v: Concept.Enum => v.getClass.getSimpleName
    case v: Concept.Union => v.getClass.getSimpleName
  }

  def prettyPrint(any: Any, detailLevel: DetailLevel = DetailLevel.BirdsEye, fieldNames: FieldNames = FieldNames.Hide): String = {
    toJson(any, detailLevel, fieldNames).toJsonPretty
  }

  def compactPrint(any: Any, detailLevel: DetailLevel = DetailLevel.BirdsEye, fieldNames: FieldNames = FieldNames.Hide): String = {
    toJson(any, detailLevel, fieldNames).toJson
  }

  sealed trait DetailLevel {
    def hideFQNames: Boolean
    def compressNestedConcepts: Boolean
    def compressData: Boolean
    def compressConcept: Boolean
    def hideInnerConcepts: Boolean
  }

  object DetailLevel {
    object Detailed extends DetailLevel {
      def hideFQNames            = false
      def compressNestedConcepts = false
      def compressData           = false
      def compressConcept        = false
      def hideInnerConcepts      = false
    }
    object Medium extends DetailLevel {
      def hideFQNames            = true
      def compressNestedConcepts = false
      def compressData           = false
      def compressConcept        = true
      def hideInnerConcepts      = false
    }
    object BirdsEye extends DetailLevel {
      def hideFQNames            = true
      def compressNestedConcepts = false
      def compressData           = true
      def compressConcept        = true
      def hideInnerConcepts      = true
    }

    object BirdsEye2 extends DetailLevel {
      def hideFQNames            = false
      def compressNestedConcepts = false
      def compressData           = true
      def compressConcept        = true
      def showInnerConcepts      = false
      def hideInnerConcepts      = true
    }

    object BirdsEye3 extends DetailLevel {
      def hideFQNames            = false
      def compressNestedConcepts = false
      def compressData           = true
      def compressConcept        = true
      def showInnerConcepts      = false
      def hideInnerConcepts      = false
    }
  }

  sealed trait FieldNames
  object FieldNames {
    object Show extends FieldNames
    object Hide extends FieldNames
  }
}

