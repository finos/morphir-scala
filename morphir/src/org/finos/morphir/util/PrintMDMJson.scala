package org.finos.morphir.util

import _root_.org.finos.morphir.naming._
import _root_.org.finos.morphir.datamodel.Label
import _root_.org.finos.morphir.datamodel.EnumLabel
import _root_.org.finos.morphir.datamodel.Data
import _root_.org.finos.morphir.datamodel.Concept
import org.finos.morphir.datamodel.Data.{Optional, Result}
import zio.json.ast._
import zio.json.EncoderOps

object PrintMDMJson {

  def prettyPrint(
      any: Any,
      detailLevel: DetailLevel = DetailLevel.BirdsEye,
      fieldNames: FieldNames = FieldNames.Hide
  ): String =
    toJson(any, detailLevel, fieldNames).toJsonPretty

  def compactPrint(
      any: Any,
      detailLevel: DetailLevel = DetailLevel.BirdsEye,
      fieldNames: FieldNames = FieldNames.Hide
  ): String =
    toJson(any, detailLevel, fieldNames).toJson

  private def toJson(any: Any, detailLevel: DetailLevel, fieldNames: FieldNames): Json =
    any match {
      case name: Name   => Json.Str(name.toTitleCase)
      case qn: FQName   => Json.Str(if (detailLevel.hideFQNames) qn.localName.toTitleCase else qn.toStringTitleCase)
      case label: Label => Json.Str(label.value)
      case v: Data      => dataToJson(v)
      case v: Concept   => conceptToJson(v, detailLevel)
      case other        => Json.Str(other.toString)
    }

  private def dataToJson(data: Data): Json =
    data match {
      case v: Data.Basic[?] => Json.Str(v.toString)
      case v: Data.Case => Json.Obj(
          "enumLabel" -> Json.Str(v.enumLabel),
          "values" -> Json.Arr(v.values.map {
            case (enumLabel, data) => Json.Obj(enumLabel.toString -> dataToJson(data))
          }*)
        )
      case v: Data.Tuple  => Json.Arr(v.values.map(dataToJson)*)
      case v: Data.Struct => Json.Obj(v.values.map { case (k, v) => k.value -> dataToJson(v) }*)
      case v: Data.Record => Json.Obj(v.values.map { case (k, v) => k.value -> dataToJson(v) }*)
      case v: Data.Optional => v match {
          case Optional.Some(data, _) => dataToJson(data)
          case Optional.None(_)       => Json.Null
        }
      case v: Data.Result => v match {
          case Result.Ok(data, _)  => dataToJson(data)
          case Result.Err(data, _) => dataToJson(data)
        }
      case v: Data.List => Json.Arr(v.values.map(dataToJson)*)
      case v: Data.Map => Json.Obj(v.values.map {
          case (k, v) => k.toString -> dataToJson(v)
        }.toList*)
      case v: Data.Set     => Json.Arr(v.values.toList.map(dataToJson)*)
      case v: Data.Union   => dataToJson(v.value)
      case v: Data.Aliased => dataToJson(v.data)

    }

  private def conceptToJson(concept: Concept, detailLevel: DetailLevel): Json =
    concept match {
      case v: Concept.Basic[_] => Json.Str(printName(v))
      case v: Concept.Any.type => Json.Str(printName(v))
      case v: Concept.Record => Json.Obj(v.fields.map {
          case (label, value) => label.value -> (if (detailLevel.hideInnerConcepts) Json.Str(label.value)
                                                 else conceptToJson(value, detailLevel))
        }*)
      case v: Concept.Struct => Json.Arr(v.fields.map(_._1.value).map(Json.Str.apply)*)
      case v: Concept.Alias => Json.Obj(
          "name"  -> Json.Str(printName(v)),
          "value" -> conceptToJson(v.value, detailLevel)
        )
      case v: Concept.List => conceptToJson(v.elementType, detailLevel)
      case v: Concept.Map => Json.Obj(
          "keyType"   -> conceptToJson(v.keyType, detailLevel),
          "valueType" -> conceptToJson(v.valueType, detailLevel)
        )
      case v: Concept.Set      => conceptToJson(v.elementType, detailLevel)
      case v: Concept.Tuple    => Json.Arr(v.values.map(conceptToJson(_, detailLevel))*)
      case v: Concept.Optional => conceptToJson(v.elementType, detailLevel)
      case v: Concept.Result => Json.Obj(
          "errType" -> conceptToJson(v.errType, detailLevel),
          "okType"  -> conceptToJson(v.okType, detailLevel)
        )
      case v: Concept.Union => Json.Arr(v.cases.map(conceptToJson(_, detailLevel))*)
      case v: Concept.Enum => Json.Arr(
          v.cases.map { enumCase =>
            Json.Obj(
              "label" -> Json.Str(enumCase.label.value),
              "fields" -> Json.Obj(enumCase.fields.map {
                case (label, value) => label.toString -> conceptToJson(value, detailLevel)
              }*)
            )
          }*
        )
    }

  private def printName(concept: Concept): String = concept.getNameString match {
    // e.g. $Person:Record(...)
    case Some(value) => "$" + value + ":" + concept.getClass.getSimpleName
    // e.g. $List(...)
    case None => "$" + concept.getClass.getSimpleName
  }

  sealed trait DetailLevel {
    def hideFQNames: Boolean
    def hideInnerConcepts: Boolean
  }

  object DetailLevel {
    object Detailed extends DetailLevel {
      def hideFQNames       = false
      def hideInnerConcepts = false
    }
    object Medium extends DetailLevel {
      def hideFQNames       = true
      def hideInnerConcepts = false
    }
    object BirdsEye extends DetailLevel {
      def hideFQNames       = true
      def hideInnerConcepts = true
    }
    object BirdsEye2 extends DetailLevel {
      def hideFQNames       = false
      def hideInnerConcepts = true
    }
    object BirdsEye3 extends DetailLevel {
      def hideFQNames       = false
      def hideInnerConcepts = false
    }
  }

  sealed trait FieldNames
  object FieldNames {
    object Show extends FieldNames
    object Hide extends FieldNames
  }
}
