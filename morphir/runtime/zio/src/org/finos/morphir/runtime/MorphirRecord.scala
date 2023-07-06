package org.finos.morphir
package runtime

import ir.Name
import EvaluationEngine._
import scala.collection.immutable.{ListMap, SeqMap}
import scala.language.dynamics

sealed trait MorphirObject

class MorphirRecord(private val data: SeqMap[Name, EvalResult], private val recordTypeName: Option[String] = None)
    extends MorphirObject
    with Product
    with Serializable
    with scala.Dynamic {

  lazy val orderedData = data.toSeq

  override def canEqual(other: Any): Boolean = other.isInstanceOf[MorphirRecord]

  override def equals(other: Any): Boolean = other match {
    case that: MorphirRecord =>
      that.canEqual(this) &&
      this.data == that.data &&
      this.recordTypeName == that.recordTypeName
    case _ => false
  }

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(data -> recordTypeName)

  def productArity: Int = data.size

  def getField(name: Name): Option[EvalResult] = data.get(name)

  def setField(key: Name, value: EvalResult): MorphirRecord =
    if (data.contains(key))
      MorphirRecord(data + (key -> value), recordTypeName)
    else
      this

  def setFields(fieldsMap: Map[Name, EvalResult]): MorphirRecord = {
    val fields = fieldsMap.filter(field => data.contains(field._1))
    MorphirRecord(data ++ fields, recordTypeName)
  }

  private def getElement(n: Int): (Name, EvalResult) =
    if (n >= 0 && n < productArity)
      orderedData(n)
    else
      throw new IndexOutOfBoundsException(s"$n is out of bounds (min 0, max ${productArity - 1})")

  def productElement(n: Int): Any = getElement(n)._2

  override def productElementName(n: Int): String = getElement(n)._1.toCamelCase

  override def productPrefix: String = recordTypeName.getOrElse("")

  // Temporary implementation - should be rewritten using macros which do the checks at compile time
  def selectDynamic(name: String): EvalResult = data
    .getOrElse(Name.fromString(name), throw new UnsupportedOperationException(s"Field $name not found"))

  // Temporary implementation - should be rewritten using macros which do the checks at compile time
  def applyDynamic(name: String)(args: Any*): MorphirRecord = name match {
    case "copy" if args.isEmpty => MorphirRecord(data, recordTypeName)
    case "copy" =>
      throw new UnsupportedOperationException(s"Invalid parameters: ${args.mkString("[", ", ", "]")} in copy method")
    case other =>
      throw new UnsupportedOperationException(s"$other not supported in record $productPrefix")
  }

  // Temporary implementation - should be rewritten using macros which do the checks at compile time
  def applyDynamicNamed(name: String)(args: (String, Any)*): MorphirRecord = name match {
    case "copy" =>
      val (fields, unknownFields) = args
        .map(f => Name.fromString(f._1) -> f._2)
        .partition(field => data.contains(field._1))

      if (unknownFields.isEmpty)
        MorphirRecord(data ++ fields.toMap, recordTypeName)
      else
        throw new UnsupportedOperationException(
          s"Method(s) ${unknownFields.map(_._1).mkString("[", ", ", "]")} not found in record $productPrefix"
        )
    case other =>
      throw new UnsupportedOperationException(s"$other not supported in record $productPrefix")
  }
}

object MorphirRecord {
  def apply(data: SeqMap[Name, EvalResult]) = new MorphirRecord(data)
  def apply(data: Seq[(Name, EvalResult)])  = new MorphirRecord(ListMap.from(data))

  def apply(data: SeqMap[Name, EvalResult], recordTypeName: Option[String]) = new MorphirRecord(data, recordTypeName)
  def apply(data: Seq[(Name, EvalResult)], recordTypeName: Option[String]) =
    new MorphirRecord(ListMap.from(data), recordTypeName)
}
