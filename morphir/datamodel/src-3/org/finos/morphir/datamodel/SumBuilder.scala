package org.finos.morphir.datamodel

import scala.reflect.ClassTag

private[datamodel] case class SumBuilder(tpe: SumBuilder.SumType, variants: List[SumBuilder.Variant]) {
  private def failInsideNotProduct(derivedAs: Any) =
    throw new IllegalArgumentException(
      s"Inner enum data (for: ${tpe}) is only allowed to come from a Scala product but it was derived as: $derivedAs"
    )

  lazy val enumType =
    tpe match {
      case SumBuilder.SumType.Enum(name) =>
        val enumCases =
          variants.map { v =>
            v match {
              // case object is represented as a case with no fields
              case variant: SumBuilder.EnumSingleton =>
                Concept.Enum.Case(Label(v.enumLabel), List())

              case variant: SumBuilder.EnumProduct =>
                val enumVariantFields =
                  variant.deriver.concept match {
                    case Concept.Record(fields) =>
                      fields.map { case (label, concept) => (EnumLabel(label.value), concept) }
                    case other =>
                      failInsideNotProduct(other)
                  }
                Concept.Enum.Case(Label(v.enumLabel), enumVariantFields)
            }

          }
        Concept.Enum(name, enumCases)
    }

  def run(value: Any, tag: Option[ClassTag[Any]] = None): Data = {
    val usedVariant =
      tag match {
        case Some(tag) =>
          variants.find(v => tag <:< v.tag) match {
            case Some(value) => value
            case None =>
              val varNames = variants.map(v => s"${v.enumLabel}:${v.tag.runtimeClass.getName}")
              throw new IllegalArgumentException(
                s"Cannot decode instance of ${tag.runtimeClass.getName} (value was ${value}:${value.getClass.getSimpleName}) becase it was not any of the possibilities: ${varNames}"
              )
          }
        // If we don't have a class-tag need to rely on JVM-level reflection (not sure this is implemented in ScalaJS)
        case None =>
          val cls = value.getClass
          variants.find(v => cls.isAssignableFrom(v.tag.runtimeClass)) match {
            case Some(value) => value
            case None =>
              val varNames = variants.map(v => s"${v.enumLabel}:${v.tag.runtimeClass.getName}")
              throw new IllegalArgumentException(
                s"Cannot decode instance of ${value}:${cls.getName} because it was not a sub-type of any of the possibilities: ${varNames}"
              )
          }
      }

    val enumValues =
      usedVariant match {
        // for a enum case object, data-type is just a 'unit'
        case SumBuilder.EnumSingleton(enumLabel, tag) =>
          List()

        case v: SumBuilder.EnumProduct =>
          value match {
            case p: Product =>
              val enumCaseRecord = v.deriver.derive(p)
              enumCaseRecord match {
                case Data.Record(values) =>
                  values.map { case (label, data) => (EnumLabel(label.value), data) }
                case other =>
                  failInsideNotProduct(other)
              }
            case other => throw new IllegalArgumentException(
                s"The value ($value) for the enum variant ${v.enumLabel} must be a scala product type (case class or multi-field enum) but it was a ${other.getClass}"
              )
          }
      }

    tpe match {
      case SumBuilder.SumType.Enum(name) =>
        Data.Case(enumValues, usedVariant.enumLabel, enumType)
    }
  }
}
object SumBuilder {
  sealed trait Variant {
    def tag: ClassTag[Any]
    def enumLabel: java.lang.String
  }
  sealed trait EnumVariant extends Variant
  // case object variant of a sealed trait or a enum case with no fields
  case class EnumSingleton(enumLabel: java.lang.String, tag: ClassTag[Any])
      extends EnumVariant
  // case class variant of sealed trait or enum case with fields
  case class EnumProduct(enumLabel: java.lang.String, tag: ClassTag[Any], deriver: GenericProductDeriver[Product])
      extends EnumVariant

  // for generic sums
  case class SumVariant(enumLabel: java.lang.String, tag: ClassTag[Any], deriver: Deriver[Any]) extends Variant

  sealed trait SumType
  object SumType {
    case class Enum(name: java.lang.String) extends SumType
    // TODO Union for non-discrimited unions
  }
}
