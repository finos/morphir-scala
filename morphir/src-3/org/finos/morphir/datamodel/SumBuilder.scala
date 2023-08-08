package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.namespacing.LocalName
import org.finos.morphir.naming._
import scala.reflect.ClassTag

private[datamodel] case class SumBuilder(
    tpe: SumBuilder.SumType,
    ordinalGetter: Any => Int,
    variants: List[SumBuilder.Variant]
) {
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
                    case Concept.Record(_, fields) =>
                      fields.map { case (label, concept) => (EnumLabel(label.value), concept) }
                    case other =>
                      failInsideNotProduct(other)
                  }
                Concept.Enum.Case(Label(v.enumLabel), enumVariantFields)

              case variant: SumBuilder.Variant =>
                throw new IllegalArgumentException("Non-Discrimiated union decoding is not supported yet.")
            }

          }
        Concept.Enum(name, enumCases)
    }

  def run(value: Any): Data = {
    val usedVariant = variants(ordinalGetter(value))

    val enumValues =
      usedVariant match {
        // for a enum case object, data-type is just a 'unit'
        case SumBuilder.EnumSingleton(enumLabel) =>
          List()

        case v: SumBuilder.EnumProduct =>
          value match {
            case p: Product =>
              val enumCaseRecord = v.deriver.derive(p)
              enumCaseRecord match {
                case Data.Record(_, values) =>
                  values.map { case (label, data) => (EnumLabel(label.value), data) }
                case other =>
                  failInsideNotProduct(other)
              }
            case other => throw new IllegalArgumentException(
                s"The value ($value) for the enum variant ${v.enumLabel} must be a scala product type (case class or multi-field enum) but it was a ${other.getClass}"
              )
          }

        case v: SumBuilder.Variant =>
          throw new IllegalArgumentException("Non Discrimiated Unions are not supported yet.")
      }

    tpe match {
      case SumBuilder.SumType.Enum(name) =>
        Data.Case(enumValues, usedVariant.enumLabel, enumType)
    }
  }
}
object SumBuilder {
  sealed trait Variant {
    def enumLabel: java.lang.String
  }
  sealed trait EnumVariant extends Variant
  // case object variant of a sealed trait or a enum case with no fields
  case class EnumSingleton(enumLabel: java.lang.String)
      extends EnumVariant
  // case class variant of sealed trait or enum case with fields
  case class EnumProduct(enumLabel: java.lang.String, deriver: GenericProductDeriver[Product])
      extends EnumVariant

  // for generic sums
  case class SumVariant(enumLabel: java.lang.String, deriver: Deriver[Any]) extends Variant

  sealed trait SumType
  object SumType {
    case class Enum(name: FQName) extends SumType
    // TODO Union for non-discrimited unions
  }
}
