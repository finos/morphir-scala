package org.finos.morphir.datamodel

import org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Label
import org.finos.morphir.datamodel.Concept

trait Deriver[T] {
  def derive(value: T): Data
  def concept: Concept
}

trait SpecificDeriver[T] extends Deriver[T] {
  def derive(value: T): Data
  def concept: Concept
}

object Deriver {
  import DeriverTypes._
  import DeriverMacros._

  inline def toData[T](value: T): Data = {
    import org.finos.morphir.datamodel.Derivers.{given, _}
    val deriver = Deriver.gen[T]
    deriver.derive(value)
  }

  inline def summonSpecificDeriver[T] =
    summonFrom {
      case deriver: SpecificDeriver[T] => deriver
      case _ =>
        error(s"Cannot find specific deriver for type: ${showType[T]}")
    }

  sealed trait UnionType
  object UnionType {
    case object SealedTrait extends UnionType
    case object Enum        extends UnionType
    case object Sum         extends UnionType
  }

  private inline def deriveSumVariants[Fields <: Tuple, Elems <: Tuple](
      inline unionType: UnionType
  ): List[SumBuilder.Variant] =
    inline erasedValue[Fields] match {
      case EmptyTuple => Nil

      case _: (field *: fields) =>
        val fieldName = constValue[field].toString
        // note that no printout done here (e.g. println("hello")) will actually happen at compile-time
        // since this is actually a string that will be spliced in at runtime
        inline erasedValue[Elems] match {
          case _: (head *: tail) =>
            val ct = summonClassTagOrFail[head].asInstanceOf[ClassTag[Any]]
            // need to make sure that ALL of the matches inside here (including the `unionType` match)
            // is inline otherwise very strange things happen! Macros will run for even variants that shouldn't be matching
            // (i.e. because the other side of the case match branch is also running)
            val variant =
              inline unionType match {
                // UnionType.Enum |
                case UnionType.SealedTrait =>
                  // enum case with fields
                  if (isCaseClass[head]) {
                    summonProductDeriver[head] match {
                      case deriver: GenericProductDeriver[Product] =>
                        SumBuilder.EnumProduct(fieldName, ct, deriver)
                      case other =>
                        throw new IllegalArgumentException(
                          "Illegal state, should not be possible, summonProductDeriver always returns a GenericProductDeriver"
                        )
                    }
                  } // enum case without fields
                  else {
                    SumBuilder.EnumSingleton(fieldName, ct)
                  }
                // for the sum-case just do regular recursive derivation
                case UnionType.Sum =>
                  val deriver = summonDeriver[head].asInstanceOf[Deriver[Any]]
                  SumBuilder.SumVariant(fieldName, ct, deriver)
              }

            // return the variant and recurse
            variant +: deriveSumVariants[fields, tail](unionType)

          case EmptyTuple =>
            error("shuold not be possible")
        }
    }

  inline def deriveProductFields[Fields <: Tuple, Elems <: Tuple](i: Int): List[ProductBuilderField] =
    inline erasedValue[Fields] match {
      case EmptyTuple => Nil

      case _: (field *: fields) =>
        val fieldName = constValue[field].toString
        inline erasedValue[Elems] match {
          case _: (head *: tail) =>
            val derivationStage =
              summonDeriver[head] match {
                case deriver: SpecificDeriver[Any] =>
                  ProductBuilder.Leaf(fieldName, i, deriver)
                case deriver: GenericProductDeriver[Product] =>
                  ProductBuilder.Product(fieldName, i, deriver)
                case deriver: GenericSumDeriver[Any] =>
                  ProductBuilder.Sum(fieldName, i, deriver)
              }
            derivationStage +: deriveProductFields[fields, tail](i + 1)

          case EmptyTuple =>
            error("shuold not be possible")
        }
    }

  inline def deriveProductFromMirror[T](m: Mirror.ProductOf[T]): GenericProductDeriver[T & Product] =
    inline if (isCaseClass[T]) {
      val stageListTuple = deriveProductFields[m.MirroredElemLabels, m.MirroredElemTypes](0)
      val mirrorProduct  = ProductBuilder.MirrorProduct(stageListTuple)
      GenericProductDeriver
        .make[T & Product](mirrorProduct)
    } else {
      errorOnType[T]("Cannot summon a generic deriver of he case class type. It is not a valid product type")
    }

  inline def deriveSumFromMirror[T](m: Mirror.SumOf[T]): GenericSumDeriver[T] =
    inline if (isEnumOrSealedTrait[T]) {
      val sumTypeName = DeriverMacros.typeName[T]
      // The clause `inferUnionType` NEEDs to be  a macro otherwise we can't get the value
      // coming out if it to work with inline matches/ifs and if our matches/ifs are not inline
      // and there is a `scala.compiletime.error` command called of a non-inline match/if branch
      // called then it will happen no mater what the input data is because these constructs
      // just do the equivalent of report.errorAndAbort for all inputs. The only way to NOT
      // activate them is to have them on a branch of a inline match/if which is not being called
      val variants =
        deriveSumVariants[m.MirroredElemLabels, m.MirroredElemTypes](inferUnionType[T])

      val builder =
        inline inferUnionType[T] match {
          case UnionType.Enum | UnionType.SealedTrait =>
            SumBuilder(SumBuilder.SumType.Enum(sumTypeName), variants)
          case UnionType.Sum =>
            error("Simple union types not allowed yet in builder synthesis")
        }
      GenericSumDeriver.make[T](builder)
    } else {
      errorOnType[T]("The following type is not a valid enum and there is no specific deriver defined for it")
    }

  // TODO When making a product deriver, make sure to exclude Option[T] since
  //      we want a specific deriver for that, not a generic one.
  inline def gen[T]: Deriver[T] =
    summonFrom {
      // If there is a leaf-level deriver, summon that first. Do NOT EVER try to summon Deriver[T]
      // directly because you will can run into infinite recursive derivation.
      case deriver: SpecificDeriver[T] =>
        deriver
      case ev: Mirror.Of[T] =>
        inline ev match {
          case m: Mirror.ProductOf[IsOption[t]] =>
            error(
              s"Cannot summon a generic derivation of Option[T], a specific encoder is required."
            )

          case m: Mirror.ProductOf[T] =>
            // cast is needed otherwise it's T & Product which doesn't seem to derive correctly
            deriveProductFromMirror[T](m).asInstanceOf[Deriver[T]]
          case m: Mirror.SumOf[T] =>
            deriveSumFromMirror[T](m)
        }
    }
}
