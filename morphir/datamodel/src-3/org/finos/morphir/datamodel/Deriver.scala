package org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline, error, codeOf}
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

  inline def toData[T](value: T): Data = {
    import org.finos.morphir.datamodel.Derivers._
    val deriver = Deriver.gen[T]
    deriver.derive(value)
  }

  inline def showType[T]: String = ${ showTypeImpl[T] }
  def showTypeImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].simplified.typeSymbol.name)
  }

  type IsProduct[P <: Product] = P

  inline def summonDeriver[T]: Deriver[T] = ${ summonDeriverImpl[T] }
  def summonDeriverImpl[T: Type](using Quotes): Expr[Deriver[T]] =
    import quotes.reflect._
    val specificDriver = Expr.summon[SpecificDeriver[T]]
    specificDriver match {
      case Some(value) => value
      case None =>
        Type.of[T] match {
          case '[IsProduct[p]] =>
            val genericDeriver = Expr.summon[GenericProductDeriver[p]]
            genericDeriver match {
              case Some(value) => '{ $value.asInstanceOf[Deriver[T]] }
              case _ =>
                report.errorAndAbort(
                  s"Cannot summon specific or generic Deriver for the type: ${TypeRepr.of[T].widen.show}"
                )
            }
          case _ =>
            report.errorAndAbort(
              s"Cannot summon specific Deriver for the type (and it is not a Product): ${TypeRepr.of[T].widen.show}"
            )
        }

    }

  inline def summonSpecificDeriver[T] =
    summonFrom {
      case deriver: SpecificDeriver[T] => deriver
      case _ =>
        error(s"Cannot find specific deriver for type: ${showType[T]}")
    }

  inline def deriveProductFields[Fields <: Tuple, Elems <: Tuple](i: Int): List[FieldStage] =
    inline erasedValue[Fields] match {
      case EmptyTuple => Nil

      case _: (field *: fields) =>
        val fieldName = constValue[field].toString
        inline erasedValue[Elems] match {
          case _: (head *: tail) =>
            val derivationStage =
              summonDeriver[head] match {
                case deriver: SpecificDeriver[Any] =>
                  Stage.FieldLeaf(fieldName, i, deriver)
                case deriver: GenericProductDeriver[Product] =>
                  Stage.FieldProduct(fieldName, i, deriver)
              }
            derivationStage +: deriveProductFields[fields, tail](i + 1)

          case EmptyTuple =>
            error("shuold not be possible")
        }
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
          case m: Mirror.ProductOf[T] =>
            val stageListTuple = deriveProductFields[m.MirroredElemLabels, m.MirroredElemTypes](0)
            val mirrorProduct  = Stage.MirrorProduct(stageListTuple)
            GenericProductDeriver
              .make[T & Product](mirrorProduct)
              .asInstanceOf[Deriver[T]] // not sure why the cast is needed
        }
    }
}

trait GenericProductDeriver[T <: Product] extends Deriver[T] {
  def derive(value: T): Data = stage.run(value)
  def stage: Stage.MirrorProduct
  // def concept: Concept.Record
}
object GenericProductDeriver {
  def make[T <: Product](creationStage: Stage.MirrorProduct) =
    new GenericProductDeriver[T] {
      val stage = creationStage
      val concept: Concept.Record = {
        // Deriver stage contains list of fields and child derivers
        val fields: List[(Label, Concept)] =
          creationStage.fields.map {
            case Stage.FieldLeaf(field, _, deriver) =>
              (Label(field), deriver.concept)
            case Stage.FieldProduct(field, _, deriver) =>
              (Label(field), deriver.concept)
          }
        Concept.Record(fields)
      }
    }

  /**
   * Automatic generator for Product types (and only product types). For anything that is automatically evaluated by the
   * Scala compiler as a implicit (e.g. auto-derivation) we need to be really careful for that not to evaulate for
   * Products and Primitives (and/or Sums) otherwise there is a danger that it will recurse infinately on certain
   * datatypes that are not well-formed. Therefore for products we have a single function that handles derivation only
   * for products and the implicit needed for that (in the Derivers). This is needed for the following purpose.
   *
   * Say that we have a simple case-class hierarchy like this {{ case class Person(name: Name, age: String) case class
   * Name(first: String, last: String)
   *
   * }}
   */
  inline def gen[T <: Product]: GenericProductDeriver[T] =
    summonFrom { case ev: Mirror.Of[T] =>
      inline ev match {
        case m: Mirror.ProductOf[T] =>
          val stageListTuple = Deriver.deriveProductFields[m.MirroredElemLabels, m.MirroredElemTypes](0)
          val mirrorProduct  = Stage.MirrorProduct(stageListTuple)
          GenericProductDeriver
            .make[T & Product](mirrorProduct)
      }
    }
}
