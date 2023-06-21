package org.finios.morphir.datamodel

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline, error, codeOf}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Label

private[datamodel] sealed trait Stage {
  def run(parent: Product): Data
}
private[datamodel] sealed trait FieldStage extends Stage {
  def field: String
}
private[datamodel] object Stage {
  case class FieldLeaf(field: String, index: Int, deriver: SpecificDeriver[Any]) extends FieldStage {
    def run(parent: Product) = deriver.derive(parent.productElement(index))
  }
  case class FieldProduct(field: String, index: Int, deriver: GenericProductDeriver[Product]) extends FieldStage {
    def run(parent: Product) =
      deriver.derive {
        val derived = parent.productElement(index)
        derived match {
          case p: Product => p
          case _ =>
            throw new IllegalArgumentException(
              s"The derived output element ${derived} at index: $index was not a product-type, ${derived.getClass()}"
            )
        }

      }
  }
  case class MirrorProduct(fields: List[FieldStage]) extends Stage {
    def run(parent: Product) =
      Data.Record(fields.map(f => (Label(f.field), f.run(parent))))
  }
}
