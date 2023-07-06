package org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Label

private[datamodel] sealed trait ProductBuilder

private[datamodel] sealed trait ProductBuilderField extends ProductBuilder {
  def run(parent: scala.Product): Data
  def field: String
}
private[datamodel] object ProductBuilder {
  case class Leaf(field: String, index: Int, deriver: SpecificDeriver[Any]) extends ProductBuilderField {
    def run(parent: scala.Product) = deriver.derive(parent.productElement(index))
  }

  private def fail(derived: Any, index: Int) =
    throw new IllegalArgumentException(
      s"The derived output element ${derived} at index: $index was not a product-type, ${derived.getClass()}"
    )

  case class Product(field: String, index: Int, deriver: GenericProductDeriver[scala.Product])
      extends ProductBuilderField {
    def run(parent: scala.Product) =
      deriver.derive {
        val derived = parent.productElement(index)
        derived match {
          case p: scala.Product => p
          case _                => fail(derived, index)
        }
      }
  }
  case class Sum(field: String, index: Int, deriver: GenericSumDeriver[Any]) extends ProductBuilderField {
    def run(parent: scala.Product) =
      deriver.derive {
        val derived = parent.productElement(index)
        derived match {
          case p: scala.Product => p
          case _                => fail(derived, index)
        }
      }
  }

  case class MirrorProduct(fields: List[ProductBuilderField]) extends ProductBuilder {
    def run(parent: scala.Product) =
      Data.Record(fields.map(f => (Label(f.field), f.run(parent))))
  }
}
