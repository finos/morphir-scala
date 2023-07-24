package org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}

trait GenericSumDeriver[T] extends Deriver[T] {
  def derive(value: T): Data =
    builder.run(value)
  def deriveWithTag(value: T)(implicit ct: ClassTag[T]): Data =
    builder.run(value, Some(ct.asInstanceOf[ClassTag[Any]]))
  def builder: SumBuilder
}
object GenericSumDeriver {
  def make[T](sumBuilder: SumBuilder) =
    new GenericSumDeriver[T] {
      val builder = sumBuilder
      // For generic-sum, most of the type-compuation logic lives inside of the builder
      def concept = sumBuilder.enumType
    }

  inline def gen[T]: GenericSumDeriver[T] =
    summonFrom { case m: Mirror.SumOf[T] => Deriver.deriveSumFromMirror[T](m) }

}
