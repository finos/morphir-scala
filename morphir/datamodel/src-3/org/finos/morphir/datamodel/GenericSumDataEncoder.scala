package org.finos.morphir.datamodel

import scala.reflect.ClassTag
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}

trait GenericSumDataEncoder[T] extends DataEncoder[T] {
  def encode(value: T): Data =
    builder.run(value)
  def deriveWithTag(value: T)(implicit ct: ClassTag[T]): Data =
    builder.run(value, Some(ct.asInstanceOf[ClassTag[Any]]))
  def builder: SumBuilder
}
object GenericSumDataEncoder {
  def make[T](sumBuilder: SumBuilder) =
    new GenericSumDataEncoder[T] {
      val builder = sumBuilder
      // For generic-sum, most of the type-compuation logic lives inside of the builder
      def concept = sumBuilder.enumType
    }

  inline def gen[T]: GenericSumDataEncoder[T] =
    summonFrom { case m: Mirror.SumOf[T] => DataEncoder.deriveSumFromMirror[T](m) }

}
