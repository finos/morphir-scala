package org.finos.morphir.universe.ir
import org.finos.morphir.universe.ir.Literal as Lit
import zio.Chunk
import ValueSpecification.Parameter

final case class ValueSpecification[+TA](inputs: Chunk[Parameter[TA]], outputs: Type[TA]) {
  self =>
  def map[TB](f: TA => TB): ValueSpecification[TB] =
    ValueSpecification(inputs.map(_.map(f)), outputs.map(f))
}

object ValueSpecification {
  final case class Parameter[+Attribs](name: Name, `type`: Type[Attribs]) { self =>
    @inline def tpe: Type[Attribs]                                 = self.`type`
    def map[Attribs2](f: Attribs => Attribs2): Parameter[Attribs2] = Parameter(name, `type`.map(f))
  }

  object Parameter {
    implicit def fromTuple[Attribs](tuple: (Name, Type[Attribs])): Parameter[Attribs] = Parameter(tuple._1, tuple._2)
  }
}
