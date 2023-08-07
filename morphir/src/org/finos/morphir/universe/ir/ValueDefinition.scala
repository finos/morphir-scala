package org.finos.morphir.universe.ir
import org.finos.morphir.naming.*

import org.finos.morphir.universe.ir.Literal as Lit
import zio.Chunk
import ValueDefinition.Parameter

final case class ValueDefinition[+TA, +VA](
    inputTypes: Chunk[Parameter[TA, VA]],
    outputType: Type[TA],
    body: Value[TA, VA]
)
object ValueDefinition {
  final case class Parameter[+TA, +VA](name: Name, attributes: VA, `type`: Type[TA]) { self =>
    def map[VA2](f: VA => VA2): Parameter[TA, VA2] =
      self.copy(attributes = f(attributes))

    def mapTypeAttributes[TA2](f: TA => TA2): Parameter[TA2, VA] =
      self.copy(`type` = `type`.map(f))

    def mapValueAttributes[VA2](f: VA => VA2): Parameter[TA, VA2] =
      self.copy(attributes = f(attributes))
  }
  object Parameter {
    implicit def toTuple3[TA, VA](parameter: Parameter[TA, VA]): (Name, VA, Type[TA]) =
      (parameter.name, parameter.attributes, parameter.`type`)
  }
}
