package org.finos.morphir.universe.ir

import org.finos.morphir.naming._
import zio.Chunk
import org.finos.morphir.universe.ir.Type.{Unit => UnitType, _}

final case class TypeMapReferenceName[Attrib](f: FQName => FQName) extends TypeRewritingFolder[Any, Attrib] {
  override def referenceCase(
      context: Any,
      tpe: Type[Attrib],
      attributes: Attrib,
      typeName: FQName,
      typeParams: List[Type[Attrib]]
  ): Type[Attrib] =
    Reference(attributes, f(typeName), typeParams)

}
