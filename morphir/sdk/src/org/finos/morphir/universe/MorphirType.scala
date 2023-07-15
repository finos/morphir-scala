package org.finos.morphir.universe
import org.finos.morphir.universe.ir.{Type, UType}

trait MorphirType[A] {
  def toIRType(value: A): Type[Any]
  final def toUType(value: A): UType = toIRType(value).map(_ => ())
}

object MorphirType {}
