package org.finos.morphir
package ir
package internal

trait MorphirTypeModuleBase { module =>
  type Type[+A] <: ir.Type.Type[A]
  final type UType = Type[scala.Unit] // TODO: Change to Type[Unit]
}
