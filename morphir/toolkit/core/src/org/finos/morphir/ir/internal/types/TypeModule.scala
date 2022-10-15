package org.finos.morphir
package ir
package internal
package types

trait TypeModule extends AllTypeSyntax {
  final type Type[+A] = types.Type[A]
  final val Type = types.Type

  final type UType = types.Type.UType
  final val UType = types.Type.UType
}

object TypeModule extends TypeModule
