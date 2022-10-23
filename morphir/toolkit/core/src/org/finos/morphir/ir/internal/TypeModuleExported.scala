package org.finos.morphir
package ir
package internal

trait TypeModuleExported { module =>
  val Type: MorphirTypeModule
  final type Type[+A] = module.Type.Type[A]
  final type UType    = module.Type.UType
}
