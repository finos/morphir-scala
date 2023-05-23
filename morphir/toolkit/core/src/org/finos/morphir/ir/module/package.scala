package org.finos.morphir.ir

import scala.annotation.nowarn

package object module {

  @nowarn
  final case class ModuleName(toPath: Path) extends AnyVal {
    self =>

    def toQualifiedModuleName: QualifiedModuleName = QualifiedModuleName.fromPath(toPath)

  }

  object ModuleName {
    def fromString(path: String): ModuleName = ModuleName(Path.fromString(path))
  }
}
