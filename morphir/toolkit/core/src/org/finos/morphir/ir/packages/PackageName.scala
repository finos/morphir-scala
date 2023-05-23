package org.finos.morphir.ir.packages

import org.finos.morphir.ir.module._
import org.finos.morphir.ir.{FQName, Path}

final case class PackageName(toPath: Path) { self =>
  def %(modulePath: ModuleName): PackagedModuleName = PackagedModuleName(self, modulePath)
  def %(moduleName: QualifiedModuleName): FQName =
    FQName(self, ModuleName(moduleName.namespace), moduleName.localName)

  def %(modulePath: String): PackagedModuleName = PackagedModuleName(self, ModuleName.fromString(modulePath))
}

object PackageName {
  def fromString(input: String): PackageName = PackageName(Path.fromString(input))
}
