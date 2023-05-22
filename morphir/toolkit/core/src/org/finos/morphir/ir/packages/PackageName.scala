package org.finos.morphir.ir.packages

import org.finos.morphir.ir.module._
import org.finos.morphir.ir.{FQName, Path}

final case class PackageName(toPath: Path) { self =>
  def %(modulePath: ModulePath): PackageAndModulePath = PackageAndModulePath(self, modulePath)
  def %(moduleName: QualifiedModuleName): FQName =
    FQName(self, ModulePath(moduleName.namespace), moduleName.localName)

  def %(modulePath: String): PackageAndModulePath = PackageAndModulePath(self, ModulePath.fromString(modulePath))
}

object PackageName {
  def fromString(input: String): PackageName = PackageName(Path.fromString(input))
}
