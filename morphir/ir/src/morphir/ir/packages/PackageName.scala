package morphir.ir.packages

import morphir.ir.module._
import morphir.ir.{FQName, Path}

final case class PackageName(toPath: Path) { self =>
  def %(modulePath: ModulePath): PackageAndModulePath = PackageAndModulePath(self, modulePath)
  def %(moduleName: ModuleName): FQName =
    FQName(self, ModulePath(moduleName.namespace), moduleName.localName)

  def %(modulePath: String): PackageAndModulePath = PackageAndModulePath(self, ModulePath.fromString(modulePath))
}

object PackageName {
  def fromString(input: String): PackageName = PackageName(Path.fromString(input))
}
