package org.finos.morphir.ir.packages

import org.finos.morphir.ir.module._
import org.finos.morphir.ir.{FQName, Name, Path}
import org.finos.morphir.datamodel.namespacing.{PackageName => Pack}

final case class PackageName(toPath: Path) { self =>
  def %(moduleName: ModuleName): PackagedModuleName = PackagedModuleName(self, moduleName)
  def %(moduleName: QualifiedModuleName): FQName =
    FQName(self, ModuleName(moduleName.namespace), moduleName.localName)

  def %(modulePath: String): PackagedModuleName = PackagedModuleName(self, ModuleName.fromString(modulePath))
}

object PackageName {
  def apply(pack: Pack): PackageName =
    PackageName(Path.fromIterable(pack.segments.map { seg => Name.fromString(seg) }))

  def fromString(input: String): PackageName = PackageName(Path.fromString(input))

}
