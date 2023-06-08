package org.finos.morphir.ir.packages
import org.finos.morphir.ir.{FQName, ModuleName, Name}

final case class PackagedModuleName(packageName: PackageName, moduleName: ModuleName) {
  self =>
  def %(name: Name): FQName = FQName(packageName, moduleName, name)

  def %(name: String): FQName = FQName(packageName, moduleName, Name.fromString(name))
}
