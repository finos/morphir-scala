package org.finos.morphir.mir.packages
import org.finos.morphir.mir.{FQName, ModulePath, Name}

final case class PackageAndModulePath(packageName: PackageName, modulePath: ModulePath) {
  self =>
  def %(name: Name): FQName = FQName(packageName, modulePath, name)

  def %(name: String): FQName = FQName(packageName, modulePath, Name.fromString(name))
}
