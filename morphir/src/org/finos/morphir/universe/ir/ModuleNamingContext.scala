package org.finos.morphir.universe.ir

final case class ModuleNamingContext(packageName: PackageName, moduleName: ModuleName)

object ModuleNamingContext {
  def apply(moduleName: ModuleName)(implicit packageNamingContext: PackageNamingContext): ModuleNamingContext =
    ModuleNamingContext(packageNamingContext.packageName, moduleName)
}
