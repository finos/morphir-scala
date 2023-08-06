package org.finos.morphir

private[morphir] trait NamingContextExports { self: naming.type =>

  sealed case class ModuleNamingContext(packageName: PackageName, moduleName: ModuleName)

  object ModuleNamingContext {
    def apply(moduleName: ModuleName)(implicit packageNamingContext: PackageNamingContext): ModuleNamingContext =
      ModuleNamingContext(packageNamingContext.packageName, moduleName)
  }

  sealed case class PackageNamingContext(packageName: PackageName)
  object PackageNamingContext {
    object morphir {
      object sdk {
        implicit val defaultPackageNamingContext: PackageNamingContext =
          PackageNamingContext(PackageName.fromString("Morphir.Sdk"))
      }
    }
  }

}
