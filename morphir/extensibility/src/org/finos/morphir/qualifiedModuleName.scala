package org.finos.morphir

private[morphir] trait QualifiedModuleNameExports { self: PackageNameExports with PathExports with ModuleNameExports =>

  /// A qualified moduule name is a globally unique identifier for a module. It is represented by the combination of a package name and the module name.
  sealed case class QualifiedModuleName(packageName: PackageName, modulePath: ModuleName) {
    def toTuple: (Path, Path) = (packageName.toPath, modulePath.toPath)
  }

  object QModuleName {
    def apply(packageName: Path, modulePath: Path): QualifiedModuleName =
      QualifiedModuleName(PackageName.fromPath(packageName), ModuleName.fromPath(modulePath))
    object AsTuple {
      def unapply(name: QualifiedModuleName): Option[(Path, Path)] =
        Some(name.toTuple)
    }
  }
}
