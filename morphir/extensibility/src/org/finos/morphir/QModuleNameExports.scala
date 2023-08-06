package org.finos.morphir

private[morphir] trait QModuleNameExports { self: NamingErrorExports with PathExports =>

  sealed case class QModuleName(packageName: Path, module: Path) {
    lazy val toPath: Path     = packageName / module
    def toTuple: (Path, Path) = (packageName, module)
  }

  object QModuleName {
    object AsTuple {
      def unapply(name: QModuleName): Option[(Path, Path)] =
        Some(name.toTuple)
    }
  }
}
