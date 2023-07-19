package org.finos.morphir.universe.ir

final case class PackageNamingContext(packageName: PackageName)
object PackageNamingContext {
  object morphir {
    object sdk {
      implicit val defaultPackageNamingContext: PackageNamingContext =
        PackageNamingContext(PackageName.fromString("Morphir.Sdk"))
    }
  }
}
