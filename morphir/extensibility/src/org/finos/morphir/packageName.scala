package org.finos.morphir
import zio.prelude.*
private[morphir] trait PackageNameExports { self: naming.type =>

  /**
   * A package name is a globally unique identifier for a package. It is represented by a `Path` which is a list of
   * names.
   */
  type PackageName = PackageName.Type

  object PackageName extends Subtype[Path] {
    val empty: PackageName                                   = wrap(Path.empty)
    def apply(firstPart: Name, rest: Name*): PackageName     = wrap(Path.fromIterable(firstPart +: rest))
    def apply(firstPart: String, rest: String*): PackageName = wrap(Path.fromIterable((firstPart +: rest).map(Name(_))))

    def fromPath(path: Path): PackageName = wrap(path)

    def fromString(str: String): PackageName = wrap(Path.fromString(str))

    implicit class PackageNameOps(val self: PackageName) {
      def value: Path = unwrap(self)

      def toPath: Path = unwrap(self)
    }
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
