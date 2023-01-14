package org.finos.morphir

import org.finos.morphir.prelude.*

package object ir {

  object Package {

    import org.finos.morphir.ir.Name.Name
    import org.finos.morphir.ir.Path.Path

    /**
     * A package name is a globally unique identifier for a package. It is represented by a `Path` which is a list of
     * names.
     */
    type PackageName = PackageName.Type

    object PackageName extends Subtype[Path] {
      def apply(firstPart: Name, rest: Name*): PackageName     = wrap(Path(firstPart :: rest.toList))
      def apply(firstPart: String, rest: String*): PackageName = wrap(Path((firstPart :: rest.toList).map(Name(_))))

      def fromPath(path: Path): PackageName = wrap(path)

      def fromString(str: String): PackageName = wrap(Path.fromString(str))

      implicit class PackageNameOps(val self: PackageName) extends AnyVal {
        def value: Path = unwrap(self)

        def toPath: Path = unwrap(self)
      }
    }

    final case class Specification[+TA](modules: Map[Module.ModuleName, Module.Specification[TA]]) {
      def map[B](f: TA => B): Specification[B] = Specification(modules.map { case (k, v) => k -> v.map(f) })
    }
  }
}
