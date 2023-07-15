package org.finos.morphir.universe
import zio.prelude.*

package object ir {

  type UType = ir.Type[scala.Unit]
  val UType: ir.Type.type = ir.Type

  /**
   * A module name is a unique identifier for a module within a package. It is represented by a pth, which is a list of
   * names.
   */
  type ModuleName = ModuleName.Type

  object ModuleName extends Subtype[Path] {

    def apply(path: Path, name: Name): ModuleName = ModuleName(path / name)

    def apply(input: String): ModuleName =
      ModuleName(Path(input.split('.').map(Name.fromString).toList))

    def fromString(input: String): ModuleName = ModuleName(input)

    implicit class ModuleNameOps(val self: ModuleName) extends AnyVal {
      @inline def localName: Name = name

      def name: Name =
        self match {
          case ModuleName(Path(Nil))      => Name.empty
          case ModuleName(Path(segments)) => segments.last
        }

      def namespace: Namespace = Namespace(Path(self.value.toList.dropRight(1)))

      def toPath: Path = unwrap(self)

      def value: Path = unwrap(self)
    }
  }

  type ModulePath = ModulePath.Type

  object ModulePath extends Subtype[Path] {
    def apply(parts: Name*): ModulePath = wrap(Path.fromList(parts.toList))

    def fromString(str: String): ModulePath = wrap(Path.fromString(str))

    implicit class ModulePathOps(val modulePath: ModulePath) extends AnyVal {
      def /(name: Name): ModuleName = ModuleName(modulePath.value / name)

      @inline def toPath: Path = modulePath.value

      @inline def value: Path = unwrap(modulePath)
    }
  }

  type Namespace = Namespace.Type

  object Namespace extends Subtype[Path] {
    def apply(parts: Name*): Namespace = Namespace(Path.fromList(parts.toList))

    implicit class NamespaceOps(val namespace: Namespace) extends AnyVal {
      def /(name: Name): ModuleName = ModuleName(namespace.value / name)

      @inline def toPath: Path = namespace.value

      @inline def value: Path = unwrap(namespace)
    }
  }

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

  object Package {

    import org.finos.morphir.universe.ir.AccessControlled
    import org.finos.morphir.universe.ir.Name
    import org.finos.morphir.universe.ir.Path

  }
}
