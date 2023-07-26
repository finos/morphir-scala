package org.finos.morphir.universe
import zio.=!=
import zio.prelude.*

package object ir {

  type UType = RawType
  val UType: RawType.type = RawType

  type RawType = RawType.Type
  object RawType extends Subtype[Type[scala.Unit]]

  type RawTypeInfo = RawTypeInfo.Type
  object RawTypeInfo extends Subtype[TypeInfo[scala.Unit]] {
    def apply[A](typeInfo: TypeInfo[A])(implicit ev: A =!= scala.Unit): RawTypeInfo = wrap(typeInfo.map(_ => ()))

  }

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

  implicit def modulePathToModuleName(modulePath: ModulePath): ModuleName = ModuleName(modulePath.value)
  implicit def moduleNameToModulePath(moduleName: ModuleName): ModulePath = ModulePath(moduleName.value)

  type Namespace = Namespace.Type

  object Namespace extends Subtype[Path] {
    def apply(parts: Name*): Namespace = Namespace(Path.fromList(parts.toList))

    implicit class NamespaceOps(val namespace: Namespace) extends AnyVal {
      def /(name: Name): ModuleName = ModuleName(namespace.value / name)

      @inline def toPath: Path = namespace.value

      @inline def value: Path = unwrap(namespace)
    }
  }

  sealed trait NodePathStep
  object NodePathStep {
    def childByName(input: String): NodePathStep = ChildByName(Name.fromString(input))
    def childByIndex(index: Int): NodePathStep   = ChildByIndex(index)

    final case class ChildByName(name: Name)  extends NodePathStep
    final case class ChildByIndex(index: Int) extends NodePathStep
  }

  type NodePath = NodePath.Type
  object NodePath extends Subtype[List[NodePathStep]] {
    import NodePathStep.*
    val empty: NodePath = wrap(List.empty)

    def apply(first: Name, rest: Name*): NodePath     = wrap((first :: rest.toList).map(ChildByName(_)))
    def apply(first: String, rest: String*): NodePath = wrap((first :: rest.toList).map(childByName(_)))

    @inline def fromIterable(iterable: Iterable[NodePathStep]): NodePath = wrap(iterable.toList)

    def fromString(input: String): NodePath =
      if (input.isEmpty()) empty
      else {
        fromIterable(input.split(":").map { stepString =>
          stepString.toIntOption match {
            case Some(index) => NodePathStep.childByIndex(index)
            case None        => NodePathStep.childByName(stepString)
          }
        })
      }

    def toString(nodePath: NodePath): String =
      if (nodePath.isEmpty) ""
      else {
        nodePath.map {
          case ChildByName(name)   => name.toCamelCase
          case ChildByIndex(index) => index.toString()
        }.mkString("#", ":", "")
      }

    implicit class NodePathOps(val self: NodePath) extends AnyVal {
      @inline def toList: List[NodePathStep] = unwrap(self)
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
