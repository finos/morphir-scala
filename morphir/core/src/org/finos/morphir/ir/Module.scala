package org.finos.morphir.ir

import org.finos.morphir.prelude.*
import AccessControlled.AccessControlled
import Documented.Documented
import Name.Name
import Namespace.Namespace
import Package.PackageName
import Path.Path
import Type.Specification as TypeSpec
import Value.Specification as ValueSpec
object Module {

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

  /**
   * A qualified module name is a globally unique identifier for a module. It is represented by the combination of a
   * package name and the module name
   */
  final case class QualifiedModuleName(packageName: PackageName, module: ModuleName) {
    lazy val toPath: Path = packageName / module

    def toTuple: (Path, Path) = (packageName, module)
  }

  object QualifiedModuleName {
    //    object AsTuple {
    //      def unapply(name: QualifiedModuleName): Option[(Path, ModuleName)] =
    //        Some(name.toTuple)
    //    }
  }

  sealed trait ModuleDefOrSpec

  final case class Specification[+TA](
      types: Map[Name, Documented[TypeSpec[TA]]],
      values: Map[Name, Documented[ValueSpec[TA]]]
  ) extends ModuleDefOrSpec {
    def map[TB](f: TA => TB): Specification[TB] = Specification(
      types.view.mapValues(_.map(_.map(f))).toMap,
      values.view.mapValues(_.map(_.map(f))).toMap
    )
  }

  final case class Definition[+TA, +VA](
      types: Map[Name, AccessControlled[Documented[Type.Definition[TA]]]],
      values: Map[Name, AccessControlled[Documented[Value.Definition[TA, VA]]]]
  ) extends ModuleDefOrSpec
}
