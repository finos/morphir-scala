package org.finos.morphir.ir.packages
import org.finos.morphir.ir.module.{ModuleName, QualifiedModuleName, Specification => ModuleSpec}
import org.finos.morphir.ir.Value.{Specification => ValueSpec}
import org.finos.morphir.ir.{Name, Path}

/**
 * Type that represents a package specification. A package specification only contains types that are exposed publicly
 * and type signatures for values that are exposed publicly.
 */
final case class Specification[+TA](modules: Map[ModuleName, ModuleSpec[TA]]) {
  self =>

  def eraseAttributes: Specification[scala.Unit] = self.mapAttributes(_ => ())

  def lookupModuleSpecification(path: Path): Option[ModuleSpec[TA]] =
    get(ModuleName.fromPath(path))

  def get(moduleName: ModuleName): Option[ModuleSpec[TA]] =
    modules.get(moduleName)

  def lookupTypeSpecification(path: Path, name: Name): Option[ModuleSpec[TA]] =
    lookupTypeSpecification(QualifiedModuleName(path, name))

  def lookupTypeSpecification(moduleName: QualifiedModuleName): Option[ModuleSpec[TA]] =
    modules.get(moduleName)

  def mapAttributes[TB](func: TA => TB): Specification[TB] = Specification(modules.map { case (name, moduleSpec) =>
    (name, moduleSpec.mapAttributes(func))
  })

  def lookupValueSpecification(
      modulePath: Path,
      localName: Name
  ): Option[ValueSpec[TA]] =
    lookupModuleSpecification(modulePath).flatMap(_.lookupValueSpecification(localName))

}

object Specification {
  val empty: Specification[Nothing] = Specification(Map.empty)

  type Raw = Specification[Unit]
  object Raw {
    def apply(modules: Map[ModuleName, ModuleSpec[Unit]]): Raw =
      Specification(modules)
  }
}
