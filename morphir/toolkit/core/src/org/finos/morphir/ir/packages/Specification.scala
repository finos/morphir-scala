package org.finos.morphir.ir.packages
import org.finos.morphir.naming.*
import org.finos.morphir.ir.module.{Specification => ModuleSpec}
import org.finos.morphir.ir.Value.{Specification => ValueSpec}

/**
 * Type that represents a package specification. A package specification only contains types that are exposed publicly
 * and type signatures for values that are exposed publicly.
 */
final case class Specification[+TA](modules: Map[ModuleName, ModuleSpec[TA]]) {
  self =>

  def eraseAttributes: Specification[scala.Unit] = self.mapAttributes(_ => ())

  def lookupModuleSpecification(path: Path): Option[ModuleSpec[TA]] =
    get(ModuleName(path))

  def get(moduleName: ModuleName): Option[ModuleSpec[TA]] =
    modules.get(moduleName)

  def lookupTypeSpecification(modulePath: Path, name: Name): Option[ModuleSpec[TA]] = ???
  /// lookupTypeSpecification(ModuleName(path, name))

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
