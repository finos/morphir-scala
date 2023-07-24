package org.finos.morphir.universe.ir

import zio.prelude.*

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
