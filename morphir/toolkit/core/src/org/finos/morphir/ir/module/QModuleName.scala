package org.finos.morphir.ir.module
import org.finos.morphir.ir.Path

final case class QModuleName(packageName: Path, module: Path) {
  lazy val toPath: Path     = packageName / module
  def toTuple: (Path, Path) = (packageName, module)
}

object QModuleName {
  object AsTuple {
    def unapply(name: QModuleName): Option[(Path, Path)] =
      Some(name.toTuple)
  }
}
