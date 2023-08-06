package org.finos.morphir
import scala.annotation.tailrec
import zio.=!=
import zio.prelude.*

object naming
    extends FQNameExports
    with ModuleNameExports
    with NameExports
    with NamespaceExports
    with NamingErrorExports
    with NodeIDExports
    with PathExports
    with PackageNameExports
    with QNameExports {

  implicit class NamespaceOps(val namespace: Namespace) extends AnyVal {
    def /(name: Name): ModuleName = ModuleName(namespace.value / name)

    @inline def toPath: Path = namespace.value

    @inline def value: Path = Namespace.toPath(namespace)
  }

  sealed case class ModuleNamingContext(packageName: PackageName, moduleName: ModuleName)

  object ModuleNamingContext {
    def apply(moduleName: ModuleName)(implicit packageNamingContext: PackageNamingContext): ModuleNamingContext =
      ModuleNamingContext(packageNamingContext.packageName, moduleName)
  }
}
