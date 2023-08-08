package org.finos.morphir
object naming
    extends FQNameExports
    with ModuleNameExports
    with NameExports
    with NamespaceExports
    with NodeIDExports
    with PathExports
    with PackageNameExports
    with QualifiedModuleNameExports
    with QNameExports {

  final implicit class PackageNameSyntax(val self: PackageName) extends AnyVal {
    def /(moduleName: ModuleName): QualifiedModuleName = QualifiedModuleName(self, moduleName)
  }

  final implicit class QualifiedModuleNameSyntax(val self: QualifiedModuleName) extends AnyVal {
  }

  final implicit class NameHelper(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Name = {
      val interlaced = interlace(sc.parts, args.map(_.toString))
      Name.fromString(interlaced.mkString)
    }
  }

  private[morphir] def interlace[T](a: Iterable[T], b: Iterable[T]): List[T] =
    if (a.isEmpty) b.toList
    else a.head +: interlace(b, a.tail)
}
