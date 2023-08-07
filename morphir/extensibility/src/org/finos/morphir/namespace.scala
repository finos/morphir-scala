package org.finos.morphir

private[morphir] trait NamespaceExports { self: NameExports with PathExports with ModuleNameExports =>
  sealed case class Namespace(path: Path) {
    def ++(name: Namespace): Namespace = Namespace(path ++ path)
    def /(segment: String): Namespace  = Namespace(path ++ Path.fromString(segment))

    @inline def toPath: Path                                       = path
    def parts(implicit renderer: PathRenderer): IndexedSeq[String] = path.parts

    def render(implicit renderer: PathRenderer): String = renderer(path)
    /// An alias for `render`
    def show(implicit renderer: PathRenderer): String = render
    def toModuleName: ModuleName                      = ModuleName(path)

    override def toString(): String = render
  }

  object Namespace {
    val ns: Namespace = Namespace(Path.empty)

    def apply(parts: Name*): Namespace         = Namespace(Path.fromIterable(parts))
    def fromIterable(segments: Iterable[Name]) = Namespace(Path.fromIterable(segments))
    def fromModuleName(moduleName: ModuleName) = Namespace(moduleName.path)
    def fromPath(path: Path): Namespace        = Namespace(path)

  }
}
