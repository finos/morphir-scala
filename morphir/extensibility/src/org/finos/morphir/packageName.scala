package org.finos.morphir
private[morphir] trait PackageNameExports { self: naming.type =>

  /**
   * A package name is a globally unique identifier for a package. It is represented by a `Path` which is a list of
   * names.
   */
  sealed case class PackageName(path: Path) { self =>
    def ++(that: PackageName): PackageName = PackageName(path ++ that.path)
    def ++(that: Path): PackageName        = PackageName(path ++ that)
    def /(pathString: String)              = PackageName(path ++ Path.fromString(pathString))

    @inline def isEmpty: Boolean = path.isEmpty
    @inline def toPath: Path     = path

    def render(implicit renderer: PathRenderer): String = renderer(path)
    /// An alias for `render`
    def show(implicit renderer: PathRenderer): String = render
    override def toString(): String                   = render
  }

  object PackageName {
    val empty: PackageName = PackageName(Path.empty)
    val root: PackageName  = PackageName(Path.empty)

    def fromPath(path: Path): PackageName = PackageName(path)

    def fromString(str: String): PackageName = PackageName(Path.fromString(str))
    def fromIterable(segements: Iterable[Name]): PackageName =
      PackageName(Path.fromIterable(segements))
    def fromList(segments: List[Name]): PackageName =
      PackageName(Path.fromList(segments))

  }
}
