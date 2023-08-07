package org.finos.morphir

private[morphir] trait NamespaceExports { self: NameExports with PathExports =>
  sealed case class Namespace(path: Path) {
    def ++(name: Namespace): Namespace = Namespace(path ++ path)

    @inline def toPath: Path = path

  }

  object Namespace {
    def apply(parts: Name*): Namespace  = Namespace(Path.fromIterable(parts))
    def fromPath(path: Path): Namespace = Namespace(path)

  }
}
