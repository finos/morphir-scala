package org.finos.morphir

import scala.annotation.tailrec
import zio.=!=
import zio.prelude.*

private[morphir] trait NamespaceExports { self: NameExports with PathExports =>
  type Namespace = Namespace.Type

  object Namespace extends Subtype[Path] {
    def apply(parts: Name*): Namespace  = Namespace(Path.fromList(parts.toList))
    def fromPath(path: Path): Namespace = wrap(path)

    def toPath(namespace: Namespace): Path = unwrap(namespace)
  }
}
