package org.finos.morphir.ir

import org.finos.morphir.foundations.*
import Module.ModuleName
import Name.Name
import Path.Path
object Namespace {
  type Namespace = Namespace.Type

  object Namespace extends Subtype[Path] {
    def apply(parts: Name*): Namespace = Namespace(Path.fromList(parts.toList))

    implicit class NamespaceOps(val namespace: Namespace) extends AnyVal {
      def /(name: Name): ModuleName = ModuleName(namespace.value / name)

      @inline def toPath: Path = namespace.value

      @inline def value: Path = unwrap(namespace)
    }
  }
}
