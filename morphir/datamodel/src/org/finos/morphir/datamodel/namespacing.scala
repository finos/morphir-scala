package org.finos.morphir.datamodel
import org.finos.morphir.foundations.*

object namespacing {
  type NamespaceSegment = NamespaceSegment.Type
  object NamespaceSegment extends Subtype[String] 
  implicit class NamespaceSegmentOps(val self: NamespaceSegment) extends AnyVal {
    import NamespaceSegment.unwrap
    def value: String = unwrap(self)
  }

  type Namespace = Namespace.Type
  object Namespace extends Newtype[Chunk[NamespaceSegment]] {
    lazy val root: Namespace = Namespace(Chunk.empty)
  }

  final case class Namespaced[+A](namespace:Namespace, value:A)
}
