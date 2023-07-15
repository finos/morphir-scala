package org.finos.morphir.datamodel
import org.finos.morphir.foundations.capabilities.*
import zio.prelude.*

object namespacing {

  def localName(name: String): LocalName = LocalName(name)

  type NamespaceSegment = NamespaceSegment.Type
  object NamespaceSegment extends Subtype[String] {
    implicit class NamespaceSegmentOps(val self: NamespaceSegment) extends AnyVal {
      def value: String = unwrap(self)
    }
  }

  type ValidNamespaceSegment = ValidNamespaceSegment.Type
  object ValidNamespaceSegment extends Subtype[String] {
    implicit class ValidNamespaceSegmentOps(val self: ValidNamespaceSegment) extends AnyVal {
      def value: String = unwrap(self)
    }
  }

  type Namespace = Namespace.Type
  object Namespace extends Newtype[List[NamespaceSegment]] {
    lazy val root: Namespace = Namespace(List.empty)

    implicit val showInstance: Show[Namespace] = ns => ns.segments.map(_.value).mkString(".")

    def fromIterable(segments: Iterable[NamespaceSegment]): Namespace =
      Namespace(segments.toList)

    implicit class NamespaceOps(val self: Namespace) extends AnyVal {
      def segments: List[NamespaceSegment]        = unwrap(self)
      def /(segment: NamespaceSegment): Namespace = Namespace(unwrap(self) :+ segment)
      def /(namespace: Namespace): Namespace      = Namespace(unwrap(self) ++ unwrap(namespace))
      // def /(name: String): Namespace = Namespace(unwrap(self) :+ NamespaceSegment(name))
      // def /(names: Iterable[String]): Namespace = Namespace(unwrap(self) ++ names.map(NamespaceSegment(_)))
      // def /(names: String*): Namespace = /(names)
      // def /(names: Chunk[String]): Namespace = Namespace(unwrap(self) ++ names.map(NamespaceSegment(_)))
    }
  }

  type LocalName = LocalName.Type
  object LocalName extends Subtype[String] {

    implicit class LocalNameOps(val self: LocalName) extends AnyVal {
      def value: String                                    = unwrap(self)
      def toQualified(namespace: Namespace): QualifiedName = QualifiedName(namespace, self)
    }

  }

  final case class QualifiedName(namespace: Namespace, localName: LocalName) { self =>
    override def toString: String = s"${namespace.show}::${localName.value}"
  }
}
