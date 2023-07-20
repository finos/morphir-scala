package org.finos.morphir.datamodel
import org.finos.morphir.foundations.*
import org.finos.morphir.foundations.capabilities.*

object namespacing {

  def localName(name: String): LocalName = LocalName(name)

  type NamespaceSegment = NamespaceSegment.Type
  object NamespaceSegment extends Subtype[String] {
    implicit class NamespaceSegmentOps(val self: NamespaceSegment) extends AnyVal {
      def value: String = unwrap(self)
    }
  }

  type Namespace = Namespace.Type
  object Namespace extends Newtype[Chunk[NamespaceSegment]] {
    lazy val ns: Namespace = Namespace(Chunk.empty)

    implicit val showInstance: Show[Namespace] = ns => ns.segments.map(_.value).mkString(".")

    def fromStrings(inputs: String*): Namespace = Namespace.fromIterable(segments(inputs))

    def fromIterable(segments: Iterable[NamespaceSegment]): Namespace =
      Namespace(Chunk.fromIterable(segments))

    def segments(inputs: Iterable[String]): Iterable[NamespaceSegment] = inputs.map(NamespaceSegment(_))

    implicit class NamespaceOps(val self: Namespace) extends AnyVal {
      def segments: Chunk[NamespaceSegment]       = unwrap(self)
      def /(segment: NamespaceSegment): Namespace = Namespace(unwrap(self) :+ segment)
      def /(namespace: Namespace): Namespace      = Namespace(unwrap(self) ++ unwrap(namespace))
    }
  }

  type LocalName = LocalName.Type
  object LocalName extends Subtype[String] {

    implicit class LocalNameOps(val self: LocalName) extends AnyVal {
      def value: String                                                       = unwrap(self)
      def toQualified(pack: PackageName, namespace: Namespace): QualifiedName = QualifiedName(pack, namespace, self)
    }

  }

  type PackageSegment = PackageSegment.Type
  object PackageSegment extends Subtype[String] {
    implicit class PackageSegmentOps(val self: PackageSegment) extends AnyVal {
      def value: String = unwrap(self)
    }
  }

  type PackageName = PackageName.Type
  object PackageName extends Newtype[Chunk[PackageSegment]] {
    lazy val root: PackageName = PackageName(Chunk.empty)

    implicit val showInstance: Show[PackageName] = ns => ns.segments.map(_.value).mkString(".")

    def fromStrings(inputs: String*): PackageName = PackageName.fromIterable(segments(inputs))

    def fromIterable(segments: Iterable[PackageSegment]): PackageName =
      PackageName(Chunk.fromIterable(segments))

    def segments(inputs: Iterable[String]): Iterable[PackageSegment] = inputs.map(PackageSegment(_))

    implicit class PackageNameOps(val self: PackageName) extends AnyVal {
      def segments: Chunk[PackageSegment]         = unwrap(self)
      def /(segment: PackageSegment): PackageName = PackageName(unwrap(self) :+ segment)
      def /(namespace: PackageName): PackageName  = PackageName(unwrap(self) ++ unwrap(namespace))
    }
  }

  final case class QualifiedName(pack: PackageName, namespace: Namespace, localName: LocalName) { self =>
    override def toString: String = s"${pack}::${namespace.show}::${localName.value}"
  }
  object QualifiedName {
    def apply(partialName: PartialName, localName: LocalName) =
      new QualifiedName(partialName.pack, partialName.namespace, localName)
  }

  case class PartialName(pack: PackageName, namespace: Namespace)
}
