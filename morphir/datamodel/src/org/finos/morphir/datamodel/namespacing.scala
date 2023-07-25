package org.finos.morphir.datamodel

import org.finos.morphir.foundations.capabilities.*
import zio.*
import zio.prelude.*
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

    implicit final class NamespaceOps(val self: Namespace) extends AnyVal {
      def segments: Chunk[NamespaceSegment]  = unwrap(self)
      def show                               = self.segments.map(_.value).mkString(".")
      def parts: IndexedSeq[String]          = unwrap(self)
      def /(segment: String): Namespace      = Namespace(unwrap(self) :+ NamespaceSegment(segment))
      def /(namespace: Namespace): Namespace = Namespace(unwrap(self) ++ unwrap(namespace))
    }
  }
  val ns = Namespace.ns

  type LocalName = LocalName.Type
  object LocalName extends Subtype[String] {
    implicit final class LocalNameOps(val self: LocalName) extends AnyVal {
      def value: String                                                       = unwrap(self)
      def toQualified(pack: PackageName, namespace: Namespace): QualifiedName = QualifiedName(pack, namespace, self)
      def /:(partialName: PartialName): QualifiedName = QualifiedName(partialName.pack, partialName.namespace, self)
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

    implicit final class PackageNameOps(val self: PackageName) extends AnyVal {
      def map[A](f: PackageSegment => A): Chunk[A] = unwrap(self).map(f)
      def segments: Chunk[PackageSegment]          = unwrap(self)
      def show                                     = self.segments.map(_.value).mkString(".")
      def %(namespace: Namespace): PartialName     = PartialName(self, namespace)
      def /(segment: String): PackageName          = PackageName(unwrap(self) :+ PackageSegment(segment))
      def /(namespace: PackageName): PackageName   = PackageName(unwrap(self) ++ unwrap(namespace))
    }
  }
  val root = PackageName.root

  final case class QualifiedName(pack: PackageName, namespace: Namespace, localName: LocalName) { self =>
    override def toString: String = s"${pack}::${namespace.show}::${localName.value}"
  }
  object QualifiedName {
    def apply(partialName: PartialName, localName: LocalName) =
      new QualifiedName(partialName.pack, partialName.namespace, localName)
  }

  case class PartialName(pack: PackageName, namespace: Namespace) {
    def %(localName: String): QualifiedName      = QualifiedName(pack, namespace, LocalName(localName))
    def %%(localName: LocalName): QualifiedName  = QualifiedName(pack, namespace, localName)
    def /(namespaceSegment: String): PartialName = PartialName(pack, namespace / namespaceSegment)
    def /(namespace: Namespace): PartialName     = PartialName(pack, namespace / namespace)
    // def :/(localName: LocalName): QualifiedName = QualifiedName(pack, namespace, localName)
  }
}
