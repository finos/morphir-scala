package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.namespacing.Namespace
import org.finos.morphir.datamodel.namespacing.{
  LocalName,
  Namespace,
  NamespaceSegment,
  QualifiedName,
  PackageName,
  PartialName,
  PackageSegment
}
import zio.Chunk

import scala.annotation.targetName

// Cannot put these into NamespaceOps in namespacing because Scala 3 will not correctly detect the operators
// e.g. report bogus errors such as:
// value :: is not a member of String, but could be made available as an extension method.
extension (self: Namespace) {
  @targetName("namespaceSlashString")
  def /(name: String): Namespace = Namespace(self.segments :+ NamespaceSegment(name))
  @targetName("namespaceSlashIterable")
  def /(names: Iterable[String]): Namespace = Namespace(self.segments ++ names.map(NamespaceSegment(_)))
  @targetName("namespaceSlashStringVar")
  def /(names: String*): Namespace = /(names)
  @targetName("namespaceSlashStringChunk")
  def /(names: Chunk[String]): Namespace = Namespace(self.segments ++ names.map(NamespaceSegment(_)))
}

extension (self: PackageName) {
  @targetName("packageSlashString")
  def /(name: String): PackageName = PackageName(self.segments :+ PackageSegment(name))
  @targetName("packageSlashIterable")
  def /(names: Iterable[String]): PackageName = PackageName(self.segments ++ names.map(PackageSegment(_)))
  @targetName("packageSlashStringVar")
  def /(names: String*): PackageName = /(names)
  @targetName("packageSlashChunk")
  def /(names: Chunk[String]): PackageName = PackageName(self.segments ++ names.map(PackageSegment(_)))

  @targetName("packageToPartialName")
  def ::(ns: Namespace) = PartialName(self, ns)
}

extension (self: PartialName) {
  def ::(localName: LocalName) = QualifiedName(self.pack, self.namespace, localName)
  @targetName("partialNameToQualified")
  def ::(localName: String) = QualifiedName(self.pack, self.namespace, LocalName(localName))
}
