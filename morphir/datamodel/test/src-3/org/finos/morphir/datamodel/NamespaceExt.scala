package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.namespacing.Namespace.unwrap
import org.finos.morphir.datamodel.namespacing.{LocalName, Namespace, NamespaceSegment, QualifiedName}
import org.finos.morphir.foundations.Chunk

import scala.annotation.targetName

// Cannot put these into NamespaceOps in namespacing because Scala 3 will not correctly detect the operators
// e.g. report bogus errors such as:
// value :: is not a member of String, but could be made available as an extension method.
extension (self: Namespace) {
  @targetName("slashString")
  def /(name: String): Namespace            = Namespace(unwrap(self) :+ NamespaceSegment(name))
  def /(names: Iterable[String]): Namespace = Namespace(unwrap(self) ++ names.map(NamespaceSegment(_)))
  def /(names: String*): Namespace          = /(names)
  def /(names: Chunk[String]): Namespace    = Namespace(unwrap(self) ++ names.map(NamespaceSegment(_)))

  def ::(localName: LocalName) = QualifiedName(self, localName)
  @targetName("slashLocalNameString")
  def ::(localName: String) = QualifiedName(self, LocalName(localName))
}
