package org.finos.morphir.datamodel

import org.finos.morphir.naming.Namespace

final case class ConceptId(namespace: Namespace, module: String, name: String) extends HasId {
  override def id: ConceptId = this
}

object ConceptId { self => }

trait HasId {
  def id: ConceptId
}

trait ConceptTag[A] extends HasId {}
