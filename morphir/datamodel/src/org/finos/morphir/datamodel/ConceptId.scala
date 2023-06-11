package org.finos.morphir.datamodel
final case class ConceptId(namespace: String, module: String, name: String) extends HasId {
  override def id: ConceptId = this
}

object ConceptId { self => }

trait HasId {
  def id: ConceptId
}

trait ConceptTag[A] extends HasId {}
