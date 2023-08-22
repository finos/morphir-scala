package org.finos.morphir.universe

import org.finos.morphir.universe.ir.*

final case class ConceptId(namespace: String, name: String) extends HasConceptId {
  override def id: ConceptId = this
}

object ConceptId {
  final case class Member(conceptId: ConceptId, member: String)
}
