package org.finos.morphir.datamodel
trait Attributes {
  def isEmpty: Boolean
}

object Attributes {
  sealed trait Binding extends Product with Serializable {
    def keyId: ConceptId
  }
}
