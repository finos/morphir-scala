package org.finos.morphir.universe

import org.finos.morphir.universe.ir.*

trait Attributes {
  def isEmpty: Boolean
}

object Attributes {
  sealed trait Binding extends Product with Serializable {
    def keyId: ConceptId
  }
  object Binding {
    final case class StaticBinding()
  }
}
