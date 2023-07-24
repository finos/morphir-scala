package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.namespacing.PartialName

case class SingleEnumWrapper(label: String, innerShape: Concept, rootPath: PartialName) {
  def construct(value: Data) =
    Data.Case(
      EnumLabel.Empty -> value
    )(label, this.concept)

  def concept =
    Concept.Enum(
      rootPath % label,
      Concept.Enum.Case(Label(label), EnumLabel.Empty -> innerShape)
    )
}
