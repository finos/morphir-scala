package org.finos.morphir.datamodel

import org.finos.morphir.naming._

case class SingleEnumWrapper(label: String, innerShape: Concept, rootPath: QualifiedModuleName) {
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

case class UnitEnumWrapper(label: String, rootPath: QualifiedModuleName) {
  def construct =
    Data.Case()(label, this.concept)

  def concept =
    Concept.Enum(
      rootPath % label,
      Concept.Enum.Case(Label(label))
    )
}
