package org.finos.morphir.mir.module

import org.finos.morphir.mir.Type.{Specification => TypeSpec}
import org.finos.morphir.mir.Value.{Specification => ValueSpec}
import org.finos.morphir.mir.{Documented, Name}

final case class Specification[+TA](
    types: Map[Name, Documented[TypeSpec[TA]]],
    values: Map[Name, Documented[ValueSpec[TA]]]
) { self =>
  def lookupValueSpecification(localName: Name): Option[ValueSpec[TA]] =
    values.get(localName).map(_.value)

  def lookupTypeSpecification(localName: Name): Option[TypeSpec[TA]] =
    types.get(localName).map(doc => doc.value)

  def eraseAttributes: Specification[Any] = self.mapAttributes(_ => ())

  def mapAttributes[TB](tf: TA => TB): Specification[TB] = Specification(
    types.map { case (name, doc) => (name, doc.map(_.map(tf))) },
    values.map { case (name, doc) => (name, doc.map(_.map(tf))) }
  )
}

object Specification {
  val empty: Specification[Nothing] = Specification(Map.empty, Map.empty)

  type Raw = Specification[Any]
  object Raw {
    def apply(
        types: Map[Name, Documented[TypeSpec[Any]]],
        values: Map[Name, Documented[ValueSpec[Any]]]
    ): Raw = Specification(types, values)
  }
}
