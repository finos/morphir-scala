package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.DataEncoder

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap
import org.finos.morphir.datamodel.{*, given}

case class Person(fName: String, lName: Int)
object Joe {
  def unapply(p: Person) =
    if (p.fName == "Joe") Some((p.fName, p.lName))
    else None
}

class ToDataOptional extends munit.FunSuite {
  test("Option") {
    assertEquals(
      DataEncoder.toData(Option(123)),
      Data.Optional.Some(Data.Int(123))
    )
  }
  test("Option Some") {
    assertEquals(
      DataEncoder.toData(Some(123)),
      Data.Optional.Some(Data.Int(123))
    )
  }
  test("Option None") {
    assertEquals(
      DataEncoder.toData(None),
      Data.Optional.None(Concept.Nothing)
    )
  }
}
