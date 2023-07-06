package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Derivers.{*, given}
import org.finos.morphir.datamodel.Concept.Enum
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Data.Case
import org.finos.morphir.datamodel.Util.*

object EnumData1 {
  sealed trait Foo
  case object Bar               extends Foo
  case class Baz(value: String) extends Foo

  val deriver = Deriver.gen[Foo]
}

object EnumData2 {
  enum Foo {
    case Bar
    case Baz(value: String)
  }

  val deriver = Deriver.gen[Foo]
}

class ToDataEnums extends munit.FunSuite {
  // DONT use Deriver.gen in same project while working on it
  // if it fails, it messes up IntelliJ's ability to know what
  // is compiled and what is not

  val concept =
    Enum(
      "Foo",
      Enum.Case(l"Bar"),
      Enum.Case(l"Baz", (el"value", Concept.String))
    )

  test("Enum Data 1 - Concept") {
    import EnumData1._
    assertEquals(deriver.concept, concept)
  }

  test("Enum Data 1 - NoVals") {
    import EnumData1._
    assertEquals(deriver.derive(Bar), Case(List(), "Bar", concept))
  }

  test("Enum Data 1 - Value") {
    import EnumData1._
    assertEquals(
      deriver.derive(Baz("something")),
      Case(el"value" -> Data.String("something"))("Baz", concept)
    )
  }

  test("Enum Data 1 - Adv") {
    import EnumData1._
    val listOfEnums = List(Bar, Baz("A"), Baz("B"))
    println("hello")
    assertEquals(
      Deriver.toData(listOfEnums),
      Data.List(
        Case()("Bar", concept),
        Case(el"value" -> Data.String("A"))("Baz", concept),
        Case(el"value" -> Data.String("B"))("Baz", concept)
      )
    )
  }

  test("Enum Data 2 - Concept") {
    import EnumData2._
    assertEquals(deriver.concept, concept)
  }

  test("Enum Data 2 - NoVals") {
    import EnumData1._
    assertEquals(deriver.derive(Bar), Case(List(), "Bar", concept))
  }

  test("Enum Data 2 - Value") {
    import EnumData1._
    assertEquals(
      deriver.derive(Baz("something")),
      Case(el"value" -> Data.String("something"))("Baz", concept)
    )
  }
}
