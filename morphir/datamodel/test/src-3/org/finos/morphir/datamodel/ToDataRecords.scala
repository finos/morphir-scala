package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Deriver
import org.finos.morphir.datamodel.Util._

class ToDataRecords extends munit.FunSuite {
  test("basic record") {
    case class Person(name: String, age: Int) ////////
    assertEquals(
      Deriver.toData(Person("Joe", 123)), //
      Data.Record(l"name" -> Data.String("Joe"), l"age" -> Data.Int(123))
    )
  }

  test("nested record") {
    case class Name(first: String, last: String)
    case class Person(name: Name, age: Int)
    assertEquals(
      Deriver.toData(Person(Name("Joe", "Bloggs"), 123)),
      Data.Record(
        l"name" -> Data.Record(l"first" -> Data.String("Joe"), l"last" -> Data.String("Bloggs")),
        l"age"  -> Data.Int(123)
      )
    )
  }

  test("nested record with list") {
    case class Name(first: String, last: String)
    case class Person(name: List[Name], age: Int)
    assertEquals(
      Deriver.toData(Person(List(Name("Joe", "Bloggs"), Name("Jim", "Roogs")), 123)),
      Data.Record(
        l"name" ->
          Data.List(
            Data.Record(l"first" -> Data.String("Joe"), l"last" -> Data.String("Bloggs")),
            Data.Record(l"first" -> Data.String("Jim"), l"last" -> Data.String("Roogs"))
          ),
        l"age" -> Data.Int(123)
      )
    )
  }
}
