package org.finos.morphir.datamodel

import org.finos.morphir.naming._
import org.finos.morphir.datamodel.Deriver
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.namespacing.LocalName
import org.finos.morphir.datamodel.{*, given}
import org.finos.morphir.datamodel.namespacing.*

class ToDataRecords extends munit.FunSuite {
  val gns: QualifiedModuleName = root / "morphir" % "datamodel"
  given GlobalDatamodelContext with {
    def value = gns
  }

  test("basic record") {
    case class Person(name: String, age: Int)
    assertEquals(
      Deriver.toData(Person("Joe", 123)),
      Data.Record(gns % "Person", l"name" -> Data.String("Joe"), l"age" -> Data.Int(123))
    )
  }

  test("basic record - override namespace") {
    case class Person(name: String, age: Int)
    val tns: QualifiedModuleName = root / "override" % "datamodel"
    given TypeDatamodelContext[Person] with {
      def value = tns
    }

    assertEquals(
      Deriver.toData(Person("Joe", 123)),
      Data.Record(tns % "Person", l"name" -> Data.String("Joe"), l"age" -> Data.Int(123))
    )
  }

  test("basic record - override namespace, override name") {
    case class Person(name: String, age: Int)
    val tns: QualifiedModuleName = root / "override" % "datamodel"
    given TypeDatamodelContext[Person] with {
      def value                                    = tns
      override def nameOverride: Option[LocalName] = Some(LocalName("Person2"))
    }

    assertEquals(
      Deriver.toData(Person("Joe", 123)),
      Data.Record(tns % "Person2", l"name" -> Data.String("Joe"), l"age" -> Data.Int(123))
    )
  }

  test("nested record") {
    case class Name(first: String, last: String)
    case class Person(name: Name, age: Int)
    assertEquals(
      Deriver.toData(Person(Name("Joe", "Bloggs"), 123)),
      Data.Record(
        gns      % "Person",
        l"name" -> Data.Record(gns % "Name", l"first" -> Data.String("Joe"), l"last" -> Data.String("Bloggs")),
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
        gns % "Person",
        l"name" ->
          Data.List(
            Data.Record(gns % "Name", l"first" -> Data.String("Joe"), l"last" -> Data.String("Bloggs")),
            Data.Record(gns % "Name", l"first" -> Data.String("Jim"), l"last" -> Data.String("Roogs"))
          ),
        l"age" -> Data.Int(123)
      )
    )
  }
}
