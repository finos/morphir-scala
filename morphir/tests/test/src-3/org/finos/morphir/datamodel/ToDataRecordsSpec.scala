package org.finos.morphir.datamodel

import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.naming._
import org.finos.morphir.datamodel.Util._
import org.finos.morphir.datamodel.{given, _}
import zio.test._
object ToDataRecordsSpec extends MorphirBaseSpec {

  val gns: QualifiedModuleName = root / "morphir" % "datamodel"

  given GlobalDatamodelContext with {
    def value = gns
  }

  def spec = suite("ToDataRecordsSpec")(
    test("basic record") {
      case class Person(name: String, age: Int)
      assertTrue(
        Deriver.toData(Person("Joe", 123)) == Data.Record(
          gns      % "Person",
          l"name" -> Data.String("Joe"),
          l"age"  -> Data.Int(123)
        )
      )
    },
    test("basic record - override namespace") {
      case class Person(name: String, age: Int)
      val tns: QualifiedModuleName = root / "override" % "datamodel"
      given TypeDatamodelContext[Person] with {
        def value = tns
      }

      assertTrue(
        Deriver.toData(Person("Joe", 123)) == Data.Record(
          tns      % "Person",
          l"name" -> Data.String("Joe"),
          l"age"  -> Data.Int(123)
        )
      )
    },
    test("basic record - override namespace, override name") {
      case class Person(name: String, age: Int)
      val tns: QualifiedModuleName = root / "override" % "datamodel"
      given TypeDatamodelContext[Person] with {
        def value = tns

        override def nameOverride: Option[Name] = Some(Name("Person2"))
      }

      assertTrue(
        Deriver.toData(Person("Joe", 123)) ==
          Data.Record(tns % "Person2", l"name" -> Data.String("Joe"), l"age" -> Data.Int(123))
      )
    },
    test("nested record") {
      case class Name(first: String, last: String)
      case class Person(name: Name, age: Int)
      assertTrue(
        Deriver.toData(Person(Name("Joe", "Bloggs"), 123)) ==
          Data.Record(
            gns      % "Person",
            l"name" -> Data.Record(gns % "Name", l"first" -> Data.String("Joe"), l"last" -> Data.String("Bloggs")),
            l"age"  -> Data.Int(123)
          )
      )
    },
    test("nested record with list") {
      case class Name(first: String, last: String)
      case class Person(name: List[Name], age: Int)
      assertTrue(
        Deriver.toData(Person(List(Name("Joe", "Bloggs"), Name("Jim", "Roogs")), 123)) ==
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
  )
}
