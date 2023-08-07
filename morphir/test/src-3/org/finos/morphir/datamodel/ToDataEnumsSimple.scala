package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.{*, given}
import org.finos.morphir.datamodel.Concept.Enum
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Data.Case
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.namespacing.*
import org.finos.morphir.datamodel.namespacing.PackageName.root
import org.finos.morphir.datamodel.namespacing.Namespace.ns

object EnumData4 {
  import EnumGns._
  implicit val gnsImpl: GlobalDatamodelContext = new GlobalDatamodelContext {
    def value = gns
  }

  enum Foo {
    case Bar
    case Baz
  }

  val deriver = Deriver.gen[Foo]
}

class ToDataEnumsSimple extends munit.FunSuite {
  import EnumGns._

  val concept =
    Enum(
      gns :: ("Foo"),
      Enum.Case(l"Bar"),
      Enum.Case(l"Baz")
    )

  test("Enum Data 4") {
    import EnumData4._
    assertEquals(deriver.derive(Foo.Bar), Case()("Bar", concept))
  }

  test("Enum Data 4.1") {
    import EnumData4._
    interceptMessage[IllegalArgumentException]("The value `null` is not an instance of the needed enum class Foo") {
      deriver.derive(null)
    }
  }
}
