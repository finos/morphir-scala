package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.{*, given}
import org.finos.morphir.datamodel.Concept.Enum
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Data.Case
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
import zio.test.Assertion.*

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

object EnumData5 {
  import EnumGns._
  implicit val gnsImpl: GlobalDatamodelContext = new GlobalDatamodelContext {
    def value                    = gns
    override def enumTranslation = EnumTranslation.SingleFieldWithRecord
  }

  sealed trait Foo
  object Foo {
    case object Bar               extends Foo
    case class Baz(value: String) extends Foo
  }

  val deriver = Deriver.gen[Foo]
}

object ToDataEnumsSimpleSpec extends MorphirBaseSpec {
  import EnumGns._

  val concept =
    Enum(
      gns % ("Foo"),
      Enum.Case(l"Bar"),
      Enum.Case(l"Baz")
    )

  val concept2 =
    Enum(
      gns % ("Foo"),
      Enum.Case(l"Bar"),
      Enum.Case(Label("Baz"), EnumLabel.Empty -> Concept.Struct(Label("value") -> Concept.String()))
    )

  def spec = suite("ToDataEnumSimpleSpec")(
    test("Enum Data 4") {
      import EnumData4._
      assertTrue(deriver.derive(Foo.Bar) == Case()("Bar", concept))
    },
    test("Enum Data 4.1") {
      import EnumData4._
      assert(deriver.derive(null))(throws(isSubtype[IllegalArgumentException](
        hasMessage(equalTo("The value `null` is not an instance of the needed enum class Foo"))
      )))

    },
    test("Enum Data 5 - Concept") {
      import EnumData5._
      assertTrue(deriver.concept == concept2)
    },
    test("Enum Data 5 - NoVals") {
      import EnumData5._
      assertTrue(deriver.derive(Foo.Bar) == Case(List(), "Bar", concept2))
    },
    test("Enum Data 5 - Value") {
      import EnumData5._
      assertTrue(
        deriver.derive(Foo.Baz("something")) ==
          Case(EnumLabel.Empty -> Data.Struct(l"value" -> Data.String("something")))("Baz", concept2)
      )
    }
  )
}
