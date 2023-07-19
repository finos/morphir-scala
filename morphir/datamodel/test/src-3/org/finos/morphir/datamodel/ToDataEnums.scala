package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Derivers.{*, given}
import org.finos.morphir.datamodel.Concept.Enum
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Data.Case
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.namespacing.*
import org.finos.morphir.datamodel.namespacing.Namespace.root

object EnumGns {
  val gns: Namespace = root / "enumtest"
}

object EnumData1 {
  import EnumGns._
  implicit val gnsImpl: GlobalNamespace = new GlobalNamespace {
    def value = gns
  }

  sealed trait Foo
  case object Bar               extends Foo
  case class Baz(value: String) extends Foo

  val deriver = Deriver.gen[Foo]
}

object EnumData2 {
  import EnumGns._
  implicit val gnsImpl: GlobalNamespace = new GlobalNamespace {
    def value = gns
  }

  enum Foo {
    case Bar
    case Baz(value: String)
  }

  val deriver = Deriver.gen[Foo]
}

object EnumData3 {
  import EnumGns._
  implicit val gnsImpl: TypeNamespace[Foo] = new TypeNamespace[Foo] {
    def value = gns
  }

  implicit val gnsImpl2: TypeNamespace[Baz] = new TypeNamespace[Baz] {
    def value = gns
  }

  sealed trait Foo
  case object Bar               extends Foo
  case class Baz(value: String) extends Foo

  val deriver = Deriver.gen[Foo]
}

class ToDataEnums extends munit.FunSuite {
  import EnumGns._
  // DONT use Deriver.gen in same project while working on it
  // if it fails, it messes up IntelliJ's ability to know what
  // is compiled and what is not
  val concept =
    Enum(
      gns :: ("Foo"),
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

  test("Enum Data 1 - List of Enums") {
    import EnumData1._
    val listOfEnums = List(Bar, Baz("A"), Baz("B"))
    assertEquals(
      Deriver.toData(listOfEnums),
      Data.List(
        Case()("Bar", concept),
        Case(el"value" -> Data.String("A"))("Baz", concept),
        Case(el"value" -> Data.String("B"))("Baz", concept)
      )
    )
  }

  test("Enum Data 1 - Sealed Trait In-Class Field") {
    import EnumData1._
    case class Stuff(a: String, b: Foo, c: Int)
    val stuff = Stuff("a_str", Baz("baz_val"), 123)
    assertEquals(
      Deriver.toData(stuff),
      Data.Record(
        gns :: "Stuff",
        l"a" -> Data.String("a_str"),
        l"b" -> Case(el"value" -> Data.String("baz_val"))("Baz", concept),
        l"c" -> Data.Int(123)
      )
    )
  }

  test("Enum Data 1 - Sealed Trait In-Class Subtype") {
    import EnumData1._
    // In this case Baz is treated as a product type because the deriver knows nothing about it being a "Foo" instance
    case class Stuff(a: String, b: Baz, c: Int)
    val stuff = Stuff("a_str", Baz("baz_val"), 123)
    assertEquals(
      Deriver.toData(stuff),
      Data.Record(
        gns :: "Stuff",
        l"a" -> Data.String("a_str"),
        l"b" -> Data.Record(gns :: "Baz", l"value" -> Data.String("baz_val")),
        l"c" -> Data.Int(123)
      )
    )
  }

  test("Enum Data 1 - Enum In-Class Field") {
    import EnumData2._
    case class Stuff(a: String, b: Foo, c: Int)
    val stuff = Stuff("a_str", Foo.Baz("baz_val"), 123)
    assertEquals(
      Deriver.toData(stuff),
      Data.Record(
        gns :: "Stuff",
        l"a" -> Data.String("a_str"),
        l"b" -> Case(el"value" -> Data.String("baz_val"))("Baz", concept),
        l"c" -> Data.Int(123)
      )
    )
  }

  // Direct-typing on enum-sub-types also not supported yet
  //  test("Enum Data 1 - Enum In-Class Field") {
  //    import EnumData2._
  //    case class Stuff(a: String, b: Foo.Baz, c: Int)
  //    val stuff = Stuff("a_str", Foo.Baz("baz_val"), 123)
  //    assertEquals(
  //      Deriver.toData(stuff),
  //      Data.Record(
  //        l"a" -> Data.String("a_str"),
  //        l"b" -> Case(el"value" -> Data.String("baz_val"))("Baz", concept),
  //        l"c" -> Data.Int(123)
  //      )
  //    )
  //  }

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

  test("Enum Data 2 - Value") {
    import EnumData3._
    assertEquals(
      deriver.derive(Baz("something")),
      Case(el"value" -> Data.String("something"))("Baz", concept)
    )
  }
}
