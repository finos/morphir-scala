package org.finos.morphir.util

import zio.test._
import zio.test.Assertion._
import PrintMDMJson._
import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.datamodel.Util._
import org.finos.morphir.datamodel._
import org.finos.morphir.ir.Type
import org.finos.morphir.naming._

object PrintMDMJsonSpec extends MorphirBaseSpec {

  def personRecordData(name: String, number: Int) = Data.Record(
    qn"Morphir/Examples/App:RecordTests:RecordType",
    (Label("name"), Data.String(name)),
    (Label("age"), Data.Int32(number))
  )

  def oneArg(i: Int): Data = Data.Case(
    List((EnumLabel.Named("arg1"), Data.Int(i))),
    "OneArg",
    unionEnumShape
  )

  def twoArg(i: Int, s: String): Data = Data.Case(
    List(
      (EnumLabel.Named("arg1"), Data.Int(i)),
      (EnumLabel.Named("arg2"), Data.String(s))
    ),
    "TwoArg",
    unionEnumShape
  )

  def unionEnumShape: Concept.Enum = Concept.Enum(
    qn"Morphir/Examples/App:ConstructorTests:UnionType",
    List(
      Concept.Enum.Case(
        Label("OneArg"),
        List(
          (EnumLabel.Named("arg1"), Concept.Int32)
        )
      ),
      Concept.Enum.Case(
        Label("TwoArg"),
        List(
          (EnumLabel.Named("arg1"), Concept.Int32),
          (EnumLabel.Named("arg2"), Concept.String)
        )
      ),
      Concept.Enum.Case(
        Label("ZeroArg"),
        List()
      )
    )
  )

  def opaqueIntShape: Concept.Enum = Concept.Enum(
    qn"Morphir/Examples/App:ExampleModule:OpaqueInt",
    List(
      Concept.Enum.Case(
        Label("Opaque"),
        List(
          (EnumLabel.Named("arg1"), Concept.Int32)
        )
      )
    )
  )

  def opaqueInt(i: Int): Data = Data.Case(
    List((EnumLabel.Named("arg1"), Data.Int(i))),
    "Opaque",
    opaqueIntShape
  )

  def spec = suite("PrintMDMJsonSpec")(
    test("compactPrintRecords") {
      val persons = Data.List(
        personRecordData("Fido", 42),
        personRecordData("Rex", 43),
        personRecordData("Spot", 44)
      )
      val compactPrinted = PrintMDMJson.compactPrint(persons)
      assert(compactPrinted)(equalTo(
        "[{\"name\":\"String(Fido)\",\"age\":\"Int32(42)\"},{\"name\":\"String(Rex)\",\"age\":\"Int32(43)\"},{\"name\":\"String(Spot)\",\"age\":\"Int32(44)\"}]"
      ))
    },
    test("prettyPrintRecords") {
      val persons = Data.List(
        personRecordData("Fido", 42),
        personRecordData("Rex", 43),
        personRecordData("Spot", 44)
      )
      val prettyPrinted = PrintMDMJson.prettyPrint(persons)
      val expected = """[
                       |  {
                       |    "name" : "String(Fido)",
                       |    "age" : "Int32(42)"
                       |  },
                       |  {
                       |    "name" : "String(Rex)",
                       |    "age" : "Int32(43)"
                       |  },
                       |  {
                       |    "name" : "String(Spot)",
                       |    "age" : "Int32(44)"
                       |  }
                       |]""".stripMargin
      assert(prettyPrinted)(equalTo(expected))
    },
    test("prettyPrintRecord") {
      val personRecord  = personRecordData("Fido", 42)
      val prettyPrinted = PrintMDMJson.prettyPrint(personRecord)
      val expected = """{
                       |  "name" : "String(Fido)",
                       |  "age" : "Int32(42)"
                       |}""".stripMargin

      assert(prettyPrinted)(equalTo(expected))
    },
    test("compactPrintRecord") {
      val personRecord   = personRecordData("Fido", 42)
      val compactPrinted = PrintMDMJson.compactPrint(personRecord)
      assert(compactPrinted)(equalTo("{\"name\":\"String(Fido)\",\"age\":\"Int32(42)\"}"))
    },
    test("prettyPrintOneArg") {
      val oneArgData    = oneArg(42)
      val prettyPrinted = PrintMDMJson.prettyPrint(oneArgData)
      val expected = """{
                       |  "enumLabel" : "OneArg",
                       |  "values" : [
                       |    {
                       |      "Named(arg1)" : "Int32(42)"
                       |    }
                       |  ]
                       |}""".stripMargin

      assert(prettyPrinted)(equalTo(expected))
    },
    test("compactPrintOneArg") {
      val oneArgData     = oneArg(42)
      val compactPrinted = PrintMDMJson.compactPrint(oneArgData)
      assert(compactPrinted)(equalTo("{\"enumLabel\":\"OneArg\",\"values\":[{\"Named(arg1)\":\"Int32(42)\"}]}"))
    },
    test("prettyPrintTwoArg") {
      val twoArgData    = twoArg(42, "Hello")
      val prettyPrinted = PrintMDMJson.prettyPrint(twoArgData)
      val expected = """{
                       |  "enumLabel" : "TwoArg",
                       |  "values" : [
                       |    {
                       |      "Named(arg1)" : "Int32(42)"
                       |    },
                       |    {
                       |      "Named(arg2)" : "String(Hello)"
                       |    }
                       |  ]
                       |}""".stripMargin

      assert(prettyPrinted)(equalTo(expected))
    },
    test("compactPrintTwoArg") {
      val twoArgData     = twoArg(42, "Hello")
      val compactPrinted = PrintMDMJson.compactPrint(twoArgData)
      assert(compactPrinted)(equalTo(
        "{\"enumLabel\":\"TwoArg\",\"values\":[{\"Named(arg1)\":\"Int32(42)\"},{\"Named(arg2)\":\"String(Hello)\"}]}"
      ))
    },
    test("prettyPrintOpaqueInt") {
      val opaqueIntData = opaqueInt(42)
      val prettyPrinted = PrintMDMJson.prettyPrint(opaqueIntData)
      val expected = """{
                       |  "enumLabel" : "Opaque",
                       |  "values" : [
                       |    {
                       |      "Named(arg1)" : "Int32(42)"
                       |    }
                       |  ]
                       |}""".stripMargin

      assert(prettyPrinted)(equalTo(expected))
    },
    test("compactPrintOpaqueInt") {
      val opaqueIntData  = opaqueInt(42)
      val compactPrinted = PrintMDMJson.compactPrint(opaqueIntData)
      assert(compactPrinted)(equalTo("{\"enumLabel\":\"Opaque\",\"values\":[{\"Named(arg1)\":\"Int32(42)\"}]}"))
    },
    test("prettyPrintNestedRecords") {
      val persons = Data.Record(
        qn"Morphir/Examples/App:RecordTests:NestedRecordType",
        (Label("name"), Data.String("Persons")),
        (
          Label("records"),
          Data.List(
            personRecordData("Ponyo", 3),
            personRecordData("Soso", 3)
          )
        )
      )
      val prettyPrinted = PrintMDMJson.prettyPrint(persons)
      val expected = """{
                       |  "name" : "String(Persons)",
                       |  "records" : [
                       |    {
                       |      "name" : "String(Ponyo)",
                       |      "age" : "Int32(3)"
                       |    },
                       |    {
                       |      "name" : "String(Soso)",
                       |      "age" : "Int32(3)"
                       |    }
                       |  ]
                       |}""".stripMargin

      assert(prettyPrinted)(equalTo(expected))
    }
  )
}
