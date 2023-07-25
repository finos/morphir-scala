package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.DataEncoder

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap
import org.finos.morphir.datamodel.{*, given}

class ToDataCollections extends munit.FunSuite {
  test("Primitive List") {
    assertEquals(
      DataEncoder.toData(List(1, 2, 3)),
      Data.List(Data.Int(1), Data.Int(2), Data.Int(3))
    )
  }
  test("Primitive List - Empty") {
    assertEquals(
      DataEncoder.toData(List[Int]()),
      Data.List.empty(Concept.Int32)
    )
  }

//  def convert(data: Data): RawValue =
//    data match {
//      case Data.Map(values, shape) =>
//      case Data.Record(values) =>
//    }

  test("Primitive Map") {
    assertEquals(
      DataEncoder.toData(LinkedHashMap("abc" -> 123, "def" -> 456)),
      Data.Map(Data.String("abc") -> Data.Int(123), Data.String("def") -> Data.Int(456))
    )
    assertEquals(
      DataEncoder.toData(ListMap("abc" -> 123, "def" -> 456)),
      Data.Map(Data.String("abc") -> Data.Int(123), Data.String("def") -> Data.Int(456))
    )
    assertEquals(
      DataEncoder.toData(Map("abc" -> 123, "def" -> 456)),
      Data.Map(Data.String("abc") -> Data.Int(123), Data.String("def") -> Data.Int(456))
    )
  }

  test("Empty Map") {
    val schema = Data.Map.empty(Concept.String, Concept.Int32)
    assertEquals(DataEncoder.toData(LinkedHashMap[String, Int]()), schema)
    assertEquals(DataEncoder.toData(ListMap[String, Int]()), schema)
    assertEquals(DataEncoder.toData(Map[String, Int]()), schema)
  }
}
