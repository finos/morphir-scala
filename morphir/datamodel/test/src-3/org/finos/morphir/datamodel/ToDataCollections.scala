package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Deriver

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap

class ToDataCollections extends munit.FunSuite {
  test("Primitive List") {
    assertEquals(
      Deriver.toData(List(1, 2, 3)),
      Data.List(Data.Int(1), Data.Int(2), Data.Int(3))
    )
  }
  test("Primitive List - Empty") {
    assertEquals(
      Deriver.toData(List[Int]()),
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
      Deriver.toData(LinkedHashMap("abc" -> 123, "def" -> 456)),
      Data.Map(Data.String("abc") -> Data.Int(123), Data.String("def") -> Data.Int(456))
    )
    assertEquals(
      Deriver.toData(ListMap("abc" -> 123, "def" -> 456)),
      Data.Map(Data.String("abc") -> Data.Int(123), Data.String("def") -> Data.Int(456))
    )
    assertEquals(
      Deriver.toData(Map("abc" -> 123, "def" -> 456)),
      Data.Map(Data.String("abc") -> Data.Int(123), Data.String("def") -> Data.Int(456))
    )
  }

  test("Empty Map") {
    val schema = Data.Map.empty(Concept.String, Concept.Int32)
    assertEquals(Deriver.toData(LinkedHashMap[String, Int]()), schema)
    assertEquals(Deriver.toData(ListMap[String, Int]()), schema)
    assertEquals(Deriver.toData(Map[String, Int]()), schema)
  }
}
