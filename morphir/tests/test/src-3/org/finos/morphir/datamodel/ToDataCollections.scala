package org.finos.morphir.datamodel

import org.finos.morphir.testing.MorphirBaseSpec

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap
import zio.test._

object ToDataCollections extends MorphirBaseSpec {
  def spec = suite("ToDataCollections")(
    test("Primitive List") {
      assertTrue(
        Deriver.toData(List(1, 2, 3)) ==
          Data.List(Data.Int(1), Data.Int(2), Data.Int(3))
      )
    },
    test("Primitive List - Empty") {
      assertTrue(
        Deriver.toData(List[Int]()) ==
          Data.List.empty(Concept.Int32())
      )
    },
    test("Primitive Map") {
      assertTrue(
        Deriver.toData(LinkedHashMap("abc" -> 123, "def" -> 456)) ==
          Data.Map(Data.String("abc") -> Data.Int(123), Data.String("def") -> Data.Int(456))
      )
      assertTrue(
        Deriver.toData(ListMap("abc" -> 123, "def" -> 456)) ==
          Data.Map(Data.String("abc") -> Data.Int(123), Data.String("def") -> Data.Int(456))
      )
      assertTrue(
        Deriver.toData(Map("abc" -> 123, "def" -> 456)) ==
          Data.Map(Data.String("abc") -> Data.Int(123), Data.String("def") -> Data.Int(456))
      )
    },
    test("Empty Map") {
      val schema = Data.Map.empty(Concept.String(), Concept.Int32())
      assertTrue(
        Deriver.toData(LinkedHashMap[String, Int]()) == schema,
        Deriver.toData(ListMap[String, Int]()) == schema,
        Deriver.toData(Map[String, Int]()) == schema
      )
    }
  )
}
