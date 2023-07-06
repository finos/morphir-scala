package org.finos.morphir.runtime

import org.finos.morphir.ir.Name
import org.finos.morphir.testing.munit.MorphirTestSuite

import scala.collection.immutable.ListMap
import com.eed3si9n.expecty.Expecty.expect

class MorphirRecordSuite extends MorphirTestSuite {
  describe("MorphirRecord Spec") {
    describe("canEqual") {
      test("Should be true with any other MorphirRecord") {
        val record1 =
          MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26), Some("Person"))
        val record2 = MorphirRecord(ListMap(Name.fromString("age") -> 28))

        assertEquals(record1.canEqual(record1), true)
        assertEquals(record1.canEqual(record2), true)
        assertEquals(record2.canEqual(record1), true)
      }
      test("Should be false with any other type") {
        val record = MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26))

        assertEquals(record.canEqual("Some string"), false)
        assertEquals(record.canEqual(6), false)
        assertEquals(record.canEqual(true), false)

      }
    }

    describe("equals") {
      test("Should be true with the same or with similar record") {
        val record1 =
          MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26), Some("Person"))
        val record2 =
          MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26), Some("Person"))

        assertEquals(record1.equals(record1), true)
        assertEquals(record1.equals(record2), true)
        assertEquals(record2.equals(record1), true)

      }
      test("Should be false with a different record") {
        val record1 =
          MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26), Some("Person"))
        val record2 = MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26))
        val record3 = MorphirRecord(ListMap(Name.fromString("age") -> 26))

        assertEquals(record1.equals(record2), false)
        assertEquals(record2.equals(record1), false)
        assertEquals(record1.equals(record3), false)
        assertEquals(record3.equals(record1), false)
        assertEquals(record2.equals(record3), false)
        assertEquals(record3.equals(record2), false)

      }
      test("Should be true with a similar record in different field order") {
        val record1 = MorphirRecord(
          ListMap(
            Name.fromString("name")    -> "John",
            Name.fromString("age")     -> 26,
            Name.fromString("married") -> false,
            Name.fromString("temp")    -> 98.7
          ),
          Some("DATA")
        )
        val record2 = MorphirRecord(
          ListMap(
            Name.fromString("age")     -> 26,
            Name.fromString("temp")    -> 98.7,
            Name.fromString("married") -> false,
            Name.fromString("name")    -> "John"
          ),
          Some("DATA")
        )

        assertEquals(record1.equals(record2), true)
        assertEquals(record2.equals(record1), true)

      }
      test("Should be false with other types") {
        val record = MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26))

        expect(
          record.equals("Some string") == false,
          record.equals(6) == false,
          record.equals(true) == false
        )
      }
    }

    describe("hashCode") {
      test("Should be equal with the same or similar record") {
        val record1 =
          MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26), Some("Person"))
        val record2 =
          MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26), Some("Person"))

        assertEquals(record1.hashCode, record2.hashCode)
      }

      test("Should be different with different records") {
        val record1 =
          MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26), Some("Person"))
        val record2 = MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26))
        val record3 = MorphirRecord(ListMap(Name.fromString("age") -> 26))

        assertNotEquals(record1.hashCode, record2.hashCode)
        assertNotEquals(record1.hashCode, record3.hashCode)
        assertNotEquals(record2.hashCode, record3.hashCode)
      }

      test("Should be the same as a similar record in different order") {
        val record1 = MorphirRecord(
          ListMap(
            Name.fromString("name")    -> "John",
            Name.fromString("age")     -> 26,
            Name.fromString("married") -> false,
            Name.fromString("temp")    -> 98.7
          ),
          Some("DATA")
        )
        val record2 = MorphirRecord(
          ListMap(
            Name.fromString("age")     -> 26,
            Name.fromString("temp")    -> 98.7,
            Name.fromString("married") -> false,
            Name.fromString("name")    -> "John"
          ),
          Some("DATA")
        )

        assertEquals(record1.hashCode, record2.hashCode)
      }

      test("Should be different with other types") {
        val record = MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26))

        expect(
          record.hashCode != "John".hashCode,
          record.hashCode != 26.hashCode,
          record.hashCode != true.hashCode
        )

      }
    }

    test("productArity should return as expected") {
      val record = MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26))

      assertEquals(record.productArity, 2)
    }

    test("getField should return as expected") {
      val record = MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26))

      expect(
        record.getField(Name.fromString("name")) == Some("John"),
        record.getField(Name.fromString("age")) == Some(26),
        record.getField(Name.fromString("not existing")) == None
      )
    }

    describe("setField") {
      test("Should return a modified record when key is one of the record's fields") {
        val name      = Name.fromString("name")
        val age       = Name.fromString("age")
        val record    = MorphirRecord(ListMap(name -> "John", age -> 26), Some("Person"))
        val expected1 = MorphirRecord(ListMap(name -> "John", age -> 20), Some("Person"))
        val expected2 = MorphirRecord(ListMap(name -> "Mary", age -> 20), Some("Person"))

        expect(
          record.setField(age, 20) == expected1,
          expected1.setField(name, "Mary") == expected2,
          record.setField(age, 20).setField(name, "Mary") == expected2
        )
      }
      test("Should return the same record when key is not one of the record's fields") {
        val record =
          MorphirRecord(ListMap(Name.fromString("name") -> "John", Name.fromString("age") -> 26), Some("Person"))

        assertEquals(record.setField(Name.fromString("other"), 20), record)
      }

      test("Should keep the same order of the fields in the modified record after being modified") {
        val name      = Name.fromString("name")
        val age       = Name.fromString("age")
        val height    = Name.fromString("height")
        val record    = MorphirRecord(ListMap(name -> "John", age -> 26, height -> 1.72), Some("Person"))
        val modified1 = record.setField(age, 20)
        val modified2 = modified1.setField(name, "Mary")

        expect(
          record != modified1,
          record != modified2,
          modified1 != modified2,
          record.productElementName(0) == modified1.productElementName(0),
          record.productElementName(1) == modified1.productElementName(1),
          record.productElementName(2) == modified1.productElementName(2),
          record.productElementName(0) == modified2.productElementName(0),
          record.productElementName(1) == modified2.productElementName(1),
          record.productElementName(2) == modified2.productElementName(2),
          record.productElement(0) == "John",
          record.productElement(1) == 26,
          record.productElement(2) == 1.72,
          modified1.productElement(0) == "John",
          modified1.productElement(1) == 20,
          modified1.productElement(2) == 1.72,
          modified2.productElement(0) == "Mary",
          modified2.productElement(1) == 20,
          modified2.productElement(2) == 1.72
        )
      }
    }

    describe("setFields") {
      test("Should return a modified record when keys are in the record's map") {
        val name     = Name.fromString("name")
        val age      = Name.fromString("age")
        val height   = Name.fromString("height")
        val record   = MorphirRecord(ListMap(name -> "John", age -> 26, height -> 1.72), Some("Person"))
        val expected = MorphirRecord(ListMap(name -> "Mary", age -> 26, height -> 1.63), Some("Person"))

        expect(
          record.setFields(Map(name -> "Mary", height -> 1.63)) == expected
        )
      }
      test("Should return a modified record when keys are in the record's map and ignore the other keys") {
        val name     = Name.fromString("name")
        val age      = Name.fromString("age")
        val height   = Name.fromString("height")
        val weight   = Name.fromString("weight")
        val record   = MorphirRecord(ListMap(name -> "John", age -> 26, height -> 1.72), Some("Person"))
        val expected = MorphirRecord(ListMap(name -> "Mary", age -> 26, height -> 1.63), Some("Person"))

        expect(
          record.setFields(Map(name -> "Mary", height -> 1.63, weight -> 120)) == expected
        )
      }
      test("Should return the same record when keys are not in the record's map") {
        val name   = Name.fromString("name")
        val age    = Name.fromString("age")
        val height = Name.fromString("height")
        val weight = Name.fromString("weight")
        val record = MorphirRecord(ListMap(name -> "John", age -> 26), Some("Person"))

        expect(
          record.setFields(Map(height -> 1.63, weight -> 120)) == record
        )
      }
      test("Should keep the same order of the fields in the new record after being modified") {
        val name   = Name.fromString("name")
        val age    = Name.fromString("age")
        val height = Name.fromString("height")
        val weight = Name.fromString("weight")
        val record = MorphirRecord(ListMap(name -> "John", age -> 26, height -> 1.72, weight -> 180.4), Some("Person"))
        val modified = record.setFields(Map(height -> 1.63, name -> "Mary"))

        expect(
          record != modified,
          record.productElementName(0) == modified.productElementName(0),
          record.productElementName(1) == modified.productElementName(1),
          record.productElementName(2) == modified.productElementName(2),
          record.productElementName(3) == modified.productElementName(3),
          record.productElement(0) == "John",
          record.productElement(1) == 26,
          record.productElement(2) == 1.72,
          record.productElement(3) == 180.4,
          modified.productElement(0) == "Mary",
          modified.productElement(1) == 26,
          modified.productElement(2) == 1.63,
          record.productElement(3) == 180.4
        )
      }
    }
    test("productElement should return as expected") {
      val name   = Name.fromString("name")
      val age    = Name.fromString("age")
      val height = Name.fromString("height")
      val weight = Name.fromString("weight")
      val record = MorphirRecord(ListMap(name -> "John", age -> 26, height -> 1.72, weight -> 180), Some("Person"))

      expect(
        record.productElement(0) == "John",
        record.productElement(1) == 26,
        record.productElement(2) == 1.72,
        record.productElement(3) == 180
      )
    }
    test("productElementName should return as expected") {
      val name   = Name.fromString("name")
      val age    = Name.fromString("age")
      val height = Name.fromString("height")
      val weight = Name.fromString("weight")
      val record = MorphirRecord(ListMap(name -> "John", age -> 26, height -> 1.72, weight -> 180), Some("Person"))

      expect(
        record.productElementName(0) == "name",
        record.productElementName(1) == "age",
        record.productElementName(2) == "height",
        record.productElementName(3) == "weight"
      )
    }
    describe("copy") {
      test("Should return the modified record with the same fields' order when all keys are in the record's map ") {
        val name     = Name.fromString("name")
        val age      = Name.fromString("age")
        val height   = Name.fromString("height")
        val weight   = Name.fromString("weight")
        val record   = MorphirRecord(ListMap(name -> "John", age -> 26, height -> 1.72, weight -> 180), Some("Person"))
        val modified = record.copy(age = 33, name = "Diana", weight = 140)
        val expected = MorphirRecord(ListMap(name -> "Diana", age -> 33, height -> 1.72, weight -> 140), Some("Person"))

        assertEquals(modified, expected)
        assertEquals(modified.productElementName(0), expected.productElementName(0))
        assertEquals(modified.productElementName(1), expected.productElementName(1))
        assertEquals(modified.productElementName(2), expected.productElementName(2))
        assertEquals(modified.productElementName(3), expected.productElementName(3))

      }
      test("Should should return the record's copy when there are no parameters keeping the field order") {
        val name     = Name.fromString("name")
        val age      = Name.fromString("age")
        val height   = Name.fromString("height")
        val record   = MorphirRecord(ListMap(name -> "John", age -> 26, height -> 1.72), Some("Person"))
        val modified = record.copy()

        assertEquals(modified, record)
        assertEquals(modified.productElementName(0), record.productElementName(0))
        assertEquals(modified.productElementName(1), record.productElementName(1))
        assertEquals(modified.productElementName(2), record.productElementName(2))

      }
    }
    test("Should allow calling by field name") {
      val movie  = Name.fromString("movie")
      val number = Name.fromString("number")
      val team   = Name.fromString("team")
      val color  = Name.fromString("color")
      val record = new MorphirRecord(ListMap(movie -> "Avatar", number -> 6, team -> "Knicks", color -> "Blue"))

      assertEquals(record.movie, "Avatar")
      assertEquals(record.number, 6)
      assertEquals(record.team, "Knicks")
      assertEquals(record.color, "Blue")
    }
    test("Should work with nested records") {
      val movie     = Name.fromString("movie")
      val number    = Name.fromString("number")
      val team      = Name.fromString("team")
      val color     = Name.fromString("color")
      val name      = Name.fromString("name")
      val favorites = Name.fromString("favorites")
      val favoritesRecord =
        new MorphirRecord(
          ListMap(movie -> "Avatar", number -> 3, team -> "Knicks", color -> "Purple"),
          Some("Favorites")
        )
      val record = MorphirRecord(ListMap(name -> "Sophia", favorites -> favoritesRecord))

      assertEquals(record.name, "Sophia")
      assertEquals(record.favorites, favoritesRecord)
      assertEquals(record.favorites.asInstanceOf[MorphirRecord].movie, "Avatar")
      assertEquals(record.favorites.asInstanceOf[MorphirRecord].number, 3)
      assertEquals(record.favorites.asInstanceOf[MorphirRecord].team, "Knicks")
      assertEquals(record.favorites.asInstanceOf[MorphirRecord].color, "Purple")
    }
  }
}
