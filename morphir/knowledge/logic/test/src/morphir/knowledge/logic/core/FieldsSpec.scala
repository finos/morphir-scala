package morphir.knowledge.logic.core

import kyo.test.*

class FieldsSpec extends Test[Any]:
  "valueOf" - {
    "should return the value when the substitution directly contains the value" in {
      val nameField = Field.define[String]("name")
      val bindings  = Fields.init(nameField -> "John Doe")
      val actual    = bindings.valueOf(nameField)
      assert(actual == Some("John Doe"))
    }

    "should return the value when the substitution in-directly contains the value" in {
      val nameField  = Field.define[String]("name")
      val aliasField = Field.define[String]("alias")
      val bindings   = Fields.init(nameField -> "John Doe", aliasField -> nameField)
      val actual     = bindings.valueOf(aliasField)
      assert(actual == Some("John Doe"))
      assert(bindings.fields == Set[Field[_]](nameField, aliasField))
    }

    "Should return None if there is no path to the field value" in {
      val a        = Field.define[Int]("a")
      val b        = Field.define[Int]("b")
      val c        = Field.define[Int]("c")
      val bindings = Fields.init(a -> 42, c -> b)
      val actual   = bindings.valueOf(b)
      assert(actual == None)
      assert(bindings.fields == Set[Field[_]](a, c))
    }

    "should return the value only if it matches the field type" in {
      val indirect    = Field.define[Int]("indirectTuple")
      val indirect2   = Field.define[Boolean]("indirect")
      val valueHolder = Field.define[Int]("valueHolder")
      val bindings    = Fields.init(indirect -> valueHolder, indirect2 -> valueHolder, valueHolder -> 42)
      assert(bindings.valueOf(indirect) == Some(42))
      assert(bindings.valueOf(indirect2) == None)
      assert(bindings.valueOf(valueHolder) == Some(42))
    }
  }

  "dynamicValueOf" - {
    "Will return the field value irrespective of type" in {
      val indirect    = Field.define[Int]("indirectTuple")
      val indirect2   = Field.define[Boolean]("indirect")
      val valueHolder = Field.define[Int]("valueHolder")
      val bindings    = Fields.init(indirect -> valueHolder, indirect2 -> valueHolder, valueHolder -> 42)
      assert(bindings.dynamicValueOf(indirect) == 42)
      assert(Option(bindings.dynamicValueOf(indirect)) == bindings.valueOf(indirect))
      assert(bindings.dynamicValueOf(indirect2) == 42)
      assert(Option(bindings.dynamicValueOf(indirect2)) != bindings.valueOf(indirect2))
      assert(bindings.dynamicValueOf(valueHolder) == 42)
      assert(Option(bindings.dynamicValueOf(valueHolder)) == bindings.valueOf(valueHolder))
    }
  }

  "Adding fields" - {
    "should add a field when fields is empty" in {
      val nameField = Field.define[String]("name")
      val sut       = Fields.empty
      val actual    = sut + (nameField -> "John Doe")
      assert(actual == Fields.init(nameField -> "John Doe"))
    }
  }
end FieldsSpec
