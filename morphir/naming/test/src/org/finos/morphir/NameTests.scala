package org.finos.morphir

import kyo.test.*
import org.finos.morphir.naming.*

class NameTests extends Test[Any]:

  "Create a Name from a string and check that:" - {
    "Name should be creatable from a single word that:" - {
      "Starts with a capital letter" in
        assert(Name.fromString("Marco") == Name("marco"))
      "Is all lowercase" in
        assert(Name.fromString("polo") == Name("polo"))
    }
    "Name should be creatable from compound words that:" - {
      "Are formed from a snake case word" in
        assert(Name.fromString("super_mario_world") == Name("super", "mario", "world"))
      "Contain many kinds of word delimiters" in
        assert(Name.fromString("fooBar_baz 123") == Name("foo", "bar", "baz", "123"))
      "Are formed from a camel-cased string" in
        assert(Name.fromString("valueInUSD") == Name("value", "in", "u", "s", "d"))
      "Are formed from a title-cased string" in {
        assert(Name.fromString("ValueInUSD") == Name("value", "in", "u", "s", "d"))
        assert(Name.fromString("ValueInJPY") == Name("value", "in", "j", "p", "y"))
      }
      "Have a number in the middle" in
        assert(Name.fromString("Nintendo64VideoGameSystem") == Name("nintendo", "64", "video", "game", "system"))
      "Are complete and utter nonsense" in
        assert(Name.fromString("_-%") == Name.empty)
    }
    "It splits the name as expected" in
      // "fooBar","blahBlah" => ["foo","bar","blah","blah"]
      // "fooBar","blahBlah" => ["fooBar","blahBlah"]
      assert(Name.fromString("fooBar").toList == List("foo", "bar"))
  }

  "Name should be convertible to a title-case string:" - {
    "When the name was originally constructed from a snake-case string" in {
      val sut = Name.fromString("snake_case_input")
      assert(Name.toTitleCase(sut) == "SnakeCaseInput")
    }
    "When the name was originally constructed from a camel-case string" in {
      val sut = Name.fromString("camelCaseInput")
      assert(Name.toTitleCase(sut) == "CamelCaseInput")
    }
  }

  "Name should be convertible to a camel-case string:" - {
    "When the name was originally constructed from a snake-case string" in {
      val sut = Name.fromString("snake_case_input")
      assert(Name.toCamelCase(sut) == "snakeCaseInput")
    }
    "When the name was originally constructed from a camel-case string" in {
      val sut = Name.fromString("camelCaseInput")
      assert(Name.toCamelCase(sut) == "camelCaseInput")
    }
  }

  "Name should be convertible to snake-case" - {
    "When given a name constructed from a list of words" in {
      val input = Name.fromList(List("foo", "bar", "baz", "123"))
      assert(Name.toSnakeCase(input) == "foo_bar_baz_123")
    }
    "When the name has parts of an abbreviation" in {
      val name = Name.fromList(List("value", "in", "u", "s", "d"))
      assert(Name.toSnakeCase(name) == "value_in_USD")
    }
  }

  "Name should be convertible to kebab-case" - {
    "When given a name constructed from a list of words" in {
      val input = Name.fromList(List("foo", "bar", "baz", "123"))
      assert(Name.toKebabCase(input) == "foo-bar-baz-123")
    }
    "When the name has parts of an abbreviation" in {
      val name = Name.fromList(List("value", "in", "u", "s", "d"))
      assert(Name.toKebabCase(name) == "value-in-USD")
    }
  }

  "Name toHumanWords should provide a list of words from a Name" - {
    "When the name is from a camelCase string" in {
      val sut = Name.fromString("ValueInUSD")
      assert(Name.toHumanWords(sut) == List("value", "in", "USD"))
    }
  }

  "fromIterable" - {
    "Splits provided names as expected" in
      assert(Name.fromIterable(List("fooBar", "fizzBuzz")) == Name("foo", "bar", "fizz", "buzz"))
  }

  "Misc" - {
    "Name.toString" in {
      assert(Name.fromString("fooBar").toString == "[foo,bar]")
      assert(Name.fromString("a").toString == "[a]")
    }
  }

  "VariableName" - {
    "When calling VariableName.unapply" in {
      val sut          = Name.fromString("InspectorGadget")
      val variableName = Name.VariableName.unapply(sut)
      assert(variableName == Some("inspectorGadget"))
    }
    "When using as an extractor" in {
      val sut    = Name.fromString("IronMan")
      val actual = sut match
        case Name.VariableName(variableName) => variableName
        case _                               => "not a variable name"
      assert(actual == "ironMan")
    }
  }

  "String Interpolation" - {
    "When using the n interpolator on a plain string" in {
      assert(n"Foo" == Name.fromString("Foo"))
      assert(n"Foo" == Name.fromList(List("foo")))
    }
    "When using the n interpolator with an int value" in {
      val n = 42
      assert(n"Foo$n" == Name.fromString("Foo42"))
      assert(n"Foo$n" == Name.fromList(List("foo", "42")))
    }
  }

  "Operators" - {
    "+" - {
      "It should support adding a String to a Name" in
        assert(Name.fromString("Foo") + "bar" == Name.fromString("FooBar"))
    }
  }
end NameTests
