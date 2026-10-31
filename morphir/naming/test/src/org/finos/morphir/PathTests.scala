package org.finos.morphir

import kyo.test.*
import org.finos.morphir.naming.*

class PathTests extends Test[Any]:

  "Creating a Path from a String" - {
    "It can be constructed from a simple string" in
      assert(Path.fromString("Person") == Path(n"person"))
    "It can be constructed from a long string" in
      assert(Path.fromString("She Sells Seashells") == Path(n"she sells seashells"))
    "It can be constructed when given a dotted string" in
      assert(Path.fromString("blog.Author") == Path(n"blog", n"author"))
    "It can be constructed when given a '-' separated string" in
      assert(Path.fromString("blog-Author") == Path(n"blog", Name.fromList(List("author"))))
    "It can be constructed when given a '/' separated string" in
      assert(Path.fromString("blog/Author") == Path(Name.fromList("blog"), Name.fromList("author")))
    "It can be constructed when given a '\\' separated string" in
      assert(Path.fromString("blog\\Author") == Path(n"blog", n"author"))
    "It can be constructed when given a ':' separated string" in
      assert(Path.fromString("Morphir:SDK") == Path(n"Morphir", n"SDK"))
    "It can be constructed when given a ';' separated string" in
      assert(Path.fromString("Blog ; Author") == Path(n"blog", n"author"))
    "It can be constructed from Name arguments" in
      assert(
        Path(Name.fromString("projectfiles"), Name.fromString("filePath")) == Path.fromList(
          List(Name.fromList(List("projectfiles")), Name.fromList(List("file", "path")))
        )
      )
    "It can be constructed from string arguments" in
      assert(
        Path("myCompany", "some Type") == Path.fromList(
          Name.fromList("my", "company"),
          Name.fromList("some", "type")
        )
      )
  }

  "Transforming a Path into a String" - {
    "Paths with period and TitleCase" in {
      val input = Path(
        Name("foo", "bar"),
        Name("baz")
      )
      assert(Path.toString(Name.toTitleCase, ".", input) == "FooBar.Baz")
    }
    "Paths with slash and Snake_Case" in {
      val input = Path(
        Name("foo", "bar"),
        Name("baz")
      )
      assert(Path.toString(Name.toSnakeCase, "/", input) == "foo_bar/baz")
    }
  }

  "Transforming a Path into list of Names" - {
    "It can be constructed using toList" in
      assert(
        Path.toList(Path(Name("Com", "Example"), Name("Hello", "World"))) == List(
          Name("Com", "Example"),
          Name("Hello", "World")
        )
      )
  }

  "Creating a Path from a Name" - {
    "It can be constructed from names" in {
      assert(Path.root / Name("Org") / Name("Finos") == Path(Vector(Name("Org"), Name("Finos"))))
      assert(
        Path.root / Name("Alpha") / Name("Beta") / Name("Gamma") == Path(Vector(
          Name("Alpha"),
          Name("Beta"),
          Name("Gamma")
        ))
      )
    }
  }

  "Checking if one Path is a prefix of another should:" - {
    """Return true: Given path is "foo/bar" and prefix is "foo" """ in {
      val sut    = Path.fromString("foo/bar")
      val prefix = Path.fromString("foo")
      assert(Path.isPrefixOf(prefix = prefix, path = sut))
    }
    """Return false: Given path is "foo/foo" and prefix is "bar" """ in {
      val sut    = Path.fromString("foo/foo")
      val prefix = Path.fromString("bar")
      assert(!Path.isPrefixOf(prefix = prefix, path = sut))
    }
    """Return true: Given equal paths""" in {
      val sut    = Path.fromString("foo/bar/baz")
      val prefix = sut
      assert(Path.isPrefixOf(prefix = prefix, path = sut))
    }
  }

  "ToString" - {
    "The standard to String should return a String representation of the path using the default PathRenderer" in
      assert(Path.fromString("foo/bar/baz").toString == "Foo.Bar.Baz")
  }
end PathTests
