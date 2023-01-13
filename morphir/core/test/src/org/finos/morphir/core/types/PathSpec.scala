package org.finos.morphir.core.types

import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.core.types.Naming._
import zio.test._

object PathSpec extends MorphirBaseSpec {
  def spec = suite("Path")(
    suite("Creating a Path from a String")(
      test("It can be constructed from a simple string") {
        assertTrue(Path.fromString("Person") == Path(List(Name.fromString("person"))))
      },
      test("It can be constructed from a long string") {
        assertTrue(
          Path.fromString("She Sells Seashells") == Path(
            List(
              Name.fromList(List("she", "sells", "seashells"))
            )
          )
        )
      },
      test("It can be constructed when given a dotted string") {
        assertTrue(
          Path.fromString("blog.Author") == Path(
            List(Name.fromList(List("blog")), Name.fromList(List("author")))
          )
        )
      },
      test("It can be constructed when given a '-' separated string") {
        assertTrue(
          Path.fromString("blog-Author") == Path(
            List(Name.fromList(List("blog")), Name.fromList(List("author")))
          )
        )
      },
      test("It can be constructed when given a '/' separated string") {
        assertTrue(
          Path.fromString("blog/Author") == Path(
            List(Name.fromList(List("blog")), Name.fromList(List("author")))
          )
        )
      },
      test("It can be constructed when given a '\' separated string") {
        assertTrue(
          Path.fromString("blog\\Author") == Path(
            List(Name.fromList(List("blog")), Name.fromList(List("author")))
          )
        )
      },
      test("It can be constructed when given a ':' separated string") {
        assertTrue(
          Path.fromString("blog:Author") == Path(
            List(Name.fromList(List("blog")), Name.fromList(List("author")))
          )
        )
      },
      test("It can be constructed when given a ';' separated string") {
        assertTrue(
          Path.fromString("Blog ; Author") == Path(
            List(Name.fromList(List("blog")), Name.fromList(List("author")))
          )
        )
      },
      test("It can be constructed from Name arguments") {
        assertTrue(
          Path(Name.fromString("projectfiles"), Name.fromString("filePath")) == Path(
            List(Name.fromList(List("projectfiles")), Name.fromList(List("file", "Path")))
          )
        )
      },
      test("It can be constructed from string arguments") {
        assertTrue(
          Path("myCompany", "some Type") == Path(
            Name.unsafeMake("my", "company"),
            Name.unsafeMake("some", "type")
          )
        )
      }
    ),
    suite("Transforming a Path into a String")(
      test("Paths with period and TitleCase") {
        val input = Path(
          List(
            Name("foo", "bar"),
            Name("baz")
          )
        )
        assertTrue(Path.toString(Name.toTitleCase, ".", input) == "FooBar.Baz")
      },
      test("Paths with slash and Snake_Case") {
        val input = Path(
          List(
            Name("foo", "bar"),
            Name("baz")
          )
        )
        assertTrue(Path.toString(Name.toSnakeCase, "/", input) == "foo_bar/baz")
      }
    ),
    suite("Transforming a Path into list of Names")(
      test("It can be constructed using toList") {
        assertTrue(
          Path.toList(Path(List(Name("Com", "Example"), Name("Hello", "World"))))
            == List(Name("Com", "Example"), Name("Hello", "World"))
        )
      }
    ),
    suite("Creating a Path from a Name")(
      test("It can be constructed from names")(
        assertTrue(
          Name("Org") / Name("Finos") == Path(List(Name("Org"), Name("Finos"))),
          Name("Alpha") / Name("Beta") / Name("Gamma") == Path(List(Name("Alpha"), Name("Beta"), Name("Gamma")))
        )
      )
    ),
    suite("Checking if one Path is a prefix of another should:")(
      test("""Return true: Given path is "foo/bar" and prefix is "foo" """) {
        val sut    = Path.fromString("foo/bar")
        val prefix = Path.fromString("foo")

        assertTrue(Path.isPrefixOf(prefix = prefix, path = sut))
      },
      test("""Return false: Given path is "foo/foo" and prefix is "bar" """) {
        val sut    = Path.fromString("foo/foo")
        val prefix = Path.fromString("bar")

        assertTrue(!Path.isPrefixOf(prefix = prefix, path = sut))
      },
      test("""Return true: Given equal paths""") {
        val sut    = Path.fromString("foo/bar/baz")
        val prefix = sut
        assertTrue(Path.isPrefixOf(prefix = prefix, path = sut))
      }
    )
  )
}
