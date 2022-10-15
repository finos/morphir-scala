package org.finos.morphir.ir.types.folding
import zio.Chunk
import org.finos.morphir.ir.{FQName, Name}
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
object FoldingTypeSpec extends MorphirBaseSpec {
  def spec = suite("FoldingTypeSpec")(
    // foldContextSuite,
    sizeSuite
  )

  def foldContextSuite = suite("foldContext")(
    test("folding over Unit") {
      val sut    = Type.unit
      val actual = sut.foldContext(0)(Type.Folder.Size)
      assertTrue(actual == 1)
    }
  )

  def sizeSuite = suite("size")(
    test("size of Unit") {
      val sut    = Type.unit
      val actual = sut.size
      assertTrue(actual == 1)
    },
    test("size of Variable") {
      val sut    = Type.variable("x")
      val actual = sut.size
      assertTrue(actual == 1)
    },
    test("size of simple Reference") {
      val sut    = Type.Reference[Any]((), FQName.fromString("x"), Chunk.empty)
      val actual = sut.size
      assertTrue(actual == 1)
    },
    test("size of Reference with a single typeParam") {
      val sut    = Type.Reference[Any]((), FQName.fromString("x"), Chunk(Type.Variable[Any]((), Name.fromString("y"))))
      val actual = sut.size
      assertTrue(actual == 2)
    }
  )
}
