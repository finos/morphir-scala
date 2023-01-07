package org.finos.morphir
package core

import zio.test.*
import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.testing.generators.WordGen

object TextRunSpec extends MorphirBaseSpec {
  def spec = suite("TextRunSpec")(
    commonSuite,
    rawSuite,
    markedSuite
  )

  def commonSuite = suite("Common")()
  def rawSuite = suite("When TextRun is Raw")(
    test("It should behave like a Raw TextRun")(
      check(WordGen.words) { word =>
        val sut = TextRun.Raw(word)
        assertTrue(sut.isMarked == false, sut.toString == word)
      }
    )
  )
  def markedSuite = suite("When TextRun is Marked")(
    test("It should behave like a Marked TextRun")(
      check(WordGen.words) { word =>
        val sut = TextRun.Marked(word, Array.empty)
        assertTrue(sut.isMarked, sut.toString == summon[TextRun.Renderer](sut))
      }
    )
  )

}
