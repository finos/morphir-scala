package org.finos.morphir
package core

import zio.test.*
import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.testing.generators.WordGen

object TextFragmentSpec extends MorphirBaseSpec {
  def spec = suite("TextFragmentSpec")(
    commonSuite,
    rawSuite,
    markedSuite
  )

  def commonSuite = suite("Common")()
  def rawSuite = suite("When TextRun is Raw")(
    test("It should behave like a Raw TextRun")(
      check(WordGen.words) { word =>
        val sut = TextFragment.Raw(word)
        assertTrue(sut.isMarked == false, sut.toString == word)
      }
    )
  )
  def markedSuite = suite("When TextRun is Marked")(
    test("It should behave like a Marked TextRun")(
      check(WordGen.words) { word =>
        val sut = TextFragment.Marked(word, Array.empty)
        assertTrue(sut.isMarked, sut.toString == summon[TextFragment.Renderer](sut))
      }
    )
  )

}
